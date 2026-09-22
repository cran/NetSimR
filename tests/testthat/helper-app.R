#Helpers for the browser tests of the three Shiny tools (tests/testthat/test-app-*.R).
#
#The tests drive each app in headless Chrome with shinytest2. They are skipped on CRAN,
#when shinytest2 is not installed and when chromote finds no Chrome or Chromium.
#
#The app runs in a background R process from an app.R written to tempdir(). Under
#R CMD check it calls library(NetSimR), the package being checked. When the tests run
#from the source tree (devtools::test(), or pkgload::load_all() and testthat::test_file()),
#app.R loads that source tree with pkgload::load_all() instead, so the app always runs
#the code under test and not an older installed release.

#skips a browser test where it cannot or should not run
skip_if_no_app_browser <- function() {
  testthat::skip_on_cran()
  #on continuous integration (GitHub Actions sets CI) the browser tests run on one job only,
  #the one whose workflow sets NETSIMR_BROWSER_TESTS=true; locally they run wherever Chrome is found
  if (identical(tolower(Sys.getenv("CI")), "true") && !identical(tolower(Sys.getenv("NETSIMR_BROWSER_TESTS")), "true")) {
    testthat::skip("the browser tests run on one CI job only (NETSIMR_BROWSER_TESTS)")
  }
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  chrome <- tryCatch(suppressMessages(chromote::find_chrome()), error = function(e) NULL)
  found <- is.character(chrome) && length(chrome) == 1 && !is.na(chrome) && nzchar(chrome) &&
    (file.exists(chrome) || nzchar(Sys.which(chrome)))
  if (!found) testthat::skip("chromote found no Chrome or Chromium for the browser tests")
  if (identical(app_test_load_mode(), "missing pkgload")) {
    testthat::skip("pkgload is needed to run the apps from the source tree")
  }
}

#the package source tree the tests run from, or NULL under R CMD check (or when the tests
#are not in a NetSimR source tree)
app_test_source_root <- function() {
  if (testthat::is_checking()) return(NULL)
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  description <- file.path(root, "DESCRIPTION")
  if (!file.exists(description) || !dir.exists(file.path(root, "R"))) return(NULL)
  package <- tryCatch(unname(read.dcf(description, fields = "Package")[1, 1]), error = function(e) NA_character_)
  if (!identical(package, "NetSimR")) return(NULL)
  root
}

#how app.R loads NetSimR: "installed", "source" or "missing pkgload"
app_test_load_mode <- function() {
  if (is.null(app_test_source_root())) return("installed")
  if (requireNamespace("pkgload", quietly = TRUE)) "source" else "missing pkgload"
}

#writes the app directory of one tool and returns its path
app_test_dir <- function(tool) {
  run <- c(
    simulator = "run_shiny_simulator",
    distribution = "run_shiny_distribution_fitting_tool",
    glm = "run_shiny_glm_fitting_tool"
  )[[tool]]
  root <- app_test_source_root()
  load <- if (is.null(root)) {
    "library(NetSimR)"
  } else {
    paste0("pkgload::load_all(", deparse(root), ", export_all = FALSE, helpers = FALSE, quiet = TRUE)")
  }
  dir <- file.path(tempdir(), paste0("netsimr-app-", tool))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(c("# written by NetSimR's tests/testthat/helper-app.R", load, paste0(run, "()")),
             file.path(dir, "app.R"))
  dir
}

#starts one of the tools ("simulator", "distribution" or "glm") in headless Chrome; the caller
#stops it with on.exit(app$stop(), add = TRUE)
start_netsimr_app <- function(tool, name, width = 1300, height = 900) {
  shinytest2::AppDriver$new(
    app_test_dir(tool),
    name = name, width = width, height = height, seed = 1,
    #starting R and the package can be slow on a busy CI runner
    load_timeout = 120 * 1000, timeout = 30 * 1000,
    options = list(shiny.fullstacktrace = TRUE)
  )
}

#waits until condition() is TRUE, polling; stops with a message on timeout
app_wait_for <- function(condition, what, timeout = 30, interval = 0.2) {
  deadline <- Sys.time() + timeout
  repeat {
    if (isTRUE(tryCatch(condition(), error = function(e) FALSE))) return(invisible(TRUE))
    if (Sys.time() > deadline) stop("Timed out after ", timeout, " seconds waiting for ", what, call. = FALSE)
    Sys.sleep(interval)
  }
}

app_idle <- function(app, duration = 300) {
  app$wait_for_idle(duration = duration, timeout = 60 * 1000)
}

#same value, with a tolerance for numbers (the browser sends them as JSON)
app_same_value <- function(actual, expected) {
  if (is.null(actual) || is.null(expected)) return(is.null(actual) && is.null(expected))
  if (is.numeric(actual) && is.numeric(expected)) {
    return(length(actual) == length(expected) &&
             isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-12)))
  }
  identical(as.vector(actual), as.vector(expected))
}

app_inputs <- function(app, ids) {
  values <- app$get_values(input = TRUE)$input
  stats::setNames(lapply(ids, function(id) values[[id]]), ids)
}

#waits until the server has every input at the expected value (a named list)
app_wait_for_inputs <- function(app, expected, what = "the inputs to be set", timeout = 30) {
  app_wait_for(function() {
    current <- app_inputs(app, names(expected))
    all(mapply(app_same_value, current, expected))
  }, what, timeout = timeout)
}

#sets inputs and waits until the server has them, instead of waiting for an output to change
#(many of these inputs change no output)
app_set <- function(app, ...) {
  values <- list(...)
  app$set_inputs(..., wait_ = FALSE)
  app_wait_for_inputs(app, values, paste("the inputs", paste(names(values), collapse = ", ")))
  app_idle(app)
}

#clicks an element by id or CSS selector without waiting for an output
app_click <- function(app, id = NULL, selector = paste0("#", id)) {
  app$run_js(sprintf("document.querySelector(%s).click();", app_js_string(selector)))
}

app_js_string <- function(x) {
  paste0("'", gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", x)), "'")
}

#shows a navbar tab by its value and waits until it is shown
app_goto <- function(app, tab) {
  link <- sprintf(".navbar a[data-value='%s']", tab)
  app_click(app, selector = link)
  app$wait_for_js(sprintf("document.querySelector(%s).classList.contains('active')", app_js_string(link)),
                  timeout = 30 * 1000)
  app_idle(app)
}

#switches the light/dark theme with the navbar buttons
app_theme <- function(app, mode) {
  app_click(app, selector = sprintf(".theme-btn[data-theme-value='%s']", mode))
  app$wait_for_js(sprintf("document.documentElement.getAttribute('data-bs-theme') === '%s'", mode),
                  timeout = 30 * 1000)
  app_idle(app)
}

#the visible text of the first element matching a selector, with runs of white space
#collapsed ("" when there is none)
app_text <- function(app, selector) {
  text <- app$get_js(sprintf(
    "(function () { var e = document.querySelector(%s); return e ? e.innerText : ''; })()",
    app_js_string(selector)
  ))
  trimws(gsub("\\s+", " ", paste(unlist(text), collapse = " ")))
}

#the texts of the notifications on screen
app_notifications <- function(app) {
  text <- app$get_js(paste0(
    "Array.from(document.querySelectorAll('#shiny-notification-panel .shiny-notification'))",
    ".map(function (e) { return e.innerText.replace(/\\s+/g, ' ').trim(); })"
  ))
  as.character(unlist(text))
}

app_clear_notifications <- function(app) {
  app$run_js("document.querySelectorAll('#shiny-notification-panel .shiny-notification-close').forEach(function (e) { e.click(); });")
  app$wait_for_js("document.querySelectorAll('#shiny-notification-panel .shiny-notification').length === 0",
                  timeout = 30 * 1000)
}

#waits for a notification matching a pattern and returns its text
app_wait_for_notification <- function(app, pattern, timeout = 30) {
  found <- character(0)
  app_wait_for(function() {
    found <<- grep(pattern, app_notifications(app), value = TRUE)
    length(found) > 0
  }, paste0("a notification matching '", pattern, "'"), timeout = timeout)
  found
}

#uploads a file to a file input and waits until the server has it
app_upload <- function(app, id, path) {
  args <- stats::setNames(list(path), id)
  do.call(app$upload_file, c(args, list(wait_ = FALSE)))
  app_wait_for(function() {
    value <- app$get_values(input = id)$input[[id]]
    is.data.frame(value) && identical(value$name[1], basename(path))
  }, paste("the upload of", basename(path)))
  app_idle(app)
}

#downloads an output once its link is ready (Shiny sets the href when it binds the output,
#which it does only while the output is visible); filename NULL keeps the server's file name
app_download <- function(app, id, filename = NULL) {
  app$wait_for_js(sprintf("(function () { var e = document.getElementById('%s'); return !!e && !!e.getAttribute('href'); })()", id),
                  timeout = 60 * 1000)
  app$get_download(id, filename = filename)
}

#R errors of the app process and errors in the browser console, from the logs
app_log_errors <- function(app) {
  logs <- as.data.frame(app$get_logs())
  r_errors <- logs$location == "shiny" & grepl("Error", logs$message)
  js_errors <- logs$location == "chromote" & logs$level %in% c("error", "throw")
  paste0("[", logs$location, " ", logs$level, "] ", logs$message)[r_errors | js_errors]
}

expect_app_logs_clean <- function(app) {
  errors <- app_log_errors(app)
  testthat::expect(length(errors) == 0,
                   paste0("The app logged errors:\n", paste(errors, collapse = "\n")))
  invisible(errors)
}

#a plot output's image, drawn onto a canvas in the browser to count its pixels by alpha:
#transparent (0), opaque (255) and semi-transparent (anything between)
app_plot_image <- function(app, id) {
  app$get_js(sprintf("(async function () {
    var img = document.querySelector('#%s img');
    if (!img || !img.src) return {found: false};
    if (img.decode) { try { await img.decode(); } catch (e) {} }
    var canvas = document.createElement('canvas');
    canvas.width = img.naturalWidth;
    canvas.height = img.naturalHeight;
    var context = canvas.getContext('2d');
    context.drawImage(img, 0, 0);
    var data = context.getImageData(0, 0, canvas.width, canvas.height).data;
    var clear = 0, partial = 0, opaque = 0;
    for (var i = 3; i < data.length; i += 4) {
      if (data[i] === 0) clear++; else if (data[i] === 255) opaque++; else partial++;
    }
    return {found: true, src: img.src, width: img.naturalWidth, height: img.naturalHeight,
            clientWidth: img.clientWidth, pixelRatio: window.devicePixelRatio || 1,
            clear: clear, partial: partial, opaque: opaque};
  })()", id))
}

#the src of a plot output's image ("" when there is none)
app_plot_src <- function(app, id) {
  src <- app$get_js(sprintf("(function () { var img = document.querySelector('#%s img'); return img && img.src ? img.src : ''; })()", id))
  as.character(unlist(src))
}

#the bytes of a PNG in a data URI
app_png_bytes <- function(src) {
  testthat::expect_match(src, "^data:image/png;base64,")
  base64enc::base64decode(sub("^data:image/png;base64,", "", src))
}

app_png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))

#the width in the IHDR chunk of a PNG
app_png_width <- function(bytes) sum(as.integer(bytes[17:20]) * 256^(3:0))

#whether the page scrolls sideways: the document is wider than the visible part of the window
app_horizontal_scroll <- function(app) {
  app$get_js("({scrollWidth: document.documentElement.scrollWidth, clientWidth: document.documentElement.clientWidth, innerWidth: window.innerWidth})")
}

expect_no_horizontal_scroll <- function(app, where) {
  size <- app_horizontal_scroll(app)
  testthat::expect(
    size$scrollWidth <= size$clientWidth && size$scrollWidth <= size$innerWidth,
    sprintf("%s scrolls sideways: the page is %s px wide in a window of %s px (%s px without the scroll bar)",
            where, size$scrollWidth, size$innerWidth, size$clientWidth)
  )
}

#all the bytes of a file, decompressed when it is gzip-compressed (as saveRDS() writes it)
app_file_bytes <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con), add = TRUE)
  chunks <- list()
  repeat {
    chunk <- readBin(con, "raw", n = 1e6)
    if (length(chunk) == 0) break
    chunks[[length(chunks) + 1]] <- chunk
  }
  unlist(chunks)
}

app_bytes_contain <- function(bytes, text) {
  length(grepRaw(charToRaw(text), bytes, fixed = TRUE)) > 0
}

#a claims CSV for the fitting tools: overdispersed claim counts (the Negative Binomial fits
#much better than the Poisson) with an exposure, and a few rating factors. It is the same in
#every session, and the caller's random numbers are left as they were.
app_claims_csv <- function() {
  file <- file.path(tempdir(), "netsimr-app-claims.csv")
  if (file.exists(file)) return(file)
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else
    rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(20260915, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  n <- 2000
  age <- sample(18:80, n, replace = TRUE)
  region <- sample(c("North", "South", "East", "West"), n, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2))
  exposure <- round(stats::runif(n, 0.1, 1), 3)
  weight <- sample(1:5, n, replace = TRUE)
  effect <- c(North = 0, South = 0.25, East = -0.2, West = 0.1)[region]
  claim_count <- stats::rnbinom(n, size = 1.5, mu = exposure * exp(-0.6 + 0.012 * (age - 45) + effect))
  data <- data.frame(age, region, exposure, weight, claim_count, stringsAsFactors = FALSE)
  utils::write.csv(data, file, row.names = FALSE)
  file
}
