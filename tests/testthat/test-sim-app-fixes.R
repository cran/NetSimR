#regression tests for the claims simulator app: charts, undefined totals, number formats,
#remembered inputs, loaded settings, Pareto slices, report edge cases and documented figures

#the text of a report without its embedded images
sim_report_words <- function(settings, results) {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  write_simulation_report(file, settings, results)
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  gsub("data:image/png;base64,[A-Za-z0-9+/=]+", "", html)
}

compare_entry_of <- function(id, settings, data = do.call(simulate_function, settings)) {
  sim_tab_compare_entry(list(id = id, settings = settings, data = data, finished = Sys.time()))
}

#the html of a renderUI output read in testServer()
ui_html <- function(x) paste(as.character(x$html), collapse = "")

#inputs that the simulator server needs before its dynamic fields render
set_page_inputs <- function(session, ...) {
  inputs <- list(
    freqDistr = "Poisson", sevDistr = "LogNormal",
    seedSetBinary = FALSE, sevCapBinary = FALSE, sevTruncateAtZero = FALSE,
    pareto_slice_times = 0, multiprocessingBinary = FALSE,
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsuranceStructureAL = "No Reinsurance Structure"
  )
  do.call(session$setInputs, utils::modifyList(inputs, list(...)))
}

# ---------------------------------------------------------------- compare chart

test_that("the compare chart is drawn by netsimr_render_plot and keeps the 1 in 200 label inside", {
  expect_true(any(grepl("netsimr_render_plot", deparse(body(sim_compare_tab_server)), fixed = TRUE)))

  #2,500 simulations: the chart stops at 1 in 250, just right of the 1 in 200 line
  entries <- lapply(1:2, function(id) compare_entry_of(id, base_settings(numOfSimulations = 2500, seedValue = id)))
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  for (width in c(400, 1200)) {
    grDevices::png(file, width = width, height = 400)
    extent <- sim_tab_return_period_plot(entries, c("Run 1", "Run 2"), dark = TRUE)
    grDevices::dev.off()
    expect_length(extent$marker, 2)
    expect_gte(extent$marker[1], extent$plot[1])
    #the label was drawn right of the line even where it ran past the edge of the plot
    expect_lte(extent$marker[2], extent$plot[2])
  }

  #no 1 in 200 line below 2,000 simulations
  small <- lapply(1:2, function(id) compare_entry_of(id, base_settings(numOfSimulations = 500, seedValue = id)))
  grDevices::png(file, width = 400, height = 400)
  extent <- sim_tab_return_period_plot(small, c("Run 1", "Run 2"))
  grDevices::dev.off()
  expect_null(extent$marker)
})

# ---------------------------------------------------------------- undefined totals

test_that("the Normal sum shortcut gives the totals the claims add up to instead of NaN", {
  #n * mean overflows: rnorm(mean = Inf, sd = Inf) gave NaN for about 2,450 of these 3,000
  #simulations, where summing the claims gives about 400 (claims of +Inf and -Inf, which are
  #truly undefined); the shortcut now agrees with summing the claims
  tally <- function(shortcuts) {
    x <- simulate_function(numOfSimulations = 3000, freq_params = c(10, 0.5), sev_params = c(1e308, 1e308),
                           seedValue = 1, freqDistr = "Binomial", sevDistr = "Normal", shortcuts = shortcuts)$total_claims
    c(nan = sum(is.nan(x)), inf = sum(x == Inf, na.rm = TRUE))
  }
  shortcut <- tally(TRUE)
  every_claim <- tally(FALSE)
  expect_lt(shortcut[["nan"]], 600)
  expect_lt(abs(shortcut[["nan"]] - every_claim[["nan"]]), 150)
  expect_lt(abs(shortcut[["inf"]] - every_claim[["inf"]]), 150)

  sum_func <- sev_dist_options$Normal@sum_func
  #within the double range the shortcut is the same single draw as before
  set.seed(1)
  shortcut <- sum_func(c(0, 1, 5), c(10, 2))
  set.seed(1)
  expect_identical(shortcut, stats::rnorm(3, mean = c(0, 10, 50), sd = sqrt(c(0, 1, 5)) * 2))
  #where it overflows, the claims are summed one by one
  expect_equal(sum_func(c(0, 2, 10), c(1e308, 1)), c(0, Inf, Inf))
  expect_equal(sum_func(c(3, 0), c(-1e308, 1)), c(-Inf, 0))
})

test_that("undefined (NaN) totals are counted and reported, not dropped silently", {
  #infinite claims through an excluded layer with an infinite limit leave Inf - Inf = NaN
  settings <- base_settings(numOfSimulations = 1000, seedValue = 1, sevDistr = "Pareto", sev_params = c(0.002, 100),
                            reinsuranceStructureEEL = "Exclude Layer", reinsurance_structure_eel_dedctible_amount = 1000,
                            reinsurance_structure_eel_limit_amount = Inf)
  results <- do.call(simulate_function, settings)
  n_nan <- sum(is.nan(results$total_claims))
  expect_gt(n_nan, 0)
  expect_lt(n_nan, 1000)

  sm <- summarise_simulation(settings, results)
  expect_equal(sm$undefined, n_nan)
  expect_equal(sm$n, 1000 - n_nan)
  expect_equal(length(sm$totals), 1000 - n_nan)
  #the claim counts of every simulation are defined, so the frequency uses them all
  expect_equal(sm$frequency$mean, mean(results$claim_counts))

  words <- sim_report_words(settings, results)
  fmt <- function(x) formatC(x, format = "d", big.mark = ",")
  expect_match(words, paste(fmt(n_nan), "of 1,000 simulations had undefined (NaN) totals"), fixed = TRUE)
  expect_match(words, paste(fmt(n_nan), "simulations had undefined (NaN) totals and are left out of the charts"), fixed = TRUE)
  expect_match(words, "Simulations with a defined total", fixed = TRUE)
  expect_match(words, paste(fmt(1000 - n_nan), "of 1,000 simulations"), fixed = TRUE)
  expect_false(grepl("NaN<", words, fixed = TRUE))

  entry <- compare_entry_of(1, settings, results)
  expect_equal(entry$metrics$undefined, n_nan)
  expect_equal(entry$metrics$n, 1000 - n_nan)
  table <- as.character(sim_tab_metrics_table(list(entry), "Run 1"))
  expect_match(table, "Undefined (NaN) totals", fixed = TRUE)
  #runs without undefined totals do not get the row
  clean <- compare_entry_of(2, base_settings(numOfSimulations = 200))
  expect_equal(clean$metrics$undefined, 0)
  expect_false(grepl("Undefined", as.character(sim_tab_metrics_table(list(clean), "Run 2")), fixed = TRUE))

  #every total undefined is an error that says so
  expect_error(summarise_simulation(settings, c(NaN, NaN)), "undefined")
})

test_that("totals of both +Inf and -Inf with a structure give a report and a compare entry", {
  #the gross mean of +Inf and -Inf is NaN, which made the share of the gross mean fail
  exclude <- base_settings(reinsuranceStructureAL = "Exclude Layer", reinsurance_structure_al_dedctible_amount = 1e300,
                           reinsurance_structure_al_limit_amount = 1e308)
  totals <- c(1:200, rep(NaN, 100), rep(Inf, 100), rep(-Inf, 100))
  results <- data.frame(claim_counts = 1L, total_claims = totals, gross_claims = totals)
  sm <- summarise_simulation(exclude, results)
  expect_true(is.na(sm$gross$table$Gross[2]))
  expect_true(is.nan(sm$stats$mean))
  expect_match(sim_report_words(exclude, results), "Gross, ceded and net", fixed = TRUE)
  expect_true(is.nan(compare_entry_of(1, exclude, results)$metrics$mean))
  #every other mix of finite, infinite and undefined totals, with and without the structure
  for (settings in list(base_settings(), exclude)) {
    for (mix in list(c(1:200, rep(-Inf, 100)), c(1:200, rep(Inf, 100), rep(-Inf, 100)),
                     c(rep(Inf, 100), rep(-Inf, 100)), c(1:200, rep(NaN, 100), rep(Inf, 100)))) {
      data <- data.frame(claim_counts = 1L, total_claims = mix, gross_claims = mix)
      expect_no_error(sim_report_words(settings, data))
      expect_no_error(compare_entry_of(1, settings, data))
    }
  }

  #the app's run: Binomial(10, 0.5) claims of Normal(1e308, 1e308) through an aggregate exclusion
  app <- base_settings(numOfSimulations = 600, seedValue = 1, freqDistr = "Binomial", freq_params = c(10, 0.5),
                       sevDistr = "Normal", sev_params = c(1e308, 1e308), reinsuranceStructureAL = "Exclude Layer",
                       reinsurance_structure_al_dedctible_amount = 1e300, reinsurance_structure_al_limit_amount = 1e308)
  data <- do.call(simulate_function, app)
  expect_true(any(data$total_claims == Inf, na.rm = TRUE) && any(data$total_claims == -Inf, na.rm = TRUE))
  expect_true(anyNA(data$total_claims))
  words <- sim_report_words(app, data)
  expect_match(words, "the claims overflowed to both +Inf and -Inf, whose sum is undefined", fixed = TRUE)
  expect_match(words, "or undefined (shown as a dash) where they add +Inf to -Inf, as the mean does", fixed = TRUE)
  expect_no_error(compare_entry_of(2, app, data))
})

test_that("the report explains undefined totals by their cause", {
  plain <- base_settings()
  totals <- c(1:200, NaN, NaN, NaN)
  #claims of +Inf and -Inf in one simulation: its gross total is undefined too
  claims_words <- sim_report_words(plain, data.frame(claim_counts = 1L, total_claims = totals, gross_claims = totals))
  expect_match(claims_words, paste("3 of 203 simulations had undefined (NaN) totals: the claims overflowed to both",
                                   "+Inf and -Inf, whose sum is undefined. They are left out"), fixed = TRUE)
  expect_false(grepl("infinite limit", claims_words, fixed = TRUE))
  #a run without layers gives only the totals, which are its gross totals
  expect_match(sim_report_words(plain, totals), "NaN) totals: the claims overflowed to both +Inf and -Inf", fixed = TRUE)

  #a structure turned infinite gross totals into infinity minus infinity
  exclude <- base_settings(reinsuranceStructureEEL = "Exclude Layer", reinsurance_structure_eel_dedctible_amount = 1000,
                           reinsurance_structure_eel_limit_amount = Inf)
  structure_words <- sim_report_words(exclude, data.frame(claim_counts = 1L, total_claims = totals,
                                                          gross_claims = c(1:200, Inf, Inf, Inf)))
  expect_match(structure_words, paste("3 of 203 simulations had undefined (NaN) totals: an infinite amount met a layer",
                                      "with an infinite deductible or an excluded layer with an infinite limit"), fixed = TRUE)
  expect_false(grepl("overflowed", structure_words, fixed = TRUE))
  #both causes, counted apart
  both <- sim_report_words(exclude, data.frame(claim_counts = 1L, total_claims = totals, gross_claims = c(1:200, NaN, Inf, Inf)))
  expect_match(both, paste("In 1 of them the claims overflowed to both +Inf and -Inf, whose sum is undefined; in 2",
                           "an infinite amount met a layer"), fixed = TRUE)
  #without the gross totals the cause for a run with layers is not known, so both are given
  expect_match(sim_report_words(exclude, totals), "for example because claims overflowed to both +Inf and -Inf", fixed = TRUE)

  #one undefined total is counted in the singular
  one <- sim_report_words(plain, c(1:200, NaN))
  expect_match(one, "1 of 201 simulations had an undefined (NaN) total: the claims overflowed", fixed = TRUE)
  expect_match(one, "It is left out of every figure and chart in this report, which describe the other 200 simulations.",
               fixed = TRUE)
  expect_match(one, "1 simulation had an undefined (NaN) total and is left out of the charts.", fixed = TRUE)
})

test_that("the report counts one simulation in the singular", {
  one <- sim_report_words(base_settings(numOfSimulations = 1), 250)
  expect_false(grepl("\\b1 simulations\\b", one))
  expect_match(one, "<div class=\"kpi-note\">1 simulation</div>", fixed = TRUE)
  expect_match(sim_report_words(base_settings(), c(1:200, NaN)), "<div class=\"kpi-note\">200 of 201 simulations</div>",
               fixed = TRUE)
  #one infinite total
  infinite <- sim_report_words(base_settings(), c(1:99, Inf))
  expect_match(infinite, "1 of 100 simulations had an infinite total, so the figures that include it are infinite.",
               fixed = TRUE)
  expect_match(infinite, "1 simulation (1.0%) had an infinite total and is left out of the charts.", fixed = TRUE)
  expect_false(grepl("\\b1 simulations\\b", infinite))
})

test_that("the compare tab shows infinite values as Inf or -Inf and undefined ones as a dash", {
  dash <- intToUtf8(8212)
  expect_identical(sim_tab_fmt_amount(Inf), "Inf")
  expect_identical(sim_tab_fmt_amount(-Inf, 2), "-Inf")
  expect_identical(sim_tab_fmt_amount(NaN), dash)
  expect_identical(sim_tab_fmt_amount(NA_real_), dash)
  expect_identical(sim_tab_fmt_int(Inf), "Inf")
  expect_identical(sim_tab_fmt_int(NaN), dash)
  expect_identical(sim_tab_fmt_pct(Inf), "Inf")
  expect_identical(sim_tab_fmt_pct(-Inf, 2), "-Inf")
  expect_identical(sim_tab_fmt_pct(NaN), dash)

  #+Inf and -Inf totals: the mean is undefined, the tail infinite
  mixed <- compare_entry_of(1, base_settings(), data.frame(claim_counts = 1L, total_claims = c(1:200, rep(Inf, 30), rep(-Inf, 10))))
  #-Inf totals only: the mean and the median are -Inf
  negative <- compare_entry_of(2, base_settings(), data.frame(claim_counts = 1L, total_claims = c(rep(-Inf, 150), 1:100)))
  html <- as.character(sim_tab_metrics_table(list(mixed, negative), c("Mixed", "Negative")))
  #the cells of one row of the table, one per run
  row <- function(label) {
    part <- regmatches(html, regexpr(paste0("<td>", label, "</td>(\\s*<td>[^<]*</td>){2}"), html))
    gsub("^<td>|</td>$", "", regmatches(part, gregexpr("<td>[^<]*</td>", part))[[1]][-1])
  }
  expect_true(is.finite(mixed$metrics$median) && is.finite(negative$metrics$var995))
  expect_identical(row("Mean"), c(dash, "-Inf"))
  expect_identical(row("Median"), c(sim_tab_fmt_amount(mixed$metrics$median, mixed$digits), "-Inf"))
  expect_identical(row("VaR 99.5%"), c("Inf", sim_tab_fmt_amount(negative$metrics$var995, negative$digits)))
  expect_identical(row("TVaR 99.5%"), c("Inf", sim_tab_fmt_amount(negative$metrics$tvar995, negative$digits)))
  expect_false(grepl("NaN", html, fixed = TRUE))
  #a note says what Inf and the dash mean; runs without infinite totals do not get it
  expect_match(html, "Inf and -Inf are infinite values. A dash is a value that is undefined", fixed = TRUE)
  clean <- compare_entry_of(3, base_settings(numOfSimulations = 200))
  expect_false(grepl("infinite values", as.character(sim_tab_metrics_table(list(clean), "Clean")), fixed = TRUE))
})

# ---------------------------------------------------------------- number formats

test_that("very large settings are described in scientific format on the compare tab", {
  s <- list(freqDistr = "Poisson", freq_params = 1, sevDistr = "Normal", sev_params = c(1e308, 1e308),
            reinsuranceStructureEEL = "No Reinsurance Structure", reinsuranceStructureAL = "Exclude Layer",
            reinsurance_structure_al_dedctible_amount = 1e300, reinsurance_structure_al_limit_amount = 1e308)
  description <- sim_tab_describe_settings(s)
  expect_lt(nchar(description), 200)
  expect_match(description, "Mean = 1.000e+308, Standard deviation = 1.000e+308", fixed = TRUE)
  expect_match(description, "AL Exclude Layer 1.000e+308 xs 1.000e+300", fixed = TRUE)
  #ordinary values keep their fixed format
  ordinary <- sim_tab_describe_settings(layered_settings(reinsurance_structure_eel_limit_amount = 200000))
  expect_match(ordinary, "Log-Normal (mu = 6, sigma = 1.5)", fixed = TRUE)
  expect_match(ordinary, "Limited Layer 200,000 xs 1,000", fixed = TRUE)

  expect_identical(sim_tab_fmt_pct(1e20), "1.000e+22%")
  expect_identical(sim_tab_fmt_pct(0.25), "25.0%")
  expect_identical(sim_tab_fmt_amount(1e308), "1.000e+308")
})

test_that("very large implied moments are shown in scientific format", {
  shiny::testServer(shiny_simulator_server, {
    set_page_inputs(session, sevDistr = "Normal")
    session$setInputs(lamda = 3, normal_mean = 1e308, normal_sd = 1)
    html <- ui_html(output$sev_implied_moments)
    expect_match(html, "1.000e+308", fixed = TRUE)
    expect_lt(nchar(html), 400)
  })
})

# ---------------------------------------------------------------- remembered inputs

test_that("typed parameters and layer amounts survive switching options away and back", {
  shiny::testServer(shiny_simulator_server, {
    set_page_inputs(session, reinsuranceStructureEEL = "Limited Layer")
    session$setInputs(lamda = 4, mu = 7.5, sigma = 1.25, reinsurance_structure_eel_dedctible_amount = 5000,
                      reinsurance_structure_eel_limit_amount = 20000)

    #Poisson -> Binomial -> Poisson keeps lambda; the Binomial's n field starts empty
    session$setInputs(freqDistr = "Binomial")
    expect_false(grepl('value="4"', ui_html(output$freq_param_1), fixed = TRUE))
    session$setInputs(n = 10)
    session$setInputs(freqDistr = "Poisson")
    expect_match(ui_html(output$freq_param_1), 'id="lamda"[^>]*value="4"')
    #and back again keeps the Binomial's n too
    session$setInputs(freqDistr = "Binomial")
    expect_match(ui_html(output$freq_param_1), 'id="n"[^>]*value="10"')

    #Log-Normal -> Gamma -> Log-Normal keeps mu and sigma
    session$setInputs(sevDistr = "Gamma")
    session$setInputs(sevDistr = "LogNormal")
    expect_match(ui_html(output$sev_param_1), 'id="mu"[^>]*value="7.5"')
    expect_match(ui_html(output$sev_param_2), 'id="sigma"[^>]*value="1.25"')
    #the Normal has its own ids, so the Log-Normal's values do not carry over
    session$setInputs(sevDistr = "Normal")
    expect_match(ui_html(output$sev_param_1), 'id="normal_mean"')
    expect_false(grepl('value="7.5"', ui_html(output$sev_param_1), fixed = TRUE))

    #a limited and an unlimited layer both have a deductible: switching keeps it
    session$setInputs(reinsuranceStructureEEL = "Unlimited Layer")
    expect_match(ui_html(output$reinsuranceStructureDeductibleEEL), 'value="5000"', fixed = TRUE)
    session$setInputs(reinsuranceStructureEEL = "Limited Layer")
    expect_match(ui_html(output$reinsuranceStructureLimitEEL), 'value="20000"', fixed = TRUE)
  })
})

test_that("switching the custom seed off and on keeps its value", {
  shiny::testServer(shiny_simulator_server, {
    set_page_inputs(session, seedSetBinary = TRUE)
    #a first seed field starts at 1
    expect_match(ui_html(output$seed_value), 'value="1"', fixed = TRUE)
    session$setInputs(seedValue = 77)
    session$setInputs(seedSetBinary = FALSE)
    session$setInputs(seedSetBinary = TRUE)
    expect_match(ui_html(output$seed_value), 'value="77"', fixed = TRUE)
  })
})

test_that("values typed just after loading an example are kept, not set back to the example's", {
  #the loader used to count a value as loaded only once it had held for half a second, and
  #took any change before then for a field reset by the browser: it sent the example's value
  #again over what the user had typed (seen in the browser tests on slow CI machines)
  sent <- list()
  local_mocked_bindings(
    sim_settings_apply_update = function(session, id, kind, value) sent[[length(sent) + 1]] <<- list(id = id, value = value),
    sim_settings_notify = function(message, type = "message") NULL
  )
  shiny::testServer(shiny_simulator_server, {
    set_page_inputs(session)
    session$setInputs(settingsIO_example = "Motor: excess of loss layer", settingsIO_load_example = 1)
    #the browser applies the example's choices, the server builds the fields and the browser
    #reports the example's values
    set_page_inputs(session, seedSetBinary = TRUE, reinsuranceStructureEEL = "Limited Layer", numberOfSimulations = 50000)
    session$elapse(300)
    session$setInputs(lamda = 5, mu = 9, sigma = 1.3, seedValue = 1, reinsurance_structure_eel_dedctible_amount = 50000,
                      reinsurance_structure_eel_limit_amount = 200000, reinsuranceStructureLimitedReinstatements = TRUE)
    session$setInputs(reinsuranceStructureReinstatementLimit = 2)
    #the user types new values at once
    before_typing <- length(sent)
    session$setInputs(numberOfSimulations = 12345, lamda = 6.5, seedValue = 2024)
    Sys.sleep(0.6)
    for (i in 1:8) session$elapse(300)
    expect_identical(sent[-seq_len(before_typing)], list())
    expect_equal(input$lamda, 6.5)
    expect_equal(input$seedValue, 2024)
  })
})

test_that("fields built after loading settings start from the loaded values, even after the load gave up", {
  #a slow browser: the example's choices reach the server only after the loader has stopped
  #waiting for the fields (here at once); the fields are built from the loaded values anyway
  old <- options(netsimr.settings_io_timeout = -1)
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(sim_settings_notify = function(message, type = "message") NULL)
  shiny::testServer(shiny_simulator_server, {
    set_page_inputs(session)
    session$setInputs(lamda = 3)
    session$setInputs(freqDistr = "Binomial")
    session$setInputs(settingsIO_example = "Motor: excess of loss layer", settingsIO_load_example = 1)
    session$elapse(300)
    set_page_inputs(session, seedSetBinary = TRUE, reinsuranceStructureEEL = "Limited Layer")
    expect_match(ui_html(output$freq_param_1), 'id="lamda"[^>]*value="5"')
    expect_match(ui_html(output$sev_param_1), 'id="mu"[^>]*value="9"')
    expect_match(ui_html(output$seed_value), 'value="1"', fixed = TRUE)
    expect_match(ui_html(output$reinsuranceStructureDeductibleEEL), 'value="50000"', fixed = TRUE)
    expect_match(ui_html(output$reinsuranceStructureLimitEEL), 'value="2e\\+05"|value="200000"')
    expect_match(ui_html(output$reinsuranceStructureLimitedReinstatements_ui), "checked", fixed = TRUE)
    session$setInputs(reinsuranceStructureLimitedReinstatements = TRUE)
    expect_match(ui_html(output$reinsuranceStructureReinstatementLimit_ui), 'value="2"', fixed = TRUE)
  })
})

# ---------------------------------------------------------------- Pareto slices

test_that("quick Add slice clicks each add a slice", {
  shiny::testServer(shiny_simulator_server, {
    sent <- function() get("slice_count_sent", envir = environment(set_slice_count))
    set_page_inputs(session)
    #two clicks before the browser has reported the first new count
    session$setInputs(add_pareto_slice = 1)
    expect_equal(sent(), 1)
    session$setInputs(add_pareto_slice = 2)
    expect_equal(sent(), 2)
    #the browser reports the count back; later clicks build on the input again
    session$setInputs(pareto_slice_times = 2)
    expect_null(sent())
    #two clicks that reach the server together: the button's value jumps by two
    session$setInputs(add_pareto_slice = 4)
    expect_equal(sent(), 4)
    #never beyond the maximum
    session$setInputs(pareto_slice_times = 4, add_pareto_slice = 10)
    expect_equal(sent(), max_number_of_pareto_slices)
    session$setInputs(pareto_slice_times = max_number_of_pareto_slices)
    expect_null(sent())
    #quick removals also build on the count just sent
    session$setInputs(remove_pareto_slice_1 = 1)
    expect_equal(sent(), max_number_of_pareto_slices - 1)
    session$setInputs(remove_pareto_slice_1 = 2)
    expect_equal(sent(), max_number_of_pareto_slices - 2)
  })
})

test_that("loaded settings replace the number of slices a click sent just before", {
  #the count sent by a click was kept until the browser echoed exactly it, so a click after
  #loading settings built on it (Add, Add, load an example without slices, Add gave 3)
  local_mocked_bindings(sim_settings_notify = function(message, type = "message") NULL)
  shiny::testServer(shiny_simulator_server, {
    sent <- function() get("slice_count_sent", envir = environment(set_slice_count))
    set_page_inputs(session)
    session$setInputs(add_pareto_slice = 1)
    session$setInputs(pareto_slice_times = 1)
    expect_null(sent())
    #a second click that the browser has not reported back when the example loads
    session$setInputs(add_pareto_slice = 2)
    expect_equal(sent(), 2)
    session$setInputs(settingsIO_example = "Motor: excess of loss layer", settingsIO_load_example = 1)
    expect_equal(sent(), 0)
    session$setInputs(pareto_slice_times = 0)
    expect_null(sent())
    session$setInputs(add_pareto_slice = 3)
    expect_equal(sent(), 1)
    #an example with slices sends its number the same way, so a click at once builds on it
    session$setInputs(settingsIO_example = "Property: aggregate cover", settingsIO_load_example = 2)
    expect_equal(sent(), 1)
    session$setInputs(add_pareto_slice = 4)
    expect_equal(sent(), 2)
  })
})

# ---------------------------------------------------------------- layout

test_that("the layout keeps the brand, the example names and the compare labels readable", {
  #the brand keeps a gap before the nav links, and where room is short it shrinks with an
  #ellipsis rather than pushing the menu button off a narrow screen
  expect_match(sim_ui_css, "\\.sim-brand \\{[^}]*margin-right: 2rem;")
  expect_match(sim_ui_css, "\\.sim-brand-text \\{[^}]*white-space: nowrap;[^}]*min-width: 0;")
  expect_match(sim_ui_css, "\\.sim-brand-subtitle \\{[^}]*text-overflow: ellipsis;")
  expect_match(sim_ui_css, "\\.navbar \\.navbar-toggle \\{[^}]*flex-shrink: 0;")
  #the expanded header is one row: the nav never wraps, and below 1400 px the theme switch
  #shows its icons only
  expect_match(sim_ui_css, "@media \\(min-width: 992px\\) \\{\\s*\\.navbar \\.navbar-nav \\{\\s*flex-wrap: nowrap;")
  expect_match(sim_ui_css, "@media \\(min-width: 992px\\) and \\(max-width: 1399.98px\\) \\{\\s*\\.theme-btn-label \\{\\s*display: none;")
  #the example select has the full width of the card, with the button under it
  expect_match(sim_settings_io_css, "\\.sim-settings-example \\{[^}]*grid-template-columns: minmax\\(0, 1fr\\);")
  expect_match(sim_tab_css, "\\.sim-compare-table td:first-child \\{[^}]*white-space: nowrap;")
})

# ---------------------------------------------------------------- report edge cases

test_that("count axes have whole-number ticks and single values are drawn in the middle", {
  expect_equal(sim_report_count_ticks(1.1), c(0, 1))
  for (top in c(1.1, 2.2, 3.3, 5.5, 110, 1234.5)) {
    ticks <- sim_report_count_ticks(top)
    expect_true(all(ticks == round(ticks)), info = top)
    expect_equal(ticks[1], 0)
    expect_false(anyDuplicated(ticks) > 0)
  }
  expect_equal(sim_report_amount_range(c(390.35, 390.35)), 390.35 + c(-12, 12) * 3.9035)
  expect_equal(sim_report_amount_range(0), c(-6, 6))
  expect_equal(sim_report_amount_range(c(1, 5, 3)), c(1, 5))
  #a widened range that would overflow is left as it is
  expect_equal(sim_report_amount_range(.Machine$double.xmax), rep(.Machine$double.xmax, 2))
})

test_that("reports of one simulation or of equal totals are built", {
  one <- base_settings(numOfSimulations = 1)
  words <- sim_report_words(one, do.call(simulate_function, one))
  expect_match(words, "TVaR 99.5% is the average of only 1 simulation", fixed = TRUE)
  equal <- base_settings(numOfSimulations = 300, freqDistr = "Fixed_number_of_Counts", freq_params = 2,
                         sevDistr = "Fixed_Severity", sev_params = 100)
  results <- do.call(simulate_function, equal)
  expect_true(all(results$total_claims == 200))
  expect_match(sim_report_words(equal, results), "Key results", fixed = TRUE)
})

test_that("the 95% range for the mean does not go below zero for totals that cannot be negative", {
  #heavy-tailed totals: the normal approximation reached far below zero
  pareto <- base_settings(numOfSimulations = 2000, seedValue = 3, sevDistr = "Pareto", sev_params = c(0.3, 100))
  sm <- summarise_simulation(pareto, do.call(simulate_function, pareto))
  expect_gte(sm$stats$mean_ci[1], 0)
  #by hand: mean 200 and standard error 200, so mean - 1.96 * se is below zero
  hand <- summarise_simulation(base_settings(), c(0, 0, 0, 0, 1000))
  expect_equal(hand$stats$mean_ci, c(0, 200 + 1.96 * hand$stats$se))
  #totals that can be negative keep the plain normal approximation
  negative <- summarise_simulation(base_settings(), c(-1000, 0, 0, 0, 1000))
  expect_equal(negative$stats$mean_ci, c(-1, 1) * 1.96 * negative$stats$se)
  expect_match(sim_report_words(pareto, do.call(simulate_function, pareto)), "normal approximation", fixed = TRUE)
})

# ---------------------------------------------------------------- documented figures

test_that("reinstatements are counted before an aggregate exclusion", {
  #three claims of 100 through 60 xs 30: recoveries 3 * 60 = 180 within the capacity (2 + 1) * 60
  run <- function(agg_layer, agg_deductible, agg_limit) {
    simulate_claims(4, "Fixed_number_of_Counts", 3, "Fixed_Severity", 100, seed = 1,
                    eel_layer = "limited", eel_deductible = 30, eel_limit = 60, eel_reinstatements = 2,
                    agg_layer = agg_layer, agg_deductible = agg_deductible, agg_limit = agg_limit)
  }
  #an aggregate exclusion of 150 xs 50 leaves 180 - 130 = 50, but the reinstatements are
  #counted on the 180 before the exclusion: 180 / 60 = 3, capped at 2
  excluded <- run("exclude", 50, 150)
  expect_equal(excluded$total_claims, rep(50, 4))
  expect_equal(excluded$number_of_reinstatements_used, rep(2, 4))
  #an aggregate layer counts them after its deductible and limit: min(180 - 100, 150) / 60
  limited <- run("limited", 100, 150)
  expect_equal(limited$total_claims, rep(80, 4))
  expect_equal(limited$number_of_reinstatements_used, rep(80 / 60, 4))
})

test_that("the tail counts and TVaR follow their documented definitions", {
  #TVaR 99.5% is the mean of the worst ceiling(n * 0.005) totals
  sm <- summarise_simulation(base_settings(), as.numeric(1:1000))
  expect_equal(sm$stats$tail_count995, 5)
  expect_equal(sm$stats$tvar995, mean(996:1000))
  expect_equal(summarise_simulation(base_settings(), as.numeric(1:1001))$stats$tail_count995, 6)
  #without ties, the simulations at or beyond VaR 99.5% are the worst 0.5%
  expect_equal(sm$stats$beyond_var995, 5)
  #with ties at VaR they are all counted: 990 zero totals and 10 of 100, VaR 100
  tied <- summarise_simulation(base_settings(), c(rep(0, 990), rep(100, 10)))
  expect_equal(tied$stats$var995, 100)
  expect_equal(tied$stats$beyond_var995, 10)
  expect_equal(tied$stats$tail_count995, 5)
  zeros <- summarise_simulation(base_settings(), rep(0, 1000))
  expect_equal(zeros$stats$beyond_var995, 1000)
  expect_equal(sum(zeros$totals >= zeros$stats$var995), zeros$stats$beyond_var995)

  settings <- base_settings(numOfSimulations = 1000)
  words <- sim_report_words(settings, do.call(simulate_function, settings))
  expect_match(words, "Simulations at or beyond VaR 99.5%", fixed = TRUE)
  expect_false(grepl("Simulations beyond VaR", words, fixed = TRUE))
  expect_match(words, "Average of the worst 0.5%", fixed = TRUE)
  expect_match(words, "TVaR 99.5% is the mean of the worst 0.5% of the simulations", fixed = TRUE)
})
