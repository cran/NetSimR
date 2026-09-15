#writes a report to a temporary file and returns its text
report_text <- function(settings, results) {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  write_simulation_report(file, settings, results)
  expect_true(file.exists(file))
  paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

has_section <- function(html, id) grepl(paste0('id="', id, '"'), html, fixed = TRUE)

expect_self_contained <- function(html) {
  expect_true(startsWith(html, "<!DOCTYPE html>"))
  expect_true(grepl("<title>Simulation Report</title>", html, fixed = TRUE))
  expect_true(grepl("<style", html, fixed = TRUE))
  expect_true(grepl("data:image/png;base64,", html, fixed = TRUE))
  expect_false(grepl("https?://", html))
  expect_false(grepl("<script[^>]+src=", html))
  for (id in c("key-results", "model-settings", "statistics", "charts", "how-to-read")) {
    expect_true(has_section(html, id), label = paste("section", id))
  }
}

test_that("a run without reinsurance gives a self-contained report", {
  settings <- base_settings(numOfSimulations = 1000)
  results <- do.call(simulate_function, settings)
  html <- report_text(settings, results)
  expect_self_contained(html)
  expect_true(has_section(html, "frequency"))
  expect_false(has_section(html, "gross-net"))
  expect_false(has_section(html, "layer-metrics"))
})

test_that("a layered run reports gross versus net and the layer metrics", {
  settings <- layered_settings(numOfSimulations = 1000)
  results <- do.call(simulate_function, settings)
  html <- report_text(settings, results)
  expect_self_contained(html)
  expect_true(has_section(html, "gross-net"))
  expect_true(has_section(html, "layer-metrics"))
  expect_true(has_section(html, "frequency"))
})

test_that("a numeric vector of totals is accepted", {
  settings <- base_settings(numOfSimulations = 1000)
  totals <- do.call(simulate_function, settings)$total_claims
  html <- report_text(settings, totals)
  expect_self_contained(html)
  expect_false(has_section(html, "frequency"))
})

#the report's text without the embedded images, whose base64 could contain any letters
report_words <- function(html) gsub("data:image/png;base64,[A-Za-z0-9+/=]+", "", html)

test_that("infinite totals are left out of the charts with a note, and the tables show them", {
  #a Pareto alpha this small passes the checks, but half of the totals overflow to Inf;
  #the report used to fail with "need finite 'ylim' values"
  settings <- base_settings(numOfSimulations = 1000, seedValue = 1, sevDistr = "Pareto", sev_params = c(0.002, 100))
  results <- do.call(simulate_function, settings)
  n_infinite <- sum(is.infinite(results$total_claims))
  expect_gt(n_infinite, 0)
  expect_lt(n_infinite, 1000)
  html <- report_text(settings, results)
  expect_self_contained(html)
  words <- report_words(html)
  expect_match(words, paste(formatC(n_infinite, format = "d", big.mark = ","),
                            "simulations \\([0-9.]+%\\) had infinite totals and are left out of the charts"))
  expect_match(words, "Mean</div>\\s*<div class=\"kpi-value\">Inf</div>")
  expect_false(grepl("NaN", words, fixed = TRUE))

  #an excluded layer: gross and net are infinite together, so ceded (Inf - Inf) is unknown
  #and its column blank, where it used to show NaN or statistics of the other rows only
  excluded <- utils::modifyList(settings, list(reinsuranceStructureEEL = "Exclude Layer",
                                               reinsurance_structure_eel_dedctible_amount = 1000,
                                               reinsurance_structure_eel_limit_amount = 5000))
  words <- report_words(report_text(excluded, do.call(simulate_function, excluded)))
  expect_true(has_section(words, "gross-net"))
  expect_match(words, "amount (infinite minus infinite) is unknown and the Ceded column is left blank", fixed = TRUE)
  expect_false(grepl("NaN", words, fixed = TRUE))

  #every total infinite: the charts say so instead of failing
  settings <- base_settings(numOfSimulations = 100, freqDistr = "Fixed_number_of_Counts", freq_params = 1,
                            sevDistr = "Fixed_Severity", sev_params = 1, paretoSlice = TRUE, pareto_slice_times = 1,
                            slice_pareto_alphas = 1e-300, slice_pareto_x_ms = 0.5)
  results <- do.call(simulate_function, settings)
  expect_true(all(is.infinite(results$total_claims)))
  expect_match(report_words(report_text(settings, results)), "100 simulations (100.0%) had infinite totals", fixed = TRUE)
})

test_that("negative zero totals are shown as 0", {
  #no claims under a Normal severity with a negative mean give totals of -0
  settings <- base_settings(numOfSimulations = 1000, sevDistr = "Normal", sev_params = c(-1000, 300))
  words <- report_words(report_text(settings, do.call(simulate_function, settings)))
  expect_false(grepl(">-0(\\.0+)?<", words))
})

test_that("empty totals are an error", {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  expect_error(write_simulation_report(file, base_settings(), numeric(0)))
  expect_error(write_simulation_report(file, base_settings(), data.frame(total_claims = c(NA_real_, NA_real_))))
  expect_error(write_simulation_report(file, base_settings(), data.frame(gross_claims = 1:5)), "total_claims")
})
