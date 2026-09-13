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

test_that("empty totals are an error", {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  expect_error(write_simulation_report(file, base_settings(), numeric(0)))
  expect_error(write_simulation_report(file, base_settings(), data.frame(total_claims = c(NA_real_, NA_real_))))
  expect_error(write_simulation_report(file, base_settings(), data.frame(gross_claims = 1:5)), "total_claims")
})
