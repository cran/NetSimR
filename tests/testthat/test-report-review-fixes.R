#regression tests from a review of the report and the Compare tab: the histogram's zeros,
#the decimals of the headline figures, small parameters, the seed, the axis titles, the
#"truncated at zero" description and the curve of a run of 20 simulations

#the text of a report without its embedded images
review_report_words <- function(settings, results = do.call(simulate_function, settings)) {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  write_simulation_report(file, settings, results)
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  gsub("data:image/png;base64,[A-Za-z0-9+/=]+", "", html)
}

#the first match of a pattern's capture group ("" when there is none)
first_capture <- function(text, pattern) {
  match <- regmatches(text, regexec(pattern, text))[[1]]
  if (length(match) < 2) "" else match[2]
}

#the value of a Key results tile
report_tile <- function(html, label) {
  first_capture(html, paste0('kpi-label">', label, '</div>\\s*<div class="kpi-value">([^<]*)</div>'))
}

#the value of a row of a one-value table (the Statistics section by default)
report_cell <- function(html, metric, section = "statistics") {
  section_html <- sub(paste0('.*<section id="', section, '"'), "", html)
  first_capture(section_html, paste0("<td>", metric, '</td>\\s*<td class="num">([^<]*)</td>'))
}

report_number <- function(text) as.numeric(gsub(",", "", text, fixed = TRUE))

#a Compare tab entry for a run
review_compare_entry <- function(id, settings, data = do.call(simulate_function, settings)) {
  sim_tab_compare_entry(list(id = id, settings = settings, data = data, finished = Sys.time()))
}

# ---------------------------------------------------------------- histogram

test_that("the histogram leaves out only the zero totals, not the negative ones", {
  #a fifth or more zeros: the zeros go, negatives and positives stay
  shown <- sim_report_histogram_totals(c(-5, 0, 0, 0, 3))
  expect_true(shown$zeros_dropped)
  expect_equal(shown$values, c(-5, 3))
  #fewer zeros: every total is drawn
  shown <- sim_report_histogram_totals(c(-5, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  expect_false(shown$zeros_dropped)
  expect_length(shown$values, 11)
  #only zeros: nothing to drop
  expect_false(sim_report_histogram_totals(c(0, 0, 0))$zeros_dropped)
  expect_false(sim_report_histogram_totals(numeric(0))$zeros_dropped)

  #Poisson 0.3 with a Normal severity of a negative mean: 72.6% zeros and 294 negative totals,
  #which used to vanish from the chart while the note mentioned only the zeros
  settings <- base_settings(numOfSimulations = 2000, seedValue = 3, freq_params = 0.3,
                            sevDistr = "Normal", sev_params = c(-100, 1000))
  results <- do.call(simulate_function, settings)
  totals <- results$total_claims
  expect_gt(mean(totals == 0), 0.2)
  n_negative <- sum(totals < 0)
  expect_gt(n_negative, 0)
  shown <- sim_report_histogram_totals(totals, mean(totals == 0))
  expect_true(shown$zeros_dropped)
  expect_equal(sum(shown$values < 0), n_negative)
  expect_false(any(shown$values == 0))
  words <- review_report_words(settings, results)
  expect_match(words, "72.6% of simulations had zero total claims and are left out of this chart", fixed = TRUE)
  expect_lt(report_number(report_cell(words, "Minimum")), 0)
})

# ---------------------------------------------------------------- headline decimals

test_that("the headline figures of a rarely hit layer agree with the statistics table and the Compare tab", {
  #a layer of 100 xs 5,000 that is rarely hit: the ceded mean is 1.905, which the tiles used
  #to show as "2" because their decimals followed the gross totals (in the thousands)
  settings <- base_settings(numOfSimulations = 500, seedValue = 1, freq_params = 2, sev_params = c(6, 1),
                            reinsuranceStructureEEL = "Limited Layer",
                            reinsurance_structure_eel_dedctible_amount = 5000,
                            reinsurance_structure_eel_limit_amount = 100)
  results <- do.call(simulate_function, settings)
  expect_lt(max(results$total_claims), 1000)
  expect_gt(max(results$gross_claims), 1000)
  words <- review_report_words(settings, results)

  expect_identical(report_tile(words, "Mean"), "1.90")
  expect_identical(report_tile(words, "Median"), "0.00")
  expect_identical(report_tile(words, "Standard deviation"), "13.50")
  for (metric in c("Mean", "Median", "Standard deviation", "Maximum")) {
    expect_identical(report_tile(words, metric), report_cell(words, metric), label = metric)
  }
  #the layer metrics and the Ceded column of the Gross, ceded and net table use the same style
  expect_identical(report_cell(words, "Expected loss", "layer-metrics"), "1.90")
  ceded_mean <- first_capture(
    sub('.*<section id="gross-net"', "", words),
    '<td>Mean</td>\\s*<td class="num">[^<]*</td>\\s*<td class="num">([^<]*)</td>'
  )
  expect_identical(ceded_mean, "1.90")
  #the gross column keeps the whole numbers of its own scale
  expect_match(report_cell(words, "Mean", "gross-net"), "^[0-9,]+$")
  #the Compare tab shows the same mean
  entry <- review_compare_entry(1, settings, results)
  expect_identical(sim_tab_fmt_amount(entry$metrics$mean, entry$digits), "1.90")

  #totals in the thousands: the tiles show whole numbers, the statistics two decimals, and
  #they agree to the rounding
  words <- review_report_words(base_settings(numOfSimulations = 1000))
  for (metric in c("Mean", "Median", "Standard deviation", "Maximum")) {
    tile <- report_tile(words, metric)
    expect_match(tile, "^[0-9,]+$", label = metric)
    expect_lt(abs(report_number(tile) - report_number(report_cell(words, metric))), 0.5, label = metric)
  }
})

# ---------------------------------------------------------------- settings

test_that("small distribution parameters and large seeds are shown in full", {
  #an Exponential rate of 1e-5 (a mean claim of 100,000) used to show as "Rate = 0"
  settings <- base_settings(numOfSimulations = 200, sevDistr = "Exponential", sev_params = 1e-5, seedValue = 100000)
  words <- review_report_words(settings)
  expect_match(words, paste0("Rate", intToUtf8(160), "=", intToUtf8(160), "0.00001<"), fixed = TRUE)
  expect_match(words, paste0("lambda (mean claims)", intToUtf8(160), "=", intToUtf8(160), "3<"), fixed = TRUE)
  description <- sim_tab_describe_settings(settings)
  expect_match(description, "Exponential (Rate = 0.00001)", fixed = TRUE)
  expect_match(description, "Poisson (lambda = 3)", fixed = TRUE)
  #ordinary parameters keep at most four decimals
  expect_match(sim_tab_describe_settings(base_settings(sev_params = c(6.123456, 1.5))), "mu = 6.1235, sigma = 1.5", fixed = TRUE)

  #the seed used to be shown in scientific notation ("Fixed at 1e+05")
  expect_match(words, "<dt>Seed</dt>\\s*<dd>Fixed at 100,000</dd>")
  expect_match(description, "seed 100,000", fixed = TRUE)
  expect_match(review_report_words(base_settings(numOfSimulations = 200, seedSetBinary = FALSE)), "Random")
})

test_that("the Compare description says when a Normal severity is truncated at zero", {
  plain <- base_settings(sevDistr = "Normal", sev_params = c(100, 50), sevTruncateAtZero = FALSE)
  truncated <- utils::modifyList(plain, list(sevTruncateAtZero = TRUE))
  expect_false(grepl("truncated", sim_tab_describe_settings(plain), fixed = TRUE))
  expect_match(sim_tab_describe_settings(truncated), "Normal (Mean = 100, Standard deviation = 50) truncated at zero, EEL", fixed = TRUE)
  #the switch only applies to the Normal distribution
  expect_false(grepl("truncated", sim_tab_describe_settings(base_settings(sevTruncateAtZero = TRUE)), fixed = TRUE))
})

# ---------------------------------------------------------------- charts

test_that("a run of 20 simulations has no return-period curve, and the chart says so", {
  #20 simulations stop the curve at return period 2, its only point, which lines() cannot draw
  expect_equal(nrow(sim_tab_return_period_curve(seq_len(19))), 0)
  expect_equal(nrow(sim_tab_return_period_curve(seq_len(20))), 0)
  for (n in 21:30) expect_gte(nrow(sim_tab_return_period_curve(seq_len(n))), 2)

  short <- review_compare_entry(1, base_settings(numOfSimulations = 20))
  long <- review_compare_entry(2, base_settings(numOfSimulations = 500, seedValue = 2))
  expect_equal(nrow(short$curve), 0)
  expect_gt(nrow(long$curve), 2)
  #with the short run included the chart stops at return period 2 and nothing can be drawn
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 600, height = 400)
  expect_no_error(sim_tab_return_period_plot(list(short, long), c("Short", "Long")))
  grDevices::dev.off()

  last_run <- shiny::reactiveVal(NULL)
  shiny::testServer(sim_compare_tab_server, args = list(last_run = last_run), {
    last_run(list(id = 1L, settings = short_settings <- base_settings(numOfSimulations = 20),
                  data = do.call(simulate_function, short_settings), finished = Sys.time()))
    session$flushReact()
    session$elapse(500)
    expect_error(output$return_period_chart, "more than 20 simulations")
  })
})

test_that("the return-period charts title their y axis with what the run models", {
  gross <- review_compare_entry(1, base_settings(numOfSimulations = 500))
  ceded <- review_compare_entry(2, layered_settings(numOfSimulations = 500))
  expect_identical(gross$modelled_label, "Total claims")
  expect_identical(ceded$modelled_label, "Ceded")
  expect_identical(sim_tab_y_label(list(gross)), "Total claims")
  expect_identical(sim_tab_y_label(list(ceded, ceded)), "Ceded")
  expect_identical(sim_tab_y_label(list(gross, ceded)), "Loss")
  expect_true(any(grepl("sim_tab_y_label", deparse(body(sim_tab_return_period_plot)), fixed = TRUE)))

  #the report draws its axis title with graphics::title(); recording its calls while the
  #report is built shows the label (the axis text cannot be read back from the PNG)
  titles <- character(0)
  local_mocked_bindings(title = function(..., ylab = NULL) titles <<- c(titles, ylab), .package = "graphics")
  review_report_words(layered_settings(numOfSimulations = 500))
  expect_true("Ceded" %in% titles)
  expect_false("Total claims" %in% titles)
  titles <- character(0)
  review_report_words(base_settings(numOfSimulations = 500))
  expect_true("Total claims" %in% titles)
})
