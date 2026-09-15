#results are returned unrounded, and summaries use the unrounded values

test_that("totals of small claims are not rounded to two decimals", {
  #claims average 0.001: rounding used to make most totals zero (P(total = 0) 0.914, not exp(-2))
  n <- 20000
  res <- simulate_claims(n, "Poisson", 2, "Exponential", 1000, seed = 1)
  expect_identical(res$total_claims, res$gross_claims)
  expect_true(any(res$gross_claims != round(res$gross_claims, 2)))
  expect_lt(abs(mean(res$total_claims == 0) - exp(-2)), 0.01)
  #compound Poisson: mean 2 * 0.001, variance 2 * E[X^2] = 2 * 2e-6
  expect_lt(abs(mean(res$total_claims) - 0.002), 4 * sqrt(4e-6 / n))
  settings <- base_settings(numOfSimulations = n, sevDistr = "Exponential", sev_params = 1000, freq_params = 2, seedValue = 1)
  expect_equal(summarise_simulation(settings, res)$stats$zero_share, mean(res$total_claims == 0))
})

test_that("the layer hit probability of amounts in millions uses unrounded totals", {
  #claims in millions through a layer above 0.05: the exact hit probability is 0.6309;
  #rounded totals gave 0.6027
  n <- 20000
  settings <- base_settings(numOfSimulations = n, freq_params = 2, sev_params = c(-3, 1), seedValue = 1,
                            reinsuranceStructureEEL = "Unlimited Layer",
                            reinsurance_structure_eel_dedctible_amount = 0.05)
  res <- do.call(simulate_function, settings)
  exact <- 1 - exp(-2 * stats::plnorm(0.05, -3, 1, lower.tail = FALSE))
  layer <- summarise_simulation(settings, res)$layer
  expect_lt(abs(layer$hit_prob - exact), 4 * sqrt(exact * (1 - exact) / n))
  for (shortcuts in c(TRUE, FALSE)) {
    thin <- do.call(simulate_function, utils::modifyList(settings, list(gross = FALSE, shortcuts = shortcuts)))
    expect_lt(abs(mean(thin$total_claims > 0) - exact), 4 * sqrt(exact * (1 - exact) / n))
  }
})

test_that("reinstatements used are not rounded and 'all used' is exact", {
  #one claim of 299 through 300 xs 0 uses 299 / 300 of the one reinstatement, not all of it
  res <- simulate_claims(10, "Fixed_number_of_Counts", 1, "Fixed_Severity", 299, seed = 1,
                         eel_layer = "limited", eel_deductible = 0, eel_limit = 300, eel_reinstatements = 1)
  expect_identical(res$number_of_reinstatements_used, rep(299 / 300, 10))
  settings <- base_settings(numOfSimulations = 10, freqDistr = "Fixed_number_of_Counts", freq_params = 1,
                            sevDistr = "Fixed_Severity", sev_params = 299,
                            reinsuranceStructureEEL = "Limited Layer", reinsurance_structure_eel_dedctible_amount = 0,
                            reinsurance_structure_eel_limit_amount = 300,
                            reinsuranceStructureLimitedReinstatements = TRUE, reinsuranceStructureReinstatementLimit = 1)
  layer <- summarise_simulation(settings, do.call(simulate_function, settings))$layer
  expect_equal(layer$reinstatements_avg, 299 / 300)
  expect_equal(layer$reinstatements_all_used_prob, 0)
  expect_equal(layer$exhaust_prob, 0)

  #a claim of 300 does use it all
  full <- utils::modifyList(settings, list(sev_params = 300))
  layer <- summarise_simulation(full, do.call(simulate_function, full))$layer
  expect_equal(layer$reinstatements_all_used_prob, 1)

  #only floating-point noise counts as reaching the limit or the capacity
  hand <- data.frame(total_claims = c(600 * (1 - 1e-12), 599.999, 0.0025),
                     number_of_reinstatements_used = c(1 - 1e-12, 1 - 1e-6, 0))
  layer <- summarise_simulation(utils::modifyList(settings, list(reinsurance_structure_eel_limit_amount = 300)), hand)$layer
  expect_equal(layer$reinstatements_all_used_prob, 1 / 3)
  expect_equal(layer$exhaust_prob, 1 / 3)
  #a small capacity is not reached by totals well below it (an absolute tolerance of 0.005 did)
  small <- utils::modifyList(settings, list(reinsurance_structure_eel_limit_amount = 0.0015))
  expect_equal(summarise_simulation(small, hand)$layer$exhaust_prob, 2 / 3)
})

test_that("small amounts keep significant digits on screen and in the report", {
  expect_equal(display_digits(1500), 0)
  expect_equal(display_digits(c(12.5, 999)), 2)
  expect_equal(display_digits(0), 2)
  expect_equal(display_digits(0.002), 5)
  expect_equal(display_digits(c(NA, Inf, 0.05)), 4)
  expect_equal(sim_tab_fmt_amount(0.00123), "0.00123")
  expect_equal(sim_tab_fmt_amount(123456.7), "123,457")

  settings <- base_settings(numOfSimulations = 1000, sevDistr = "Exponential", sev_params = 1000)
  res <- do.call(simulate_function, settings)
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  write_simulation_report(file, settings, res)
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mean_text <- formatC(mean(res$total_claims), format = "f", digits = display_digits(res$total_claims))
  expect_true(grepl(mean_text, html, fixed = TRUE))
  expect_false(grepl("^0[.]0+$", mean_text))
})
