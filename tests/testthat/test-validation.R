#messages are matched on fragments so that small wording changes do not break the tests
expect_settings_error <- function(regexp, ...) {
  expect_error(run_simulation(...), regexp, ignore.case = TRUE)
}

test_that("complete settings pass validation", {
  expect_length(find_missing_simulation_settings(base_settings()), 0)
  expect_length(find_missing_simulation_settings(layered_settings()), 0)
})

test_that("a blank AL limit is named in the error", {
  expect_settings_error("AL Limit Amount", reinsuranceStructureAL = "Limited Layer",
                        reinsurance_structure_al_dedctible_amount = 100)
  expect_settings_error("AL Limit Amount", reinsuranceStructureAL = "Limited Layer",
                        reinsurance_structure_al_dedctible_amount = 100,
                        reinsurance_structure_al_limit_amount = NA_real_)
  expect_settings_error("AL Limit Amount", reinsuranceStructureAL = "Exclude Layer",
                        reinsurance_structure_al_dedctible_amount = 100,
                        reinsurance_structure_al_limit_amount = "abc")
  expect_settings_error("AL Deductible Amount", reinsuranceStructureAL = "Unlimited Layer")
  expect_settings_error("EEL Limit Amount", reinsuranceStructureEEL = "Limited Layer",
                        reinsurance_structure_eel_dedctible_amount = 100)
  expect_settings_error("Number of Reinstatements", reinsuranceStructureEEL = "Limited Layer",
                        reinsurance_structure_eel_dedctible_amount = 100,
                        reinsurance_structure_eel_limit_amount = 1000,
                        reinsuranceStructureLimitedReinstatements = TRUE)
})

test_that("a blank severity cap is named in the error", {
  expect_settings_error("Severity Cap", sevCapBinary = TRUE)
  expect_settings_error("Severity Cap", sevCapBinary = TRUE, sev_cap_amount = NA_real_)
})

test_that("a blank distribution parameter is named in the error", {
  expect_settings_error("Severity parameter.*sigma", sev_params = c(6, NA))
  expect_settings_error("Severity parameter.*sigma", sev_params = 6)
  expect_settings_error("Frequency parameter.*lam", freq_params = NULL)
  expect_settings_error("Frequency parameter.*beta", freqDistr = "Negative_Binomial", freq_params = list(4))
  expect_settings_error("Severity distribution", sevDistr = "Weibull")
  expect_settings_error("EEL reinsurance structure", reinsuranceStructureEEL = "Some Layer")
})

test_that("several problems are reported together", {
  expect_settings_error("Severity Cap.*AL Limit Amount|AL Limit Amount.*Severity Cap",
                        sevCapBinary = TRUE,
                        reinsuranceStructureAL = "Limited Layer",
                        reinsurance_structure_al_dedctible_amount = 100)
})

test_that("claim counts must be whole numbers", {
  expect_settings_error("whole number", freqDistr = "Fixed_number_of_Counts", freq_params = 2.5)
  expect_settings_error("whole number", freqDistr = "Binomial", freq_params = c(2.5, 0.3))
  expect_equal(nrow(run_simulation(numOfSimulations = 100, freqDistr = "Binomial", freq_params = c(3, 0.3))), 100)
})

test_that("Pareto slice thresholds must increase", {
  expect_settings_error("increas", paretoSlice = TRUE, pareto_slice_times = 2,
                        slice_pareto_alphas = c(2, 3), slice_pareto_x_ms = c(1000, 500))
  expect_settings_error("increas", paretoSlice = TRUE, pareto_slice_times = 2,
                        slice_pareto_alphas = c(2, 3), slice_pareto_x_ms = c(1000, 1000))
  expect_settings_error("Slice.*2", paretoSlice = TRUE, pareto_slice_times = 2,
                        slice_pareto_alphas = c(2, 3), slice_pareto_x_ms = c(1000))
  res <- run_simulation(numOfSimulations = 200, paretoSlice = TRUE, pareto_slice_times = 2,
                        slice_pareto_alphas = c(2, 3), slice_pareto_x_ms = c(1000, 5000))
  expect_equal(nrow(res), 200)
})

test_that("the number of simulations must be a whole number between 1 and 10,000,000", {
  expect_settings_error("Number of simulations.*whole number", numOfSimulations = 2.5)
  expect_settings_error("Number of simulations", numOfSimulations = 0)
  expect_settings_error("Number of simulations", numOfSimulations = -5)
  expect_settings_error("Number of simulations", numOfSimulations = 10000001)
  expect_settings_error("Number of simulations", numOfSimulations = NULL)
  expect_settings_error("Number of simulations", numOfSimulations = "100")
  expect_equal(nrow(run_simulation(numOfSimulations = 1)), 1)
})

test_that("a fixed seed must be a whole number", {
  expect_settings_error("Seed.*whole number", seedSetBinary = TRUE, seedValue = 2.5)
  expect_settings_error("Seed", seedSetBinary = TRUE, seedValue = NULL)
  expect_settings_error("Seed", seedSetBinary = TRUE, seedValue = "abc")
  expect_equal(nrow(run_simulation(numOfSimulations = 100, seedSetBinary = FALSE, seedValue = NULL)), 100)
})

test_that("a fixed seed must lie in the integer range that set.seed() accepts", {
  expect_true(any(grepl("Seed value must be between", find_missing_simulation_settings(base_settings(seedValue = 1e10)))))
  expect_settings_error("Seed value must be between", seedValue = 1e10)
  expect_settings_error("Seed value must be between", seedValue = -1e10)
  expect_settings_error("Seed value must be between", seedValue = .Machine$integer.max + 1)
  expect_settings_error("Seed", seedValue = Inf)
  expect_equal(nrow(run_simulation(numOfSimulations = 100, seedValue = .Machine$integer.max)), 100)
  expect_equal(nrow(run_simulation(numOfSimulations = 100, seedValue = -.Machine$integer.max)), 100)
})

test_that("the app's number of simulations input has the limits that validation applies", {
  html <- as.character(shiny_simulator_ui)
  expect_match(html, 'id="numberOfSimulations"[^>]*min="1"[^>]*max="10000000"')
  expect_length(find_missing_simulation_settings(base_settings(numOfSimulations = max_number_of_simulations)), 0)
  expect_settings_error("Number of simulations.*10,000,000", numOfSimulations = max_number_of_simulations + 1)
})

test_that("a Normal with no probability above zero cannot be truncated at zero", {
  expect_settings_error("zero|truncat", sevDistr = "Normal", sev_params = c(-1000, 1), sevTruncateAtZero = TRUE)
  #the same Normal is accepted when truncation is off
  expect_equal(nrow(run_simulation(numOfSimulations = 100, sevDistr = "Normal",
                                   sev_params = c(-1000, 1), sevTruncateAtZero = FALSE)), 100)
  #a Normal with some probability above zero is accepted with truncation
  expect_equal(nrow(run_simulation(numOfSimulations = 100, sevDistr = "Normal",
                                   sev_params = c(-1000, 1000), sevTruncateAtZero = TRUE)), 100)
})
