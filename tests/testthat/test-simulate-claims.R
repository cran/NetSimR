#simulate_claims(): the interface with short argument names

test_that("simulate_claims gives the same results as simulate_function", {
  short <- simulate_claims(
    2000, "Poisson", 3, "LogNormal", c(6, 1.5), seed = 42,
    pareto_thresholds = c(1000, 5000), pareto_alphas = c(2, 1.5),
    severity_cap = 20000,
    eel_layer = "limited", eel_deductible = 1000, eel_limit = 5000, eel_reinstatements = 2,
    agg_layer = "limited", agg_deductible = 500, agg_limit = 10000
  )
  long <- run_layered_simulation(
    paretoSlice = TRUE, pareto_slice_times = 2, slice_pareto_alphas = c(2, 1.5),
    slice_pareto_x_ms = c(1000, 5000), sevCapBinary = TRUE, sev_cap_amount = 20000
  )
  expect_identical(short, long)
})

test_that("defaults switch the optional features off", {
  res <- simulate_claims(1000, "Poisson", 3, "Gamma", c(2, 100), seed = 1)
  expect_equal(names(res), c("claim_counts", "total_claims", "gross_claims"))
  expect_equal(res$total_claims, res$gross_claims)
  expect_identical(res, run_simulation(numOfSimulations = 1000, sevDistr = "Gamma", sev_params = c(2, 100), seedValue = 1))
})

test_that("without a seed, set.seed() makes the run reproducible", {
  set.seed(3)
  first <- simulate_claims(500, "Poisson", 2, "Exponential", 0.01)
  set.seed(3)
  expect_identical(simulate_claims(500, "Poisson", 2, "Exponential", 0.01), first)
})

test_that("distribution names ignore case, spaces and underscores", {
  base <- simulate_claims(500, "Negative_Binomial", c(2, 1.5), "LogNormal", c(6, 1), seed = 1)
  expect_identical(simulate_claims(500, "negative binomial", c(2, 1.5), "lognormal", c(6, 1), seed = 1), base)
  expect_identical(simulate_claims(500, "NEGATIVE-BINOMIAL", c(2, 1.5), "Log-Normal", c(6, 1), seed = 1), base)
  expect_error(simulate_claims(500, "Weibull", 1, "LogNormal", c(6, 1)), "Unknown frequency distribution \"Weibull\"")
  expect_error(simulate_claims(500, "Poisson", 1, c("Gamma", "Normal"), c(6, 1)), "severity must be one distribution name")
})

test_that("parameters may be named, in any order", {
  base <- simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1.5), seed = 1)
  expect_identical(simulate_claims(500, "Poisson", c(lambda = 3), "LogNormal", c(sdlog = 1.5, meanlog = 6), seed = 1), base)
  #the app's parameter ids work too
  expect_identical(simulate_claims(500, "Poisson", c(lamda = 3), "LogNormal", c(sigma = 1.5, mu = 6), seed = 1), base)
  expect_identical(simulate_claims(500, "Poisson", list(3), "LogNormal", list(6, 1.5), seed = 1), base)
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(meanlog = 6, sd = 1.5)), "names that do not match")
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", 6), "2 numbers for the Log-Normal \\(meanlog, sdlog\\)")
  expect_error(simulate_claims(500, "Poisson", "3", "LogNormal", c(6, 1)), "1 number for the Poisson")
})

test_that("invalid values are reported by the simulator's checks", {
  expect_error(simulate_claims(500, "Poisson", -1, "LogNormal", c(6, 1)), "lambda.*at least 0")
  expect_error(simulate_claims(500, "Poisson", 1, "LogNormal", c(6, 1), eel_layer = "unlimited", eel_deductible = -5),
               "Deductible.*at least 0")
})

test_that("layers need their amounts and accept short or full names", {
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), eel_layer = "limited", eel_limit = 100),
               "eel_deductible is needed")
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), agg_layer = "exclude", agg_deductible = 100),
               "agg_limit is needed")
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), eel_layer = "partial"),
               "eel_layer must be one of")
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), eel_layer = "unlimited",
                               eel_deductible = 100, eel_reinstatements = 1),
               "eel_reinstatements applies only")
  expect_warning(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), eel_layer = "unlimited",
                                 eel_deductible = 100, eel_limit = 50),
                 "eel_limit is ignored")
  expect_warning(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), agg_deductible = 100),
                 "ignored because agg_layer is \"none\"")
  short <- simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), seed = 1, eel_layer = "Limited",
                           eel_deductible = 100, eel_limit = 500)
  full <- simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), seed = 1, eel_layer = "Limited Layer",
                          eel_deductible = 100, eel_limit = 500)
  expect_identical(short, full)
  expect_true(all(short$total_claims <= short$gross_claims))
})

test_that("Pareto slices need one alpha per threshold and at most six", {
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), pareto_thresholds = c(1000, 5000),
                               pareto_alphas = 2),
               "same length")
  expect_error(simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), pareto_thresholds = 1:7 * 1000,
                               pareto_alphas = rep(2, 7)),
               "At most 6 Pareto slices")
  res <- simulate_claims(500, "Poisson", 3, "LogNormal", c(6, 1), seed = 1,
                         pareto_thresholds = 1:6 * 1000, pareto_alphas = c(3, 2.8, 2.5, 2.2, 2, 1.8))
  expect_equal(nrow(res), 500)
})

test_that("truncation is used for the Normal and ignored with a warning otherwise", {
  res <- simulate_claims(2000, "Fixed_number_of_Counts", 1, "Normal", c(mean = 100, sd = 200), seed = 1,
                         truncate_at_zero = TRUE)
  expect_true(all(res$total_claims >= 0))
  expect_warning(simulate_claims(100, "Poisson", 3, "Gamma", c(2, 100), truncate_at_zero = TRUE),
                 "truncate_at_zero is ignored")
})

test_that("the TRUE/FALSE options must be a single TRUE or FALSE", {
  #gross = NA used to drop the gross column silently
  run <- function(...) simulate_claims(100, "Poisson", 3, "LogNormal", c(6, 1.5), seed = 1, ...)
  expect_error(run(gross = NA), "gross must be TRUE or FALSE")
  expect_error(run(gross = "yes"), "gross must be TRUE or FALSE")
  expect_error(run(shortcuts = c(TRUE, FALSE)), "shortcuts must be TRUE or FALSE")
  expect_error(run(parallel = NA), "parallel must be TRUE or FALSE")
  expect_error(run(parallel = 1), "parallel must be TRUE or FALSE")
  expect_error(run(truncate_at_zero = NULL), "truncate_at_zero must be TRUE or FALSE")
})

test_that("parameters mixing named and unnamed values get a clear error", {
  expect_error(simulate_claims(100, "Poisson", 3, "LogNormal", c(8, sdlog = 1.5)),
               "severity_params must be all named or all unnamed.*meanlog, sdlog")
  expect_error(simulate_claims(100, "Negative_Binomial", list(2, beta = 1.5), "LogNormal", c(8, 1.5)),
               "frequency_params must be all named or all unnamed.*r, beta")
})

test_that("gross = FALSE and the progress callback pass through", {
  calls <- 0
  res <- simulate_claims(1000, "Poisson", 3, "LogNormal", c(6, 1.5), seed = 1, gross = FALSE,
                         eel_layer = "unlimited", eel_deductible = 1000, chunk_size = 250,
                         progress = function(value, detail) calls <<- calls + 1)
  expect_false("gross_claims" %in% names(res))
  expect_equal(calls, 4)
})
