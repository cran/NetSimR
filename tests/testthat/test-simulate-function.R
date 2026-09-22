test_that("simulate_function runs outside Shiny and returns one row per simulation", {
  res <- run_simulation(numOfSimulations = 1000)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1000)
  expect_true(all(c("claim_counts", "total_claims", "gross_claims") %in% names(res)))
  expect_false("number_of_reinstatements_used" %in% names(res))
  expect_false(anyNA(res))
  expect_true(all(res$claim_counts >= 0))
  expect_equal(res$claim_counts, round(res$claim_counts))
  expect_true(all(res$total_claims >= 0))
})

test_that("the number of rows is right when the run spans several chunks", {
  res <- run_simulation(numOfSimulations = 1234, chunk_size = 500)
  expect_equal(nrow(res), 1234)
})

test_that("gross claims are never below the modelled totals", {
  res <- run_layered_simulation()
  expect_true(all(res$gross_claims >= res$total_claims))
  expect_true(any(res$gross_claims > res$total_claims))
})

test_that("without structures the modelled totals equal the gross claims", {
  res <- run_simulation()
  expect_equal(res$total_claims, res$gross_claims)
  #simulations without claims have a zero total and the others a positive one
  expect_true(all(res$total_claims[res$claim_counts == 0] == 0))
  expect_true(all(res$total_claims[res$claim_counts > 0] > 0))
})

test_that("a fixed seed makes a run reproducible", {
  first <- run_simulation(seedValue = 7)
  second <- run_simulation(seedValue = 7)
  other <- run_simulation(seedValue = 8)
  expect_identical(first, second)
  expect_false(identical(first$total_claims, other$total_claims))
})

test_that("a seedValue on its own fixes the seed", {
  args <- list(numOfSimulations = 300, freq_params = 3, sev_params = c(6, 1.5), freqDistr = "Poisson", sevDistr = "LogNormal")
  run <- function(...) do.call(simulate_function, c(args, list(...)))
  #the seed value used to be ignored unless seedSetBinary = TRUE was given as well
  first <- run(seedValue = 5)
  expect_identical(run(seedValue = 5), first)
  expect_identical(run(seedValue = 5, seedSetBinary = TRUE), first)
  expect_false(identical(run(seedValue = 6)$total_claims, first$total_claims))
  #as with seedSetBinary = TRUE, the caller's random number stream is left unchanged
  set.seed(1)
  before <- .Random.seed
  run(seedValue = 5)
  expect_identical(.Random.seed, before)
  #an explicit FALSE still ignores the seed value and draws the seed from the caller's stream
  set.seed(2)
  unseeded <- run(seedValue = 5, seedSetBinary = FALSE)
  expect_false(identical(unseeded$total_claims, first$total_claims))
  set.seed(2)
  expect_identical(run(seedValue = 5, seedSetBinary = FALSE), unseeded)
})

test_that("optional arguments may be omitted", {
  res <- simulate_function(
    numOfSimulations = 500,
    freq_params = 2,
    sev_params = c(2, 100),
    freqDistr = "Poisson",
    sevDistr = "Gamma",
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsuranceStructureAL = "No Reinsurance Structure"
  )
  expect_equal(nrow(res), 500)
  expect_equal(res$total_claims, res$gross_claims)
})

test_that("simulate_claims refuses a progress option that is not a function", {
  run <- function(...) simulate_claims(300, "Poisson", 3, "Gamma", c(2, 100), seed = 1, chunk_size = 100, ...)
  #the engine skipped a progress value that was not a function, so a typo ran silently
  expect_error(run(progress = "no"), "progress must be a function or NULL.", fixed = TRUE)
  expect_error(run(progress = 5), "progress must be a function or NULL.", fixed = TRUE)
  fractions <- numeric()
  res <- run(progress = function(value, detail) fractions <<- c(fractions, value))
  expect_equal(fractions, c(1, 2, 3) / 3)
  expect_identical(res, run())
})

test_that("limited reinstatements cap the ceded total and report the reinstatements used", {
  res <- run_simulation(
    numOfSimulations = 50,
    freqDistr = "Fixed_number_of_Counts", freq_params = 10,
    sevDistr = "Fixed_Severity", sev_params = 100,
    reinsuranceStructureEEL = "Limited Layer",
    reinsurance_structure_eel_dedctible_amount = 50,
    reinsurance_structure_eel_limit_amount = 30,
    reinsuranceStructureLimitedReinstatements = TRUE,
    reinsuranceStructureReinstatementLimit = 2
  )
  expect_true("number_of_reinstatements_used" %in% names(res))
  expect_equal(res$gross_claims, rep(1000, 50))
  #ten claims cede 30 each, 300 in all, capped at (2 + 1) * 30
  expect_equal(res$total_claims, rep(90, 50))
  expect_equal(res$number_of_reinstatements_used, rep(2, 50))
})

test_that("reinstatements used lie between zero and the reinstatement limit", {
  res <- run_layered_simulation(reinsuranceStructureAL = "No Reinsurance Structure")
  used <- res$number_of_reinstatements_used
  expect_true(all(used >= 0 & used <= 2))
  expect_true(all(res$total_claims <= 3 * 5000 + 1e-6))
  #the column is unrounded: the ceded total over the limit, capped at the reinstatements
  expect_identical(used, pmin(res$total_claims / 5000, 2))
})

test_that("unlimited reinstatements do not add the reinstatements column", {
  res <- run_layered_simulation(reinsuranceStructureLimitedReinstatements = FALSE)
  expect_false("number_of_reinstatements_used" %in% names(res))
})

test_that("a run with no claims gives zeros", {
  res <- run_simulation(numOfSimulations = 300, freqDistr = "Fixed_number_of_Counts", freq_params = 0)
  expect_equal(res$claim_counts, rep(0, 300))
  expect_equal(res$total_claims, rep(0, 300))
  expect_equal(res$gross_claims, rep(0, 300))

  layered <- run_layered_simulation(numOfSimulations = 300, freqDistr = "Poisson", freq_params = 0)
  expect_equal(layered$total_claims, rep(0, 300))
  expect_equal(layered$gross_claims, rep(0, 300))
  expect_equal(layered$number_of_reinstatements_used, rep(0, 300))
})

test_that("the severity cap limits each claim", {
  res <- run_simulation(
    numOfSimulations = 500,
    sevDistr = "Fixed_Severity", sev_params = 1000,
    sevCapBinary = TRUE, sev_cap_amount = 400
  )
  expect_equal(res$gross_claims, 400 * res$claim_counts)
})

test_that("a Pareto slice replaces the tail above its threshold", {
  n <- 2000
  res <- run_simulation(
    numOfSimulations = n,
    freqDistr = "Fixed_number_of_Counts", freq_params = 1,
    sevDistr = "Fixed_Severity", sev_params = 100,
    paretoSlice = TRUE, pareto_slice_times = 1,
    slice_pareto_alphas = 3, slice_pareto_x_ms = 50
  )
  #every fixed claim of 100 lies above the threshold, so all are redrawn from Pareto(3, 50)
  expect_true(all(res$total_claims >= 50))
  expect_gt(length(unique(res$total_claims)), 100)
  pareto_mean <- 3 * 50 / 2
  pareto_sd <- 50 / 2 * sqrt(3)
  expect_lt(abs(mean(res$total_claims) - pareto_mean), 4 * pareto_sd / sqrt(n))
})

test_that("a Normal severity truncated at zero never produces negative claims", {
  n <- 5000
  res <- run_simulation(
    numOfSimulations = n,
    freqDistr = "Fixed_number_of_Counts", freq_params = 1,
    sevDistr = "Normal", sev_params = c(100, 200),
    sevTruncateAtZero = TRUE
  )
  expect_true(all(res$total_claims >= 0))
  expect_true(all(res$gross_claims >= 0))

  #closed form of the Normal(100, 200) truncated below at zero
  a <- -100 / 200
  lambda <- dnorm(a) / pnorm(a, lower.tail = FALSE)
  truncated_mean <- 100 + 200 * lambda
  truncated_sd <- 200 * sqrt(1 + a * lambda - lambda^2)
  expect_lt(abs(mean(res$total_claims) - truncated_mean), 4 * truncated_sd / sqrt(n))

  #the same Normal without truncation does produce negative claims
  plain <- run_simulation(
    numOfSimulations = n,
    freqDistr = "Fixed_number_of_Counts", freq_params = 1,
    sevDistr = "Normal", sev_params = c(100, 200),
    sevTruncateAtZero = FALSE
  )
  expect_true(any(plain$total_claims < 0))
})

test_that("truncation at zero is ignored for non-Normal severities", {
  for (case in list(list(distr = "LogNormal", params = c(6, 1.5)),
                    list(distr = "Gamma", params = c(2, 100)),
                    list(distr = "Pareto", params = c(2, 100)))) {
    with_flag <- run_simulation(sevDistr = case$distr, sev_params = case$params, sevTruncateAtZero = TRUE)
    without_flag <- run_simulation(sevDistr = case$distr, sev_params = case$params, sevTruncateAtZero = FALSE)
    expect_identical(with_flag, without_flag, info = case$distr)
  }
})
