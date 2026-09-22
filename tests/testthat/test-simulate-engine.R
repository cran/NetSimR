#random streams, input ranges, shortcuts and layer-only draws of simulate_function()

test_that("a fixed seed leaves the caller's random number stream unchanged", {
  set.seed(123)
  expected <- runif(3)
  set.seed(123)
  run_simulation(numOfSimulations = 500)
  expect_identical(runif(3), expected)
  expect_identical(RNGkind()[1], "Mersenne-Twister")
})

test_that("a fixed seed gives the same results whatever the caller's normal and sample kinds", {
  old_kind <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3])), add = TRUE)
  RNGkind("default", "default", "default")
  expected <- run_simulation(numOfSimulations = 500)

  RNGkind(normal.kind = "Box-Muller")
  set.seed(5)
  seed_before <- .Random.seed
  expect_identical(run_simulation(numOfSimulations = 500), expected)
  expect_identical(RNGkind()[2], "Box-Muller")
  expect_identical(.Random.seed, seed_before)

  #restoring the "Rounding" sample kind afterwards must not warn
  RNGkind(normal.kind = "Inversion")
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  suppressWarnings(set.seed(5))
  seed_before <- .Random.seed
  expect_no_warning(res <- run_simulation(numOfSimulations = 500))
  expect_identical(res, expected)
  expect_identical(RNGkind()[3], "Rounding")
  expect_identical(.Random.seed, seed_before)
})

test_that("without a fixed seed, set.seed() before the call makes the run reproducible", {
  set.seed(9)
  first <- run_simulation(seedSetBinary = FALSE, seedValue = NULL)
  set.seed(9)
  second <- run_simulation(seedSetBinary = FALSE, seedValue = NULL)
  expect_identical(first, second)
})

#the parallel tests of this file share one 2-worker multisession plan, so its workers start
#once rather than once per test; the plan in place before this file is restored at its end
shared_workers <- new.env()
use_shared_workers <- function() {
  if (is.null(shared_workers$previous_plan)) {
    shared_workers$previous_plan <- future::plan(future::multisession, workers = 2)
  } else if (!(inherits(future::plan(), "multisession") && future::nbrOfWorkers() == 2)) {
    future::plan(future::multisession, workers = 2)
  }
  invisible(NULL)
}

#the socket connections open in this session (the workers' ones), by number and description
open_sockets <- function() {
  connections <- showConnections(all = TRUE)
  is_socket <- connections[, "class"] == "sockconn"
  paste(rownames(connections)[is_socket], connections[is_socket, "description"])
}

test_that("parallel runs give the same results as sequential runs", {
  skip_on_cran()
  use_shared_workers()
  settings <- layered_settings(numOfSimulations = 3000, chunk_size = 700)
  sequential <- do.call(simulate_function, utils::modifyList(settings, list(multiprocessing = FALSE)))
  parallel_run <- do.call(simulate_function, utils::modifyList(settings, list(multiprocessing = TRUE)))
  expect_identical(parallel_run, sequential)
})

test_that("an error in a parallel chunk is raised with its message", {
  skip_on_cran()
  use_shared_workers()
  worker_sockets <- open_sockets()
  #a huge (finite) Poisson mean passes the settings checks but makes the chunks fail
  sequential_error <- tryCatch(suppressWarnings(run_simulation(freq_params = 1e300)), error = identity)
  parallel_error <- tryCatch(suppressWarnings(run_simulation(freq_params = 1e300, multiprocessing = TRUE)),
                             error = identity)
  expect_s3_class(sequential_error, "error")
  expect_s3_class(parallel_error, "error")
  expect_identical(conditionMessage(parallel_error), conditionMessage(sequential_error))
  #the workers survive the error: a chunk group whose result was left unread made future
  #relaunch its worker on the next run, and the new worker's connection was not closed when
  #the plan was shut down (the garbage collector closed it later, with a warning)
  expect_equal(nrow(run_simulation(numOfSimulations = 1000, chunk_size = 250, multiprocessing = TRUE)), 1000)
  expect_identical(open_sockets(), worker_sockets)
})

#values captured from version 0.2.1 (chunks run with future.apply) before the parallel runs
#moved to plain futures; a seed must keep giving these exact results, sequential and parallel
captured_runs <- list(
  lognormal = list(
    settings = base_settings(numOfSimulations = 3000, chunk_size = 700),
    rows = c(1, 700, 701, 3000),
    total_claims = c(1187.7193703133978, 3893.6430360833383, 6946.0622400559878, 3316.9041544597885),
    sum_total = 11301734.919914259, sum_counts = 9074L
  ),
  layered = list(
    settings = layered_settings(numOfSimulations = 2500, seedValue = 7, chunk_size = 600),
    rows = c(1, 600, 601, 2500),
    total_claims = c(0, 0, 0, 5250.1031907718243),
    sum_total = 3000510.9040053124, sum_counts = 7370L
  ),
  sliced = list(
    settings = base_settings(numOfSimulations = 2000, freq_params = c(2, 1.5), sev_params = c(2, 500),
                             seedValue = -123, freqDistr = "Negative_Binomial", sevDistr = "Gamma",
                             chunk_size = 450, paretoSlice = TRUE, pareto_slice_times = 2,
                             slice_pareto_alphas = c(2, 1.5), slice_pareto_x_ms = c(800, 3000),
                             sevCapBinary = TRUE, sev_cap_amount = 20000),
    rows = c(1, 450, 451, 2000),
    total_claims = c(0, 1092.4702047802709, 1258.2885751184531, 0),
    sum_total = 6403422.2416602327, sum_counts = 5969L
  ),
  layer_only = list(
    settings = base_settings(numOfSimulations = 2000, freq_params = 4, sev_params = c(7, 1.2), seedValue = 2024,
                             chunk_size = 500, gross = FALSE, reinsuranceStructureEEL = "Unlimited Layer",
                             reinsurance_structure_eel_dedctible_amount = 2000),
    rows = c(1, 500, 501, 2000),
    total_claims = c(9365.3522021841109, 18682.392983290858, 21316.137577761365, 10301.807862973266),
    sum_total = 9447174.21068554, sum_counts = 8133L
  ),
  one_step = list(
    settings = base_settings(numOfSimulations = 1800, freq_params = 5, sev_params = c(2, 400), seedValue = 99,
                             sevDistr = "Gamma", chunk_size = 400),
    rows = c(1, 400, 401, 1800),
    total_claims = c(2166.2649671083527, 3232.8910058146676, 3533.7957860159777, 3322.5562143139823),
    sum_total = 7226656.7484657066, sum_counts = 9141L
  )
)

expect_captured_run <- function(res, case, label) {
  #the random streams are the same on every platform, but exp(), log() and the quantile
  #functions can differ in the last bits (e.g. on macOS arm64), so the amounts are compared
  #to a tight relative tolerance; the claim counts must match exactly
  expect_equal(res$total_claims[case$rows], case$total_claims, tolerance = 1e-10, label = label)
  expect_equal(sum(res$total_claims), case$sum_total, tolerance = 1e-10, label = label)
  expect_identical(sum(res$claim_counts), case$sum_counts, label = label)
}

test_that("seeded runs give the results captured from earlier versions", {
  for (name in names(captured_runs)) {
    case <- captured_runs[[name]]
    expect_captured_run(do.call(simulate_function, case$settings), case, name)
  }
})

test_that("seeded parallel runs give the captured results, on the caller's plan or their own", {
  skip_on_cran()
  use_shared_workers()
  for (name in names(captured_runs)) {
    case <- captured_runs[[name]]
    settings <- utils::modifyList(case$settings, list(multiprocessing = TRUE))
    expect_captured_run(do.call(simulate_function, settings), case, paste(name, "(caller's plan)"))
  }
  #with a one-worker plan the call starts its own workers and restores the plan afterwards
  future::plan(future::sequential)
  sequential_plan <- future::plan()
  old_options <- options(mc.cores = 2, parallelly.availableCores.methods = "mc.cores")
  on.exit(options(old_options), add = TRUE)
  case <- captured_runs$layered
  settings <- utils::modifyList(case$settings, list(multiprocessing = TRUE))
  expect_captured_run(do.call(simulate_function, settings), case, "layered (own plan)")
  expect_identical(future::plan(), sequential_plan)
})

test_that("a parallel call on its own workers leaves no connections open, after an error too", {
  skip_on_cran()
  old_plan <- future::plan(future::sequential)
  on.exit(future::plan(old_plan), add = TRUE)
  old_options <- options(mc.cores = 2, parallelly.availableCores.methods = "mc.cores")
  on.exit(options(old_options), add = TRUE)
  sockets_before <- open_sockets()
  expect_equal(nrow(run_simulation(numOfSimulations = 1000, chunk_size = 250, multiprocessing = TRUE)), 1000)
  expect_identical(open_sockets(), sockets_before)
  expect_error(suppressWarnings(run_simulation(freq_params = 1e300, multiprocessing = TRUE)))
  expect_identical(open_sockets(), sockets_before)
})

test_that("the progress callback is called once per chunk", {
  calls <- numeric(0)
  run_simulation(numOfSimulations = 1000, chunk_size = 250,
                 progress = function(value, detail) calls <<- c(calls, value))
  expect_equal(calls, c(0.25, 0.5, 0.75, 1))
})

test_that("the default chunk size handles high claim frequencies", {
  res <- run_simulation(numOfSimulations = 1500, freq_params = 1000, sevDistr = "Exponential", sev_params = 0.01,
                        reinsuranceStructureEEL = "Unlimited Layer", reinsurance_structure_eel_dedctible_amount = 0)
  expect_equal(nrow(res), 1500)
  expect_lt(abs(mean(res$claim_counts) - 1000), 5)
})

test_that("the default chunk size keeps at least 100 simulations per chunk", {
  #20,000 claims per simulation would give chunks of 50 for a million claims; the floor of
  #100 simulations (documented under chunk_size) gives 3 chunks for 250 simulations
  calls <- 0
  res <- run_simulation(numOfSimulations = 250, freqDistr = "Fixed_number_of_Counts", freq_params = 20000,
                        sevDistr = "Fixed_Severity", sev_params = 1,
                        progress = function(value, detail) calls <<- calls + 1)
  expect_equal(calls, 3)
  expect_equal(res$total_claims, rep(20000, 250))
})

test_that("out-of-range distribution parameters are named in the error", {
  expect_range_error <- function(regexp, ...) expect_error(run_simulation(...), regexp)
  expect_range_error("lambda.*at least 0", freq_params = -1)
  expect_range_error("probability.*at most 1", freqDistr = "Binomial", freq_params = c(10, 1.5))
  expect_range_error("r [(]shape[)].*greater than 0", freqDistr = "Negative_Binomial", freq_params = c(-2, 1))
  expect_range_error("beta.*greater than 0", freqDistr = "Negative_Binomial", freq_params = c(2, 0))
  expect_range_error("Scale.*greater than 0", sevDistr = "Gamma", sev_params = c(2, 0))
  expect_range_error("Rate.*greater than 0", sevDistr = "Exponential", sev_params = 0)
  expect_range_error("alpha.*greater than 0", sevDistr = "Pareto", sev_params = c(0, 100))
  expect_range_error("Standard deviation.*at least 0", sevDistr = "Normal", sev_params = c(1000, -5))
  expect_range_error("sigma.*at least 0", sev_params = c(6, -1))
  expect_range_error("Claim amount.*at least 0", sevDistr = "Fixed_Severity", sev_params = -10)
})

test_that("negative means are valid for the Normal and the Log-Normal", {
  expect_equal(nrow(run_simulation(numOfSimulations = 100, sev_params = c(-1, 0.5))), 100)
  expect_equal(nrow(run_simulation(numOfSimulations = 100, sevDistr = "Normal", sev_params = c(-100, 500),
                                   sevTruncateAtZero = TRUE)), 100)
})

test_that("negative amounts and zero limits are rejected", {
  expect_error(run_simulation(reinsuranceStructureEEL = "Unlimited Layer",
                              reinsurance_structure_eel_dedctible_amount = -500),
               "EEL Deductible Amount must be at least 0")
  expect_error(run_simulation(sevCapBinary = TRUE, sev_cap_amount = -100), "Severity Cap Amount must be at least 0")
  expect_error(run_layered_simulation(reinsurance_structure_eel_limit_amount = 0),
               "EEL Limit Amount must be greater than 0")
  expect_error(run_layered_simulation(reinsurance_structure_al_limit_amount = -1),
               "AL Limit Amount must be greater than 0")
  expect_error(run_layered_simulation(reinsuranceStructureReinstatementLimit = -1), "Reinstatements must be at least 0")
  expect_error(run_layered_simulation(reinsuranceStructureReinstatementLimit = 1.5),
               "Reinstatements must be a whole number")
  expect_error(run_simulation(paretoSlice = TRUE, pareto_slice_times = 1, slice_pareto_alphas = 0,
                              slice_pareto_x_ms = 100),
               "Slice 1 alpha must be greater than 0")
  expect_error(run_simulation(chunk_size = 0), "Chunk size")
  #a deductible of zero and no reinstatements are valid
  expect_equal(nrow(run_layered_simulation(reinsurance_structure_eel_dedctible_amount = 0,
                                           reinsuranceStructureReinstatementLimit = 0)), 2000)
})

test_that("the distribution objects are valid and complete", {
  #the validity method is called directly: validObject() needs the installed package
  validity <- methods::getValidity(methods::getClassDef("distributionClass"))
  for (object in c(freq_dist_options, sev_dist_options)) {
    expect_true(isTRUE(validity(object)), info = object@distrID)
  }
  for (object in freq_dist_options) expect_false(is.null(object@split_func), info = object@distrID)
  for (object in sev_dist_options) expect_false(is.null(object@survival_func), info = object@distrID)
  bad <- freq_dist_options$Negative_Binomial
  bad@param_labels <- "r (shape)"
  expect_match(validity(bad), "param_labels must have one label per parameter")
  bad <- sev_dist_options$Gamma
  bad@tail_quantile_func <- NULL
  expect_match(validity(bad), "given together")
})

test_that("tail quantiles invert the survival functions", {
  params <- list(Normal = c(1000, 300), LogNormal = c(7, 1.2), Gamma = c(2, 500),
                 Exponential = 0.002, Pareto = c(2.5, 400))
  s <- c(0.9, 0.5, 0.1, 1e-4)
  for (id in names(params)) {
    object <- sev_dist_options[[id]]
    x <- object@tail_quantile_func(s, params[[id]])
    expect_equal(object@survival_func(x, params[[id]]), s, tolerance = 1e-8, info = id)
  }
  model <- severity_model(sev_dist_options$Normal, c(100, 200), truncate_at_zero = TRUE)
  expect_equal(model$survival(model$tail_quantile(c(0.9, 0.2))), c(0.9, 0.2), tolerance = 1e-8)
  expect_equal(model$survival(0), 1)
})

test_that("the spliced Pareto tail matches the slice-by-slice construction", {
  model <- severity_model(sev_dist_options$LogNormal, c(8, 1.5))
  spliced <- pareto_splice(model, thresholds = c(10000, 50000), alphas = c(1.8, 1.2))
  s1 <- model$survival(10000)
  expected <- c(
    model$survival(5000), s1,
    s1 * (10000 / 20000)^1.8,
    s1 * (10000 / 50000)^1.8,
    s1 * (10000 / 50000)^1.8 * (50000 / 2e5)^1.2
  )
  expect_equal(spliced$survival(c(5000, 10000, 20000, 50000, 2e5)), expected)
  s <- c(0.5, spliced$survival(20000), spliced$survival(1e6))
  expect_equal(spliced$survival(spliced$tail_quantile(s)), s, tolerance = 1e-10)
  draws <- spliced$draw_above_first(1e5)
  expect_true(all(draws > 10000))
  #share above 50,000 among the claims above 10,000
  expect_lt(abs(mean(draws > 50000) - (10000 / 50000)^1.8), 0.005)
})

test_that("totals drawn in one step follow the claim-by-claim distribution", {
  n <- 40000
  cases <- list(
    list(distr = "Gamma", params = c(2, 500), mean = 1000, var = 2 * 500^2),
    list(distr = "Exponential", params = 0.001, mean = 1000, var = 1000^2),
    list(distr = "Normal", params = c(1000, 200), mean = 1000, var = 200^2),
    list(distr = "Fixed_Severity", params = 1000, mean = 1000, var = 0)
  )
  for (case in cases) {
    res <- run_simulation(numOfSimulations = n, freq_params = 4, sevDistr = case$distr, sev_params = case$params)
    #compound Poisson: mean lambda * E[X], variance lambda * E[X^2]
    total_mean <- 4 * case$mean
    total_var <- 4 * (case$var + case$mean^2)
    expect_lt(abs(mean(res$total_claims) - total_mean), 4 * sqrt(total_var / n), label = case$distr)
    expect_lt(abs(var(res$total_claims) / total_var - 1), 0.05, label = case$distr)
    expect_true(all(res$total_claims[res$claim_counts == 0] == 0))
    slow <- run_simulation(numOfSimulations = 2000, freq_params = 4, sevDistr = case$distr,
                           sev_params = case$params, shortcuts = FALSE)
    expect_equal(nrow(slow), 2000)
  }
})

test_that("drawing only the claims that reach the layer gives the same distribution", {
  n <- 60000
  compare <- function(...) {
    full <- run_simulation(numOfSimulations = n, seedValue = 1, ...)
    thin <- run_simulation(numOfSimulations = n, seedValue = 2, gross = FALSE, ...)
    expect_false("gross_claims" %in% names(thin))
    #a fixed number of claims has no spread, hence the small tolerance
    expect_lte(abs(mean(thin$claim_counts) - mean(full$claim_counts)), 5 * sd(full$claim_counts) / sqrt(n) + 1e-9)
    se <- sqrt((var(full$total_claims) + var(thin$total_claims)) / n)
    expect_lt(abs(mean(thin$total_claims) - mean(full$total_claims)), 5 * se)
    expect_lt(abs(mean(thin$total_claims == 0) - mean(full$total_claims == 0)), 0.01)
    list(full = full, thin = thin)
  }
  layer <- list(reinsuranceStructureEEL = "Limited Layer", reinsurance_structure_eel_dedctible_amount = 1000,
                reinsurance_structure_eel_limit_amount = 5000)
  do.call(compare, layer)
  do.call(compare, c(layer, list(freqDistr = "Negative_Binomial", freq_params = c(2, 1.5))))
  do.call(compare, c(layer, list(freqDistr = "Binomial", freq_params = c(10, 0.3))))
  do.call(compare, c(layer, list(freqDistr = "Fixed_number_of_Counts", freq_params = 3)))
  do.call(compare, list(reinsuranceStructureEEL = "Unlimited Layer", reinsurance_structure_eel_dedctible_amount = 2000,
                        sevDistr = "Gamma", sev_params = c(2, 500)))
  do.call(compare, c(layer, list(paretoSlice = TRUE, pareto_slice_times = 2, slice_pareto_alphas = c(2, 1.5),
                                 slice_pareto_x_ms = c(800, 3000), sevCapBinary = TRUE, sev_cap_amount = 4000)))
  do.call(compare, c(layer, list(sevDistr = "Normal", sev_params = c(500, 800), sevTruncateAtZero = TRUE)))
  #reinstatements and an aggregate layer act on the layer-only totals as usual
  runs <- do.call(compare, c(layer, list(reinsuranceStructureLimitedReinstatements = TRUE,
                                        reinsuranceStructureReinstatementLimit = 1,
                                        reinsuranceStructureAL = "Limited Layer",
                                        reinsurance_structure_al_dedctible_amount = 500,
                                        reinsurance_structure_al_limit_amount = 6000)))
  expect_true(all(runs$thin$total_claims <= 6000))
  expect_true(all(runs$thin$number_of_reinstatements_used <= 1))
})

test_that("a cap at or below the deductible cedes nothing with layer-only draws", {
  res <- run_simulation(numOfSimulations = 500, gross = FALSE, sevCapBinary = TRUE, sev_cap_amount = 800,
                        reinsuranceStructureEEL = "Unlimited Layer", reinsurance_structure_eel_dedctible_amount = 1000)
  expect_equal(res$total_claims, rep(0, 500))
  expect_gt(mean(res$claim_counts), 2.5)
})

#put back the plan in place before this file (on CRAN the parallel tests are skipped and
#never start the shared workers)
if (!is.null(shared_workers$previous_plan)) future::plan(shared_workers$previous_plan)
