#the simulator server, driven through shiny::testServer(): failed runs and the future plan

#inputs for a small sequential run without reinsurance; named arguments override them
set_run_inputs <- function(session, ...) {
  inputs <- list(
    freqDistr = "Poisson", lamda = 3,
    sevDistr = "LogNormal", mu = 6, sigma = 1.5,
    numberOfSimulations = 500,
    seedSetBinary = TRUE, seedValue = 1,
    multiprocessingBinary = FALSE,
    pareto_slice_times = 0,
    sevCapBinary = FALSE, sevTruncateAtZero = FALSE,
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsuranceStructureAL = "No Reinsurance Structure"
  )
  do.call(session$setInputs, utils::modifyList(inputs, list(...)))
}

#a huge (finite) Poisson mean passes the settings checks but makes the run itself fail;
#an infinite one is now stopped by the checks
run_failing <- function(session, run) {
  expect_message(
    suppressWarnings(session$setInputs(lamda = 1e300, RunSimulations = run)),
    "Simulation failed"
  )
}

#testServer() copies plain server variables when it starts; read the current value through
#the environment of a function defined in the server
current_settings <- function(server_function) get("simulation_settings", envir = environment(server_function))

test_that("a failed run keeps the previous results", {
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session)
    session$setInputs(RunSimulations = 1)
    first <- last_run()
    expect_equal(first$id, 1L)
    expect_equal(nrow(simulated_data$data), 500)
    expect_identical(current_settings(restore_original_plan), first$settings)

    run_failing(session, run = 2)
    expect_identical(last_run(), first)
    expect_identical(simulated_data$data, first$data)
    expect_identical(current_settings(restore_original_plan), first$settings)

    #the next successful run replaces them
    session$setInputs(lamda = 2, RunSimulations = 3)
    expect_equal(last_run()$id, 2L)
    expect_equal(current_settings(restore_original_plan)$freq_params, 2)
    expect_identical(simulated_data$data, last_run()$data)
  })
})

test_that("the app leaves the user's future plan in place", {
  old_plan <- future::plan(future::sequential, gc = TRUE)
  on.exit(future::plan(old_plan), add = TRUE)
  user_plan <- future::plan()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session)
    session$setInputs(RunSimulations = 1)
    run_failing(session, run = 2)
    expect_identical(future::plan(), user_plan)
  })
  expect_identical(future::plan(), user_plan)
})

test_that("workers started by the app are shut down and the user's plan restored", {
  skip_on_cran()
  old_options <- options(mc.cores = 2)
  on.exit(options(old_options), add = TRUE)
  old_plan <- future::plan(future::sequential, gc = TRUE)
  on.exit(future::plan(old_plan), add = TRUE)
  user_plan <- future::plan()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session)
    session$setInputs(multiprocessingBinary = TRUE)
    expect_equal(future::nbrOfWorkers(), 2)
    #a failed run shuts the app's workers down; the next run starts them again
    run_failing(session, run = 1)
    expect_identical(future::plan(), user_plan)
    session$setInputs(lamda = 3, RunSimulations = 2)
    expect_equal(nrow(simulated_data$data), 500)
    expect_equal(future::nbrOfWorkers(), 2)
  })
  expect_identical(future::plan(), user_plan)
})

test_that("the compare tab draws the return-period chart for two stored runs, in both themes", {
  last_run <- shiny::reactiveVal(NULL)
  make_run <- function(id, ...) {
    settings <- base_settings(numOfSimulations = 1000, seedValue = id, ...)
    list(id = id, settings = settings, data = do.call(simulate_function, settings), finished = Sys.time())
  }
  shiny::testServer(sim_compare_tab_server, args = list(last_run = last_run), {
    last_run(make_run(1L))
    session$flushReact()
    last_run(make_run(2L, freq_params = 4))
    session$flushReact()
    #the chart inputs are debounced
    session$elapse(500)
    light <- output$return_period_chart
    expect_match(light$src, "^data:image/png;base64,")
    expect_match(light$alt, "return period")

    session$setInputs(app_theme = "dark")
    dark <- output$return_period_chart
    expect_match(dark$src, "^data:image/png;base64,")
    expect_false(identical(dark$src, light$src))
  })
})

test_that("the return-period chart fits six runs with long names in a narrow plot", {
  entries <- lapply(1:6, function(id) {
    settings <- base_settings(numOfSimulations = 500, seedValue = id)
    sim_tab_compare_entry(list(id = id, settings = settings, data = do.call(simulate_function, settings),
                               finished = Sys.time()))
  })
  names <- c(paste("A very long run name that will not fit on one line of the legend", 1:2), paste("Run", 3:6))
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  for (dark in c(FALSE, TRUE)) {
    expect_no_error(shiny::plotPNG(function() sim_tab_return_period_plot(entries, names, dark = dark),
                                   filename = file, width = 320, height = 400, bg = "transparent"))
    expect_gt(file.size(file), 0)
  }
})

test_that("the compare chart's y axis spans negative runs, and infinite totals are left out", {
  #the axis used to run from zero to the largest loss, so a run of negative totals was invisible
  ticks <- sim_tab_y_ticks(c(-2779, -100, 0, Inf, 35000))
  expect_true(all(is.finite(ticks)))
  expect_lte(min(ticks), -2779)
  expect_gte(max(ticks), 35000)
  expect_true(0 %in% ticks)
  expect_equal(range(sim_tab_y_ticks(c(0, 0))), c(0, 1))
  expect_equal(range(sim_tab_y_ticks(c(Inf, NA))), c(0, 1))

  entry_of <- function(id, ...) {
    settings <- base_settings(numOfSimulations = 1000, seedValue = id, ...)
    sim_tab_compare_entry(list(id = id, settings = settings, data = do.call(simulate_function, settings),
                               finished = Sys.time()))
  }
  negative <- entry_of(1, sevDistr = "Normal", sev_params = c(-1000, 300))
  positive <- entry_of(2)
  infinite <- entry_of(3, sevDistr = "Pareto", sev_params = c(0.002, 100))
  expect_true(any(is.infinite(infinite$curve$value)))
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 700, height = 450)
  on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  sim_tab_return_period_plot(list(negative, positive, infinite), c("Negative", "Positive", "Infinite"))
  usr <- graphics::par("usr")
  expect_lte(usr[3], min(negative$curve$value))
  expect_gte(usr[4], max(positive$curve$value))

  #infinite totals show as Inf in the metrics, and their standard deviation as a dash
  expect_identical(sim_tab_fmt_amount(infinite$metrics$mean), "Inf")
  expect_identical(sim_tab_fmt_amount(infinite$metrics$sd), intToUtf8(8212))
  expect_identical(sim_tab_fmt_amount(-0), "0.00")
  expect_false(grepl("NaN", as.character(sim_tab_metrics_table(list(negative, infinite), c("A", "B"))), fixed = TRUE))
  #the x axis reaches 1 in 1,000,000, the longest return period of the largest run
  expect_true(all(c(2e5, 5e5, 1e6) %in% return_period_axis_ticks))
})

test_that("a multi-worker plan set by the user is reused and left running", {
  skip_on_cran()
  old_plan <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)
  user_plan <- future::plan()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session, multiprocessingBinary = TRUE)
    session$setInputs(RunSimulations = 1)
    expect_equal(nrow(simulated_data$data), 500)
    run_failing(session, run = 2)
    expect_identical(future::plan(), user_plan)
  })
  expect_identical(future::plan(), user_plan)
  expect_equal(future::nbrOfWorkers(), 2)
})
