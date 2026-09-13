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

#an infinite Poisson mean passes the settings checks but makes the run itself fail
run_failing <- function(session, run) {
  expect_message(
    suppressWarnings(session$setInputs(lamda = Inf, RunSimulations = run)),
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
