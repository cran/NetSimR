#saved settings: the ids, the built-in examples and the loading of older files

test_that("the settings ids cover every slice and no longer include the slice switch", {
  ids <- sim_settings_input_ids()
  expect_true(all(paste0("slice_pareto_param_", seq_len(2 * max_number_of_pareto_slices)) %in% ids))
  expect_true("pareto_slice_times" %in% ids)
  expect_false("paretoSlice" %in% ids)
  expect_equal(max_number_of_pareto_slices, 6)
})

test_that("the built-in examples give the number of slices", {
  for (name in names(sim_settings_examples)) {
    example <- sim_settings_examples[[name]]
    expect_null(example$paretoSlice, info = name)
    expect_true(is.numeric(example$pareto_slice_times), info = name)
  }
})

test_that("older files with the slice switch load as a number of slices", {
  off <- sim_settings_migrate(list(paretoSlice = FALSE, pareto_slice_times = "3"), version = 2)
  expect_equal(off$pareto_slice_times, 0)
  expect_false("paretoSlice" %in% names(off))

  on <- sim_settings_migrate(
    list(paretoSlice = TRUE, pareto_slice_times = "2", slice_pareto_param_1 = 2,
         slice_pareto_param_2 = 1000, slice_pareto_param_3 = 1.5, slice_pareto_param_4 = 5000),
    version = 2
  )
  expect_equal(on$pareto_slice_times, "2")
  expect_false("paretoSlice" %in% names(on))
  entries <- sim_settings_plan(on)
  ids <- vapply(entries, `[[`, character(1), "id")
  expect_true(all(c("pareto_slice_times", paste0("slice_pareto_param_", 1:4)) %in% ids))
  #the slice fields are always in the page, so they are set without waiting
  slice_entries <- entries[ids %in% paste0("slice_pareto_param_", 1:4)]
  expect_true(all(vapply(slice_entries, function(e) is.null(e$gate), logical(1))))
})

test_that("only the slices in use are restored", {
  inputs <- list(pareto_slice_times = 1, slice_pareto_param_1 = 2, slice_pareto_param_2 = 1000,
                 slice_pareto_param_3 = 9, slice_pareto_param_4 = 9)
  ids <- vapply(sim_settings_plan(inputs), `[[`, character(1), "id")
  expect_true(all(c("slice_pareto_param_1", "slice_pareto_param_2") %in% ids))
  expect_false(any(c("slice_pareto_param_3", "slice_pareto_param_4") %in% ids))
})

test_that("the truncation switch is restored once, after the Normal severity", {
  example <- sim_settings_collect(sim_settings_examples[["Simple: Normal claims truncated at zero"]])
  entries <- sim_settings_plan(sim_settings_migrate(example$inputs, example$version))
  ids <- vapply(entries, `[[`, character(1), "id")
  expect_equal(sum(ids == "sevTruncateAtZero"), 1)
  truncate <- entries[[which(ids == "sevTruncateAtZero")]]
  expect_true(truncate$value)
  expect_true(truncate$gate(list(sevDistr = "Normal")))
  expect_false(truncate$gate(list(sevDistr = "LogNormal")))

  #other severities do not use the switch
  motor <- sim_settings_plan(sim_settings_examples[["Motor: excess of loss layer"]])
  expect_false("sevTruncateAtZero" %in% vapply(motor, `[[`, character(1), "id"))

  #loading the example queues the switch once, to be set when the Normal is on the page
  shiny::testServer(sim_settings_io_server, {
    session$setInputs(settingsIO_example = "Simple: Normal claims truncated at zero", settingsIO_load_example = 1)
    queued <- vapply(pending(), `[[`, character(1), "id")
    expect_equal(sum(queued == "sevTruncateAtZero"), 1)
  })
})

test_that("six Pareto slices can be simulated", {
  res <- run_simulation(
    numOfSimulations = 500, paretoSlice = TRUE, pareto_slice_times = 6,
    slice_pareto_alphas = c(2.5, 2.2, 2, 1.8, 1.6, 1.4),
    slice_pareto_x_ms = c(500, 1000, 2000, 4000, 8000, 16000)
  )
  expect_equal(nrow(res), 500)
  expect_error(
    run_simulation(paretoSlice = TRUE, pareto_slice_times = 7, slice_pareto_alphas = rep(2, 7),
                   slice_pareto_x_ms = 1:7 * 1000),
    "from 1 to 6"
  )
})
