#saved settings: the ids, the built-in examples, the plain-text settings files and loading them

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

test_that("the slices in use are set without waiting", {
  entries <- sim_settings_plan(list(pareto_slice_times = 2, slice_pareto_param_1 = 2, slice_pareto_param_2 = 1000,
                                    slice_pareto_param_3 = 1.5, slice_pareto_param_4 = 5000))
  ids <- vapply(entries, `[[`, character(1), "id")
  expect_true(all(c("pareto_slice_times", paste0("slice_pareto_param_", 1:4)) %in% ids))
  #the slice fields are always in the page, so they are set without waiting
  slice_entries <- entries[ids %in% paste0("slice_pareto_param_", 1:4)]
  expect_true(all(vapply(slice_entries, function(e) is.null(e$gate), logical(1))))
})

test_that("only the slices in use are restored, and the later slice fields are cleared", {
  inputs <- list(pareto_slice_times = 1, slice_pareto_param_1 = 2, slice_pareto_param_2 = 1000,
                 slice_pareto_param_3 = 9, slice_pareto_param_4 = 9)
  entries <- sim_settings_plan(inputs)
  ids <- vapply(entries, `[[`, character(1), "id")
  value_of <- function(id) entries[[which(ids == id)]]$value
  expect_equal(value_of("slice_pareto_param_1"), 2)
  expect_equal(value_of("slice_pareto_param_2"), 1000)
  #the saved values of unused slices are not restored: their fields are emptied, so that
  #'Add slice' does not bring back the values of earlier settings
  later <- paste0("slice_pareto_param_", 3:(2 * max_number_of_pareto_slices))
  expect_true(all(later %in% ids))
  expect_true(all(vapply(later, function(id) is.na(value_of(id)), logical(1))))
  expect_true(all(vapply(entries[ids %in% later], function(e) is.null(e$gate) && e$kind == "numeric", logical(1))))

  #no slices: every slice field is emptied
  none <- sim_settings_plan(sim_settings_examples[["Motor: excess of loss layer"]])
  none_ids <- vapply(none, `[[`, character(1), "id")
  slice_ids <- paste0("slice_pareto_param_", seq_len(2 * max_number_of_pareto_slices))
  expect_true(all(slice_ids %in% none_ids))
  expect_true(all(vapply(none[none_ids %in% slice_ids], function(e) is.na(e$value), logical(1))))
})

test_that("the truncation switch is restored once, and switched off by settings without it", {
  entries <- sim_settings_plan(sim_settings_collect(sim_settings_examples[["Simple: truncated Normal"]]))
  ids <- vapply(entries, `[[`, character(1), "id")
  expect_equal(sum(ids == "sevTruncateAtZero"), 1)
  truncate <- entries[[which(ids == "sevTruncateAtZero")]]
  expect_true(truncate$value)
  #the switch is always in the page (shown for the Normal only), so it is set at once
  expect_null(truncate$gate)

  #other severities switch it off, so an earlier Normal's truncation does not come back
  #when the Normal is picked later; a file without the switch does the same
  motor <- sim_settings_plan(sim_settings_examples[["Motor: excess of loss layer"]])
  motor_ids <- vapply(motor, `[[`, character(1), "id")
  expect_equal(sum(motor_ids == "sevTruncateAtZero"), 1)
  expect_false(motor[[which(motor_ids == "sevTruncateAtZero")]]$value)
  bare <- sim_settings_plan(list(sevDistr = "Gamma", shape = 2, scale = 100))
  bare_ids <- vapply(bare, `[[`, character(1), "id")
  expect_false(bare[[which(bare_ids == "sevTruncateAtZero")]]$value)

  #loading the example sets the switch at once rather than queuing it
  shiny::testServer(sim_settings_io_server, {
    session$setInputs(settingsIO_example = "Simple: truncated Normal", settingsIO_load_example = 1)
    queued <- vapply(pending(), `[[`, character(1), "id")
    expect_false("sevTruncateAtZero" %in% queued)
  })
})

test_that("every example round-trips through a plain-text settings file", {
  expect_match(sim_settings_file_name, "\\.txt$")
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  for (name in names(sim_settings_examples)) {
    example <- sim_settings_examples[[name]]
    sim_settings_write(example, file)
    #a text file a person can read, one "id: value" line per setting
    lines <- readLines(file)
    expect_identical(lines[1:2], c("NetSimRSettings: claims simulator", paste("SettingsVersion:", sim_settings_version)),
                     info = name)
    expect_true(paste("freqDistr:", deparse(example$freqDistr)) %in% lines, info = name)
    back <- sim_settings_read(file)
    expect_identical(back$inputs, sim_settings_collect(example), info = name)
    expect_true(isTRUE(sim_settings_validate(back$inputs, back$version)), info = name)
  }
})

#settings different from every example, with fields that only appear for some choices
custom_settings <- list(
  freqDistr = "Negative_Binomial", r = 2.5, beta = 3,
  sevDistr = "Normal", normal_mean = 5000, normal_sd = 2500, sevTruncateAtZero = TRUE,
  numberOfSimulations = 12345, seedSetBinary = TRUE, seedValue = 99, multiprocessingBinary = FALSE,
  pareto_slice_times = 2, slice_pareto_param_1 = 2.2, slice_pareto_param_2 = 20000,
  slice_pareto_param_3 = 1.6, slice_pareto_param_4 = 80000,
  sevCapBinary = TRUE, sev_cap_amount = 1e6,
  reinsuranceStructureEEL = "Limited Layer", reinsurance_structure_eel_dedctible_amount = 10000,
  reinsurance_structure_eel_limit_amount = 50000, reinsuranceStructureLimitedReinstatements = TRUE,
  reinsuranceStructureReinstatementLimit = 3,
  reinsuranceStructureAL = "Exclude Layer", reinsurance_structure_al_dedctible_amount = 1e5,
  reinsurance_structure_al_limit_amount = 2e5
)

test_that("customised settings saved by the app load back into every field", {
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  shiny::testServer(shiny_simulator_server, {
    do.call(session$setInputs, custom_settings)
    file.copy(output$settingsIO_save, file, overwrite = TRUE)
  })
  back <- sim_settings_read(file)
  expect_type(back, "list")
  expect_setequal(names(back$inputs), names(custom_settings))
  for (id in names(custom_settings)) expect_identical(back$inputs[[id]], custom_settings[[id]], info = id)

  #loading it: the controls that are always in the page are set at once, the fields rendered
  #for the chosen options once they appear
  applied <- list()
  notes <- character()
  local_mocked_bindings(
    sim_settings_apply_update = function(session, id, kind, value) applied[[id]] <<- value,
    sim_settings_notify = function(message, type = "message") notes <<- c(notes, paste(type, message))
  )
  shiny::testServer(sim_settings_io_server, {
    session$setInputs(settingsIO_load = list(datapath = file, name = "claims_simulator_settings.txt"))
    queued <- pending()
    restored <- c(applied, stats::setNames(lapply(queued, `[[`, "value"), vapply(queued, `[[`, character(1), "id")))
    for (id in names(custom_settings)) expect_identical(restored[[id]], custom_settings[[id]], info = id)
    #the rendered fields wait for their options
    expect_true(all(c("r", "beta", "normal_mean", "normal_sd", "seedValue", "sev_cap_amount",
                      "reinsuranceStructureReinstatementLimit", "reinsurance_structure_al_limit_amount") %in%
                      vapply(queued, `[[`, character(1), "id")))
  })
  expect_identical(notes, "message Settings loaded from 'claims_simulator_settings.txt'.")
})

test_that("an update the browser lost is sent again, but a value typed instead of it is kept", {
  sent <- list()
  local_mocked_bindings(
    sim_settings_apply_update = function(session, id, kind, value) sent[[length(sent) + 1]] <<- list(id = id, value = value),
    sim_settings_notify = function(message, type = "message") NULL
  )
  seed_sends <- function() sum(vapply(sent, function(s) identical(s$id, "seedValue"), logical(1)))
  shiny::testServer(sim_settings_io_server, {
    #the seed field is on the page with another seed
    session$setInputs(freqDistr = "Poisson", sevDistr = "LogNormal", seedSetBinary = TRUE, seedValue = 77,
                      reinsuranceStructureEEL = "Limited Layer", reinsuranceStructureAL = "No Reinsurance Structure")
    session$setInputs(settingsIO_example = "Motor: excess of loss layer", settingsIO_load_example = 1)
    expect_equal(seed_sends(), 1)
    #the field still has its old value: the browser lost the update, which is sent again
    Sys.sleep(0.6)
    session$elapse(300)
    expect_equal(seed_sends(), 2)
    #the user types a seed before the update arrives: it is kept, and not set again
    session$setInputs(seedValue = 2024)
    expect_false("seedValue" %in% vapply(pending(), `[[`, character(1), "id"))
    Sys.sleep(0.6)
    session$elapse(300)
    expect_equal(seed_sends(), 2)
  })
})

test_that("corrupted files, .rds files and other tools' files are refused with a message", {
  applied <- list()
  notes <- character()
  local_mocked_bindings(
    sim_settings_apply_update = function(session, id, kind, value) applied[[id]] <<- value,
    sim_settings_notify = function(message, type = "message") notes <<- c(notes, paste(type, message))
  )
  load_file <- function(path, name) {
    shiny::testServer(sim_settings_io_server, {
      session$setInputs(settingsIO_load = list(datapath = path, name = name))
      expect_length(pending(), 0)
    })
    notes[length(notes)]
  }
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- function(name) file.path(dir, name)
  sim_settings_write(sim_settings_examples[["Motor: excess of loss layer"]], path("good.txt"))

  #a value that is code is never run, and the file is refused
  lines <- readLines(path("good.txt"))
  writeLines(sub("^lamda: .*$", "lamda: system('echo hacked')", lines), path("corrupted.txt"))
  expect_identical(sim_settings_read(path("corrupted.txt")), "The setting 'lamda' has a value that cannot be read.")
  expect_match(load_file(path("corrupted.txt"), "corrupted.txt"),
               "^error 'corrupted.txt': The setting 'lamda' has a value that cannot be read\\. Please choose a \\.txt file")
  #a truncated file lacks its header
  writeLines(lines[-(1:2)], path("truncated.txt"))
  expect_match(load_file(path("truncated.txt"), "truncated.txt"), "This is not a NetSimR settings file.", fixed = TRUE)
  writeBin(as.raw(c(0x1f, 0x8b, 0:60)), path("binary.txt"))
  expect_match(load_file(path("binary.txt"), "binary.txt"), "This is not a NetSimR settings file.", fixed = TRUE)

  #an .rds settings file as NetSimR 0.3.0 saved it is no longer read
  saveRDS(list(format = "NetSimR simulator settings", version = 2, saved = Sys.time(),
               inputs = sim_settings_examples[["Motor: excess of loss layer"]]), path("old.rds"))
  expect_identical(sim_settings_read(path("old.rds")), "This is not a NetSimR settings file.")
  expect_match(load_file(path("old.rds"), "netsimr_simulator_settings.rds"),
               "'netsimr_simulator_settings.rds': This is not a NetSimR settings file.", fixed = TRUE)

  #the settings of another tool
  write_settings_file(list(response_variable = "claims"), path("glm.txt"), tool = "GLM fitting tool", version = 1)
  expect_match(load_file(path("glm.txt"), "glm.txt"),
               "This settings file is for the GLM fitting tool, not the claims simulator.", fixed = TRUE)

  #values the app does not offer, and newer or older settings versions
  motor <- sim_settings_examples[["Motor: excess of loss layer"]]
  write_settings_file(utils::modifyList(motor, list(sevDistr = "Weibull")), path("unknown.txt"),
                      tool = sim_settings_tool, version = sim_settings_version)
  expect_match(load_file(path("unknown.txt"), "unknown.txt"), "unknown severity distribution: 'Weibull'", fixed = TRUE)
  write_settings_file(utils::modifyList(motor, list(reinsuranceStructureAL = c("Limited Layer", "Exclude Layer"))),
                      path("two.txt"), tool = sim_settings_tool, version = sim_settings_version)
  expect_match(load_file(path("two.txt"), "two.txt"),
               "inputs with more than one value: 'reinsuranceStructureAL'", fixed = TRUE)
  write_settings_file(motor, path("newer.txt"), tool = sim_settings_tool, version = sim_settings_version + 1)
  expect_match(load_file(path("newer.txt"), "newer.txt"), "saved by a newer version of NetSimR", fixed = TRUE)
  write_settings_file(motor, path("older.txt"), tool = sim_settings_tool, version = 2)
  expect_match(load_file(path("older.txt"), "older.txt"), "no longer loads", fixed = TRUE)

  expect_true(all(startsWith(notes, "error ")))
  expect_length(applied, 0)
})

test_that("a repeated line or a vector in a hand-edited file is refused instead of reaching a field", {
  applied <- list()
  notes <- character()
  local_mocked_bindings(
    sim_settings_apply_update = function(session, id, kind, value) applied[[id]] <<- value,
    sim_settings_notify = function(message, type = "message") notes <<- c(notes, paste(type, message))
  )
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  sim_settings_write(sim_settings_examples[["Motor: excess of loss layer"]], file)
  lines <- readLines(file)
  load_file <- function() {
    shiny::testServer(sim_settings_io_server, {
      session$setInputs(settingsIO_load = list(datapath = file, name = "edited.txt"))
      expect_length(pending(), 0)
    })
    notes[length(notes)]
  }

  #a duplicated line used to load c(5, 6) into the lambda field
  writeLines(c(lines, "lamda: 6"), file)
  expect_identical(sim_settings_read(file), "'lamda' is given more than once in the settings file.")
  expect_match(load_file(), "^error 'edited.txt': 'lamda' is given more than once in the settings file\\. Please choose")
  #a vector in a field that holds one value is refused rather than reaching the field
  writeLines(sub("^lamda: .*$", "lamda: c(5, 6)", lines), file)
  settings <- sim_settings_read(file)
  expect_length(settings$inputs$lamda, 2)
  expect_identical(sim_settings_validate(settings$inputs, settings$version),
                   "The settings file holds inputs with more than one value: 'lamda'.")
  expect_match(load_file(), "^error 'edited.txt': The settings file holds inputs with more than one value: 'lamda'\\.$")
  #a value built to be huge (each range is capped, and so is what they are joined into) is refused
  #when the file is read, before the values are built
  writeLines(sub("^lamda: .*$", "lamda: c(1:1000000, 1:1000000)", lines), file)
  expect_identical(sim_settings_read(file), "The setting 'lamda' has a value that cannot be read.")
  expect_match(load_file(), "has a value that cannot be read", fixed = TRUE)
  #a blank line between settings, and a missing version line
  writeLines(append(lines, "", after = 3), file)
  expect_match(load_file(), "more than one block of settings; remove any blank lines", fixed = TRUE)
  writeLines(lines[-2], file)
  expect_match(load_file(), "The settings file has no valid version.", fixed = TRUE)
  expect_length(applied, 0)
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
