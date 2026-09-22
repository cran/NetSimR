#plain-text settings files of the Shiny tools

test_that("settings round-trip through the text file with their types", {
  values <- list(
    count = 5000, whole = 3L, seed = -12L, text = "Poisson", flag = TRUE, missing = NA,
    missing_text = NA_character_, none = NULL, cap = Inf, negative = -2.5, tiny = 1e-300,
    huge = 1.7e308, thresholds = c(1000, 5000, 25000), ids = 1:4, named = c(mean = 3, sd = 1.5),
    query = "SELECT a, b\nFROM claims -- comment: with 'quotes' and \"double\" quotes",
    commas = "a, b; c: d", accented = "café"
  )
  file <- tempfile(fileext = ".dcf")
  on.exit(unlink(file), add = TRUE)
  write_settings_file(values, file, tool = "test tool", version = 3)
  back <- read_settings_file(file, tool = "test tool")
  expect_identical(back$tool, "test tool")
  expect_identical(back$version, 3)
  expect_identical(names(back$values), names(values))
  for (id in setdiff(names(values), "tiny")) expect_identical(back$values[[id]], values[[id]], info = id)
  #the file holds the text 1e-300 exactly, but R's conversion of text to a double loses some
  #precision at extreme exponents on platforms without long double (macOS arm64: 9.999999985e-301)
  expect_equal(back$values$tiny, values$tiny, tolerance = 1e-8)
  #the file is plain text a person can read
  expect_true(any(grepl("^thresholds: c\\(1000, 5000, 25000\\)$", readLines(file))))
})

test_that("doubles round-trip exactly, and typed values keep their short form", {
  #15 significant digits (deparse's default) wrote a third as 0.333333333333333, which read
  #back 3.3e-16 off, so a seeded run loaded from its own file did not reproduce
  values <- list(third = 1/3, big = 1234567890.1234567, sum = 0.1 + 0.2, typed = 0.3, count = 5000,
                 pair = c(0.1, 1/3), whole = 3L)
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  write_settings_file(values, file, tool = "test tool", version = 3)
  back <- read_settings_file(file, tool = "test tool")$values
  for (id in names(values)) expect_identical(back[[id]], values[[id]], info = id)
  lines <- readLines(file)
  #only the values that need them are written with 17 digits
  expect_true("typed: 0.3" %in% lines)
  expect_true("count: 5000" %in% lines)
  expect_true("whole: 3L" %in% lines)
  expect_true("third: 0.33333333333333331" %in% lines)
  expect_true("sum: 0.30000000000000004" %in% lines)

  #a seeded run gives the same totals from the loaded parameter as from the typed one
  run <- function(sigma) {
    simulate_function(numOfSimulations = 2000, freq_params = 3, sev_params = c(6, sigma), seedValue = 1,
                      freqDistr = "Poisson", sevDistr = "LogNormal")$total_claims
  }
  expect_identical(run(back$third), run(1/3))
})

test_that("hand-edited files with a repeated setting, a blank line or no version are refused with a message", {
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  #a repeated setting read as a list of both values, which passed as a length-two number
  writeLines(c("NetSimRSettings: test tool", "SettingsVersion: 3", "lamda: 5", "lamda: 6"), file)
  expect_error(read_settings_file(file, tool = "test tool"), "'lamda' is given more than once", fixed = TRUE)
  #a blank line makes read.dcf() see two records; the file used to be "not a NetSimR settings file"
  writeLines(c("NetSimRSettings: test tool", "SettingsVersion: 3", "lamda: 5", "", "mu: 2"), file)
  expect_error(read_settings_file(file, tool = "test tool"), "more than one block of settings; remove any blank lines", fixed = TRUE)
  #a missing version line gave "subscript out of bounds"
  writeLines(c("NetSimRSettings: test tool", "lamda: 5"), file)
  expect_error(read_settings_file(file, tool = "test tool"), "no valid version", fixed = TRUE)
  #an unchanged file still reads
  writeLines(c("NetSimRSettings: test tool", "SettingsVersion: 3", "lamda: 5", "mu: 2"), file)
  expect_identical(read_settings_file(file, tool = "test tool")$values, list(lamda = 5, mu = 2))
})

test_that("reading a settings file never evaluates code", {
  file <- tempfile(fileext = ".dcf")
  on.exit(unlink(file), add = TRUE)
  marker <- tempfile()
  attacks <- c(
    paste0("file.create('", gsub("\\\\", "/", marker), "')"),
    "system('echo hacked')", "(function() 1)()", "get('Sys.time')()", "c(1, stop('x'))",
    "quote(x)", "list(1, 2)", "`c`(Sys.time())", "1:1e9"
  )
  for (attack in attacks) {
    writeLines(c("NetSimRSettings: test tool", "SettingsVersion: 1", paste0("value: ", attack)), file)
    expect_error(read_settings_file(file, tool = "test tool"), "cannot be read", info = attack)
  }
  expect_false(file.exists(marker))
})

test_that("files that are not settings files of the tool are refused with a message", {
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  #an .rds file, as the tools saved before
  saveRDS(list(version = 2, inputs = list(a = 1)), file)
  expect_error(read_settings_file(file, tool = "test tool"), "not a NetSimR settings file")
  writeLines("just some text", file)
  expect_error(read_settings_file(file, tool = "test tool"), "not a NetSimR settings file")
  write_settings_file(list(a = 1), file, tool = "other tool", version = 1)
  expect_error(read_settings_file(file, tool = "test tool"), "for the other tool, not the test tool")
  writeLines(c("NetSimRSettings: test tool", "SettingsVersion: x"), file)
  expect_error(read_settings_file(file, tool = "test tool"), "no valid version")
  expect_error(write_settings_file(list(`bad name` = 1), file, "test tool", 1), "names")
  expect_error(write_settings_file(list(a = list(1)), file, "test tool", 1), "vectors")
})
