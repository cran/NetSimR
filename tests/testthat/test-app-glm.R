#Browser test of the GLM fitting tool (helpers in helper-app.R)

test_that("the GLM tool's model download holds no password or session, and its settings are text", {
  skip_if_no_app_browser()
  password <- "E2e-Secret-Pw-4711"
  user <- "e2e-database-user"
  host <- "db.e2e.internal"
  csv <- app_claims_csv()

  app <- start_netsimr_app("glm", "glm-poisson")
  on.exit(app$stop(), add = TRUE)

  #database details typed on the Data tab, then a CSV file imported instead
  app_goto(app, "data")
  app_set(app, data_source = "Database")
  app_set(app, db_type = "PostgreSQL")
  app_set(app, db_host = host, db_user = user, db_password = password)
  app_set(app, data_source = "CSV File")
  app_upload(app, "csv_file", csv)
  app_click(app, "submit")
  app$wait_for_js("/2,000/.test((document.getElementById('data_overview') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)

  #a Poisson model with the log of the exposure as the offset
  app_goto(app, "model")
  app_set(app, response_variable = "claim_count")
  app_set(app, glm_distribution = "poisson")
  app_wait_for_inputs(app, list(link_function = "log"), "the log link of the Poisson family")
  app_set(app, offset = "exposure", formula = "age + region")
  app_click(app, "fit_model")
  app$wait_for_js("/AIC/.test((document.getElementById('model_stats') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)
  #(the tiles' labels are in capitals, which innerText keeps)
  expect_match(tolower(app_text(app, "#model_stats")), "observations 2,000", fixed = TRUE)

  #the downloaded model is the model, without the session and so without the password
  model_file <- app_download(app, "download_model")
  expect_identical(basename(model_file), "glm_model.rds")
  bytes <- app_file_bytes(model_file)
  expect_gt(length(bytes), 1000)
  expect_false(app_bytes_contain(bytes, password))
  expect_false(app_bytes_contain(bytes, "ShinySession"))
  model <- readRDS(model_file)
  expect_s3_class(model, "glm")
  data <- utils::read.csv(csv)
  reference <- stats::glm(claim_count ~ offset(log(exposure)) + age + region, family = stats::poisson(), data = data)
  expect_equal(stats::coef(model), stats::coef(reference), tolerance = 1e-8)

  #settings are saved as text, without the connection details unless the box is ticked,
  #and never with the password
  app_goto(app, "settings")
  plain_file <- app_download(app, "DownloadDataHandlerConf")
  expect_match(basename(plain_file), "[.]txt$")
  plain_text <- readLines(plain_file, warn = FALSE)
  expect_identical(plain_text[1], "NetSimRSettings: GLM fitting tool")
  plain <- read.dcf(plain_file, all = TRUE)
  expect_identical(plain$glm_distribution, "\"poisson\"")
  expect_identical(plain$formula, "\"age + region\"")
  expect_false(any(c("db_host", "db_port", "db_name", "db_user", "sql_query", "db_password") %in% names(plain)))
  for (secret in c(password, user, host)) expect_false(any(grepl(secret, plain_text, fixed = TRUE)))

  app_set(app, settings_include_db = TRUE)
  full_file <- app_download(app, "DownloadDataHandlerConf", filename = file.path(tempdir(), "glm_settings_with_connection.txt"))
  full_text <- readLines(full_file, warn = FALSE)
  full <- read.dcf(full_file, all = TRUE)
  expect_true(all(c("db_host", "db_user", "sql_query") %in% names(full)))
  expect_identical(full$db_user, paste0("\"", user, "\""))
  expect_identical(full$db_host, paste0("\"", host, "\""))
  expect_false("db_password" %in% names(full))
  expect_false(any(grepl(password, full_text, fixed = TRUE)))

  expect_app_logs_clean(app)
})
