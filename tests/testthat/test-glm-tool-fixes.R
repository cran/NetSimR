#the GLM fitting tool: what a downloaded model carries, failed imports, comparing stored models,
#the column choices after an import and the chart device, driven through shiny::testServer()

#insurance-like data: an ID, rating factors, an exposure, a weight, a count and an amount
glm_claims_data <- function(n = 3000) {
  set.seed(21)
  d <- data.frame(policy_id = sprintf("P%05d", seq_len(n)), age = sample(18:80, n, TRUE),
                  region = sample(c("North", "South", "East", "West"), n, TRUE),
                  exposure = round(runif(n, 0.2, 1), 3), weight = sample(1:4, n, TRUE), stringsAsFactors = FALSE)
  d$claim_count <- rpois(n, d$exposure * exp(-2 + 0.01 * d$age))
  d$claim_amount <- ifelse(d$claim_count > 0, round(rgamma(n, 2, 0.001), 2), NA)
  d$age[5] <- NA
  d
}

glm_write_csv <- function(d) {
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  path
}

#the bytes of an RDS file once decompressed
rds_bytes <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  bytes <- raw(0)
  repeat {
    chunk <- readBin(con, "raw", 1e6)
    if (length(chunk) == 0) break
    bytes <- c(bytes, chunk)
  }
  bytes
}

test_that("GLM fitting tool downloads a model without the Shiny session or the password", {
  d <- glm_claims_data()
  path <- glm_write_csv(d)
  password <- "S3cretPW-glm-test"
  #the same model fitted with glm(), with nothing but the model in its file (a formula made here
  #would keep this test's environment)
  reference <- glm(stats::as.formula("claim_count ~ offset(log(exposure)) + age + region", env = globalenv()),
                   family = poisson, data = d, weights = weight, na.action = na.exclude)
  plain <- tempfile(fileext = ".rds")
  saveRDS(reference, plain)
  shiny::testServer(GLMFittingToolServer, {
    #a password typed on the Data tab before a CSV file is imported
    session$setInputs(data_source = "Database", db_type = "PostgreSQL", db_user = "someone", db_password = password)
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "exposure", offset_log = TRUE, weights = "weight", formula = "age + region", fit_model = 1)
    model <- fitted_model()
    expect_s3_class(model, "glm")
    #the file of the Model (RDS) download
    file <- output$download_model
    bytes <- rds_bytes(file)
    expect_length(grepRaw(password, bytes, fixed = TRUE, all = TRUE), 0)
    expect_length(grepRaw("ShinySession", bytes, fixed = TRUE, all = TRUE), 0)
    #nor the server's own variables, which would be saved with its environment
    expect_length(grepRaw("fit_result", bytes, fixed = TRUE, all = TRUE), 0)
    expect_length(grepRaw("selected_data", bytes, fixed = TRUE, all = TRUE), 0)
    #about the size of the same model fitted with glm()
    expect_lt(file.size(file), 5 * file.size(plain))
    expect_lt(length(serialize(model$family, NULL)), 1e5)
    #the saved model predicts as glm() does, keeps a row for each row of the data (NA where
    #age is missing) and shows the real call in its summary
    saved <- readRDS(file)
    newdata <- d[1:50, ]
    expect_equal(predict(saved, newdata, type = "response"), predict(reference, newdata, type = "response"))
    expect_equal(coef(saved), coef(reference))
    expect_length(fitted(saved), nrow(d))
    expect_true(is.na(fitted(saved)[5]))
    call_text <- paste(deparse(saved$call), collapse = "")
    expect_match(call_text, "poisson(link = \"log\")", fixed = TRUE)
    expect_match(call_text, "weights = weight", fixed = TRUE)
    expect_match(paste(capture.output(summary(saved)), collapse = "\n"), "offset(log(exposure))", fixed = TRUE)
    #update() refits it given the data, as data = or as model_data
    expect_equal(coef(update(saved, . ~ . - region, data = d)), coef(update(reference, . ~ . - region)))
    model_data <- d
    expect_equal(coef(update(saved, . ~ . - region)), coef(update(reference, . ~ . - region)))
    #every family: nothing of the session in the model
    responses <- c(gaussian = "age", binomial = "region", Gamma = "claim_amount", inverse.gaussian = "claim_amount")
    for (family in names(responses)) {
      session$setInputs(response_variable = responses[[family]], glm_distribution = family,
                        link_function = glm_family_links[[family]][1], offset = "None", weights = "None",
                        formula = if (family == "binomial") "age" else "region", fit_model = 1 + match(family, names(responses)))
      model <- fitted_model()
      expect_s3_class(model, "glm")
      bytes <- serialize(model, NULL)
      expect_length(grepRaw("ShinySession", bytes, fixed = TRUE, all = TRUE), 0)
      expect_length(grepRaw(password, bytes, fixed = TRUE, all = TRUE), 0)
    }
  })
})

test_that("GLM fitting tool keeps the data it has when an import fails", {
  d <- glm_claims_data(200)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    model <- fitted_model()
    #a database import that fails
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = file.path(tempdir(), "no_such_database.sqlite"),
                      sql_query = "SELECT 1", submit = 2)
    expect_equal(nrow(selected_data()), 200)
    expect_equal(names(selected_data()), names(d))
    expect_match(output$selected_input_data_table$html, "P00001", fixed = TRUE)
    expect_match(output$data_overview$html, "The last import failed", fixed = TRUE)
    expect_match(output$data_overview$html, "claims.csv", fixed = TRUE)
    #the model and its data still go together, and fitting again works
    expect_identical(fitted_model(), model)
    session$setInputs(formula = "age + region", fit_model = 2)
    expect_equal(stats::nobs(fitted_model()), sum(!is.na(d$age)))
    #an import that works replaces the data and the note
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d[1:50, ]), name = "small.csv"), submit = 3)
    expect_equal(nrow(selected_data()), 50)
    expect_no_match(output$data_overview$html, "The last import failed", fixed = TRUE)
  })
  #a failed first import still leaves no data
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", submit = 1)
    expect_null(selected_data())
  })
})

test_that("GLM fitting tool compares the AIC of stored models only with the same weights", {
  d <- glm_claims_data(500)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1, save_formula_1 = 1)
    session$setInputs(weights = "weight", fit_model = 2, save_formula_2 = 1)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_no_match(both, "lower", fixed = TRUE)
    expect_match(output$aic_output_1$html, "Not comparable", fixed = TRUE)
    expect_match(output$aic_output_2$html, "different data, response, observations or weights", fixed = TRUE)
    #the same weights: comparable, and the lower AIC is marked
    session$setInputs(formula = "age + region", fit_model = 3, save_formula_1 = 2)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_match(both, "lower", fixed = TRUE)
    expect_no_match(both, "Not comparable", fixed = TRUE)
  })
})

test_that("GLM fitting tool says when the shown model was fitted to data imported before", {
  d <- glm_claims_data(300)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "a.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    model <- fitted_model()
    expect_false(model_outdated())
    expect_no_match(output$model_stats$html, "imported before", fixed = TRUE)
    expect_match(output$model_downloads$html, "download_data_with_predictions", fixed = TRUE)
    #a failed import keeps the data, so the model is still of the data shown
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = file.path(tempdir(), "no_such_database.sqlite"),
                      sql_query = "SELECT 1", submit = 2)
    expect_false(model_outdated())
    #a new import: the Data tab shows 50 rows while the model is of the 300 imported before, which
    #the tiles say, and the downloads wait for the model to be fitted again
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d[1:50, ]), name = "b.csv"), submit = 3)
    expect_equal(nrow(selected_data()), 50)
    expect_true(model_outdated())
    expect_identical(fitted_model(), model)
    expect_match(output$model_stats$html, "This model was fitted to 'a.csv' (300 rows), imported before the data now on the Data tab ('b.csv', 50 rows). Click Fit model", fixed = TRUE)
    expect_match(output$model_stats$html, "299", fixed = TRUE)
    expect_match(output$model_downloads$html, "fitted to 'a.csv' (300 rows)", fixed = TRUE)
    expect_match(output$model_downloads$html, "Fit it again to enable the downloads", fixed = TRUE)
    expect_no_match(output$model_downloads$html, "download_data_with_predictions", fixed = TRUE)
    #a download asked for anyway (a page left open, a kept link) is refused with an error and
    #no file, rather than served from the old model and its data
    for (id in c("download_model", "download_summary", "download_data_with_predictions")) {
      expect_error(output[[id]], "fitted to data imported before", class = "shiny.silent.error")
    }
    session$setInputs(visualize_variable = "region", number_of_bands_input = 10, execute_visualization = 1)
    expect_match(output$fitness_note$html, "The chart is of that data", fixed = TRUE)
    #fitted again, the model is of the new data, the notes go and the downloads are of the new data
    session$setInputs(fit_model = 2)
    expect_false(model_outdated())
    expect_equal(stats::nobs(fitted_model()), sum(!is.na(d$age[1:50])))
    expect_no_match(output$model_stats$html, "imported before", fixed = TRUE)
    expect_match(output$model_downloads$html, "download_model", fixed = TRUE)
    expect_equal(stats::nobs(readRDS(output$download_model)), sum(!is.na(d$age[1:50])))
    expect_match(paste(readLines(output$download_summary), collapse = "\n"), "Coefficients:", fixed = TRUE)
    predicted <- utils::read.csv(output$download_data_with_predictions)
    expect_equal(nrow(predicted), 50)
    expect_equal(predicted$policy_id, d$policy_id[1:50])
    expect_false(grepl("imported before", paste(output$fitness_note$html, collapse = ""), fixed = TRUE))
  })
  #with no model fitted, the downloads are refused too
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "a.csv"), submit = 1)
    for (id in c("download_model", "download_summary", "download_data_with_predictions")) {
      expect_error(output[[id]], "Fit a model", class = "shiny.silent.error")
    }
  })
})

test_that("GLM fitting tool compares the AIC of stored models only when they are of the same rows of the same import", {
  d <- glm_claims_data(300)
  set.seed(22)
  other <- data.frame(age = sample(18:80, 300, TRUE), region = sample(c("North", "South"), 300, TRUE),
                      claim_count = rpois(300, 5), stringsAsFactors = FALSE)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "a.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "region", fit_model = 1, save_formula_1 = 1)
    #an unrelated file with the same number of rows, the same response name and no weights
    session$setInputs(csv_file = list(datapath = glm_write_csv(other), name = "other.csv"), submit = 2)
    session$setInputs(formula = "region", fit_model = 2, save_formula_2 = 1)
    expect_equal(stored_models[[1]]()$n, stored_models[[2]]()$n)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_no_match(both, "lower", fixed = TRUE)
    expect_match(both, "Not comparable with the other model: different data", fixed = TRUE)
    #the same import, but rows left out for a missing value in one model only: not comparable either
    session$setInputs(csv_file = list(datapath = glm_write_csv(d), name = "a.csv"), submit = 3)
    session$setInputs(formula = "region", fit_model = 3, save_formula_1 = 2)
    session$setInputs(formula = "age", fit_model = 4, save_formula_2 = 2)
    expect_equal(stored_models[[1]]()$n, 300)
    expect_equal(stored_models[[2]]()$n, 299)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_match(both, "Not comparable", fixed = TRUE)
    #the same rows of the same import: comparable
    session$setInputs(formula = "age + region", fit_model = 5, save_formula_1 = 3)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_match(both, "lower", fixed = TRUE)
    expect_no_match(both, "Not comparable", fixed = TRUE)
  })
})

test_that("GLM fitting tool keeps every warning of a binomial fit with more than two response values", {
  set.seed(23)
  n <- 400
  d <- data.frame(y = sample(c("a", "b", "c"), n, TRUE), id = sprintf("P%03d", rep(1:135, length.out = n)),
                  x = rnorm(n), stringsAsFactors = FALSE)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "w.csv"), submit = 1)
    session$setInputs(response_variable = "y", glm_distribution = "binomial", link_function = "logit",
                      offset = "None", weights = "None", formula = "id", fit_model = 1)
    warnings <- fit_result()$warnings
    expect_match(paste(warnings, collapse = " "), "'id' is text with 135 different values", fixed = TRUE)
    expect_match(paste(warnings, collapse = " "), "the response has 3 values: 'a' is failure", fixed = TRUE)
  })
})

test_that("GLM fitting tool's predictions download keeps a prediction column of the data apart from the model's", {
  d <- glm_claims_data(100)
  d$prediction <- round(runif(100), 3)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "p.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    header <- readLines(output$download_data_with_predictions, n = 1)
    expect_identical(header, paste0("\"", paste(c(names(d), "model_prediction"), collapse = "\",\""), "\""))
    downloaded <- utils::read.csv(output$download_data_with_predictions)
    expect_equal(downloaded$prediction, d$prediction)
    expect_equal(downloaded$model_prediction, unname(fitted(fitted_model())))
    #without a clash the column is prediction, as before
    session$setInputs(csv_file = list(datapath = glm_write_csv(d[setdiff(names(d), "prediction")]), name = "q.csv"), submit = 2)
    session$setInputs(fit_model = 2)
    expect_identical(names(utils::read.csv(output$download_data_with_predictions)), c(setdiff(names(d), "prediction"), "prediction"))
  })
})

test_that("GLM fitting tool fits with the link of the family chosen, not the link the browser has yet to echo", {
  d <- glm_claims_data(200)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "gaussian", link_function = "identity",
                      offset = "None", weights = "None", formula = "age")
    #the family changes and Fit is clicked before the link select reports the new family's default
    session$setInputs(glm_distribution = "poisson")
    session$setInputs(fit_model = 1)
    expect_identical(fitted_model()$family$link, "log")
    expect_identical(fit_result()$spec$link, "log")
    #the echo arrives; a link chosen by hand is used
    session$setInputs(link_function = "log")
    session$setInputs(link_function = "sqrt", fit_model = 2)
    expect_identical(fitted_model()$family$link, "sqrt")
    #a stored model is loaded and fitted before its family and link are echoed
    session$setInputs(save_formula_1 = 1)
    session$setInputs(glm_distribution = "gaussian")
    session$setInputs(link_function = "identity", fit_model = 3)
    expect_identical(fitted_model()$family$link, "identity")
    session$setInputs(load_formula_1 = 1)
    expect_identical(chosen_link(), "sqrt")
    #the family select is echoed first, then the link
    session$setInputs(glm_distribution = "poisson")
    session$setInputs(fit_model = 4)
    expect_identical(fitted_model()$family$link, "sqrt")
  })
})

test_that("GLM fitting tool fits with the family a load selects, not the one the browser has yet to echo", {
  d <- glm_claims_data(200)
  settings <- tempfile(fileext = ".txt")
  write_settings_file(list(glm_distribution = "poisson"), settings, glm_settings_tool, glm_settings_version)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "sqrt",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    expect_identical(fitted_model()$family$family, "poisson")
    session$setInputs(save_formula_1 = 1)
    session$setInputs(glm_distribution = "gaussian")
    session$setInputs(link_function = "identity", fit_model = 2)
    expect_identical(fitted_model()$family$family, "gaussian")
    #the stored Poisson model is loaded and Fit clicked before the browser echoes anything: the
    #family was read from the browser, so the fit was a Gaussian model with the stored link
    session$setInputs(load_formula_1 = 1)
    session$setInputs(fit_model = 3)
    expect_identical(fitted_model()$family$family, "poisson")
    expect_identical(fitted_model()$family$link, "sqrt")
    expect_identical(fit_result()$spec$family, "poisson")
    #a settings file with a new family and no link: the family's default link, as the browser will show
    session$setInputs(glm_distribution = "gaussian")
    session$setInputs(link_function = "identity", fit_model = 4)
    expect_identical(fitted_model()$family$family, "gaussian")
    session$setInputs(load_config = list(datapath = settings, name = "settings.txt"))
    session$setInputs(fit_model = 5)
    expect_identical(fitted_model()$family$family, "poisson")
    expect_identical(fitted_model()$family$link, "log")
    #a family the user picks is used as before
    session$setInputs(glm_distribution = "gaussian")
    session$setInputs(link_function = "identity", fit_model = 6)
    expect_identical(fitted_model()$family$family, "gaussian")
  })
})

test_that("GLM fitting tool fits a loaded model whole, not with the response and formula shown before", {
  d <- glm_claims_data(300)
  d$has_claim <- as.integer(d$claim_count > 0)
  file_path <- tempfile(fileext = ".txt")
  write_settings_file(list(glm_distribution = "poisson", link_function = "log", offset = "exposure", offset_log = TRUE,
                           formula = "age + region"), file_path, glm_settings_tool, glm_settings_version)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    #a stored binomial model of has_claim, then a Gaussian model of claim_count on the screen
    session$setInputs(response_variable = "has_claim", glm_distribution = "binomial", link_function = "probit",
                      offset = "None", offset_log = FALSE, weights = "None", formula = "age + region", fit_model = 1)
    expect_identical(fitted_model()$family$family, "binomial")
    session$setInputs(save_formula_1 = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "gaussian")
    session$setInputs(link_function = "identity", formula = "age", fit_model = 2)
    expect_identical(fit_result()$spec$response, "claim_count")
    #loaded and fitted before the browser reports anything: the fit took the binomial family with the
    #count response on screen before the load, and failed ("y values must be 0 <= y <= 1")
    session$setInputs(load_formula_1 = 1)
    session$setInputs(fit_model = 3)
    expect_s3_class(fitted_model(), "glm")
    spec <- fit_result()$spec
    expect_identical(spec[c("response", "family", "link", "formula")],
                     list(response = "has_claim", family = "binomial", link = "probit", formula = "age + region"))
    #the browser reports the values; a fit then gives the same model
    session$setInputs(response_variable = "has_claim", glm_distribution = "binomial")
    session$setInputs(link_function = "probit", formula = "age + region", fit_model = 4)
    expect_identical(fit_result()$spec[c("response", "family", "link", "formula")], spec[c("response", "family", "link", "formula")])
    #a settings file with an offset and a formula, fitted before the browser reports them
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson")
    session$setInputs(link_function = "sqrt", formula = "age", fit_model = 5)
    session$setInputs(load_config = list(datapath = file_path, name = "settings.txt"))
    session$setInputs(fit_model = 6)
    spec <- fit_result()$spec
    expect_identical(spec[c("family", "link", "offset", "offset_log", "formula")],
                     list(family = "poisson", link = "log", offset = "exposure", offset_log = TRUE, formula = "age + region"))
    expect_identical(names(coef(fitted_model())), c("(Intercept)", "age", "regionNorth", "regionSouth", "regionWest"))
    #values sent together with the click of Fit model are the values fitted
    session$setInputs(offset = "None", offset_log = FALSE, formula = "age", fit_model = 7)
    expect_identical(fit_result()$spec[c("offset", "formula")], list(offset = "None", formula = "age"))
  })
})

test_that("GLM fitting tool's settings loads keep the family and link fitted and shown in step", {
  d <- glm_claims_data(200)
  settings <- function(...) {
    path <- tempfile(fileext = ".txt")
    write_settings_file(list(...), path, glm_settings_tool, glm_settings_version)
    list(datapath = path, name = "settings.txt")
  }
  family_only <- settings(glm_distribution = "poisson")
  wrong_link <- settings(glm_distribution = "poisson", link_function = "logit")
  shiny::testServer(GLMFittingToolServer, {
    notes <- list()
    session$sendNotification <- function(type, message) notes[[length(notes) + 1]] <<- message
    #what the link select is told to show
    link_shown <- NULL
    session$sendInputMessage <- function(inputId, message) {
      if (identical(inputId, "link_function") && !is.null(message$value)) link_shown <<- message$value
    }
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "sqrt",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    session$setInputs(save_formula_1 = 1)
    session$setInputs(glm_distribution = "gaussian")
    session$setInputs(link_function = "identity", fit_model = 2)
    #a stored Poisson/sqrt model, then a file naming only the Poisson family, before the browser
    #reports either: the file set the default link to be fitted, while the link left pending by
    #the stored model was the one the screen selected when the family was reported
    session$setInputs(load_formula_1 = 1)
    session$setInputs(load_config = family_only)
    session$setInputs(fit_model = 3)
    expect_identical(fitted_model()$family$family, "poisson")
    expect_identical(fitted_model()$family$link, "sqrt")
    expect_identical(link_shown, "sqrt")
    session$setInputs(glm_distribution = "poisson")
    session$setInputs(fit_model = 4)
    expect_identical(fitted_model()$family$link, "sqrt")
    expect_identical(link_shown, "sqrt")
    #a link the family does not offer left the link select blank while the fit used the default
    session$setInputs(link_function = "log", fit_model = 5)
    notes <- list()
    session$setInputs(load_config = wrong_link)
    session$setInputs(fit_model = 6)
    expect_identical(fitted_model()$family$family, "poisson")
    expect_identical(fitted_model()$family$link, "log")
    expect_identical(link_shown, "log")
    expect_match(as.character(notes[[1]]$html), "The poisson family has no 'logit' link, so its default, 'log', is used.",
                 fixed = TRUE)
  })
})

test_that("GLM fitting tool keeps the number of bands within the slider's range", {
  expect_equal(glm_band_count(1e9), 50)
  expect_equal(glm_band_count(-3), 2)
  expect_equal(glm_band_count(7.4), 7)
  expect_equal(glm_band_count(NULL), 10)
  expect_equal(glm_band_count("ten"), 10)
  expect_equal(glm_band_count(Inf), 10)
  expect_identical(glm_settings_values(list(number_of_bands_input = 1e9))$number_of_bands_input, 50)
  expect_identical(glm_settings_values(list(number_of_bands_input = 7))$number_of_bands_input, 7)
  expect_identical(glm_settings_values(list(number_of_bands_input = 1))$number_of_bands_input, 2)
  d <- glm_claims_data(200)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    #a client that sends a number the slider cannot: the chart has at most 50 bands
    session$setInputs(visualize_variable = "exposure", number_of_bands_input = 1e9, band_method = "quantile", execute_visualization = 1)
    expect_lte(nrow(fitness_data()), 50)
    expect_gt(nrow(fitness_data()), 40)
  })
})

test_that("GLM fitting tool chooses a response and a chart variable that suit the data", {
  d <- glm_claims_data(300)
  #the counts for the default families, the amounts for the Gamma family; never the ID or the age
  expect_equal(glm_default_response(d, "gaussian"), "claim_count")
  expect_equal(glm_default_response(d, "poisson"), "claim_count")
  expect_equal(glm_default_response(d, "Gamma"), "claim_amount")
  choices <- glm_column_choices(d, "poisson")
  expect_equal(choices, list(response_variable = "claim_count", offset = "None", weights = "None", visualize_variable = "age"))
  #a numeric ID, in any order, is not the chart variable either
  d_numeric <- d
  d_numeric$policy_id <- sample(100001:100300)
  expect_equal(glm_column_choices(d_numeric, "poisson")$visualize_variable, "age")
  #the words of the name, whatever the case and separators; not a word that only contains one
  expect_equal(glm_default_response(data.frame(account_type = 1:3 %% 2, ClaimCount = c(0, 1, 0), x = c(2.5, 1, 3)), "poisson"), "ClaimCount")
  expect_equal(glm_default_response(data.frame(a = rnorm(5), Y = rnorm(5), b = rnorm(5)), "gaussian"), "Y")
  #no name suggests a response: the last numeric column that is not an ID
  plain <- data.frame(id = 1:40, grp = rep(c("a", "b"), 20), x = rnorm(40), z = rnorm(40), row = 40:1)
  expect_equal(glm_default_response(plain, "gaussian"), "z")
  expect_equal(glm_default_variable(plain, "z"), "grp")
  #text with too many values to draw is skipped, and a column already chosen is not the variable
  many <- data.frame(postcode = rep(sprintf("PC%03d", 1:150), 4), weight = 1, age = sample(18:80, 600, TRUE), claims = rpois(600, 1))
  expect_equal(glm_column_choices(many, "poisson", list(weights = "weight"))$visualize_variable, "age")
  #the choices already made are kept when the data has the column
  kept <- glm_column_choices(d, "poisson", list(response_variable = "age", offset = "exposure", visualize_variable = "region"))
  expect_equal(kept, list(response_variable = "age", offset = "exposure", weights = "None", visualize_variable = "region"))
  expect_equal(glm_column_choices(d, "poisson", list(response_variable = "not_a_column"))$response_variable, "claim_count")
  #a text response only for the binomial family
  yes_no <- data.frame(claim = sample(c("yes", "no"), 30, TRUE), age = rnorm(30))
  expect_equal(glm_default_response(yes_no, "binomial"), "claim")
  expect_equal(glm_default_response(yes_no, "poisson"), "age")
  #what counts as an ID
  expect_true(glm_id_like(sprintf("P%03d", 1:100)))
  expect_true(glm_id_like(sample(1:100)))
  expect_false(glm_id_like(sample(18:80, 100, TRUE)))
  expect_false(glm_id_like(round(rnorm(100, 5000, 2000), 2)))
  expect_false(glm_id_like(sample(1e6, 100)))
  expect_false(glm_id_like(rep(c("a", "b"), 50)))
  expect_false(glm_id_like(c(1, Inf, 3)))
  #the server applies them after an import without failing
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    expect_equal(nrow(selected_data()), 300)
  })
})

test_that("GLM fitting tool draws its chart with the tools' plot device and redraws it on resize", {
  expect_false(any(grepl("(^|[^_[:alnum:]])renderPlot\\(", deparse(GLMFittingToolServer))))
  d <- glm_claims_data(300)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    session$setInputs(visualize_variable = "region", number_of_bands_input = 10, execute_visualization = 1,
                      .clientdata_output_fitness_plot_width = 900, .clientdata_output_fitness_plot_height = 520)
    wide <- output$fitness_plot
    expect_match(wide$src, "^data:image/png;base64,")
    expect_match(wide$alt, "region")
    session$setInputs(app_theme = "dark", .clientdata_output_fitness_plot_width = 400)
    narrow <- output$fitness_plot
    expect_match(narrow$src, "^data:image/png;base64,")
    expect_false(identical(narrow$src, wide$src))
  })
})
