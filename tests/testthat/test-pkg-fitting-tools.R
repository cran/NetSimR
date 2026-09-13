#the distribution fitting and GLM fitting tools, driven through shiny::testServer()

#the servers call functions the package imports with importFrom(): run on the installed package they are
#found in its namespace, but run on the R/ files sourced into an environment they are not, so attach them
if (!exists("renderPlotly", mode = "function")) {
  for (pkg in c("plotly", "fitdistrplus")) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

write_claims_csv <- function() {
  set.seed(10)
  d <- data.frame(counts = rpois(300, 3), w = sample(1:4, 300, TRUE), sev = round(exp(rnorm(300, 7, 1.2))), txt = "x")
  d$sev[1:3] <- c(-1, 0, NA)
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  list(path = path, data = d)
}

write_csv <- function(d) {
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  path
}

upload <- function(session, path) {
  session$setInputs(file1 = list(datapath = path, name = basename(path)), data_includes_header = TRUE, sep = ",", quote = intToUtf8(34), dec = ".")
}

test_that("distribution fitting tool fits frequency and severity distributions", {
  csv <- write_claims_csv()
  shiny::testServer(distribution_fitting_tool_Server, {
    upload(session, csv$path)
    expect_equal(nrow(data()), 300)
    expect_equal(numeric_columns(), c("counts", "w", "sev"))
    #frequency: the Poisson MLE is the mean, the summary outputs render
    session$setInputs(counts_var = "counts", counts_weighted_var = FALSE, counts_weights_var = "w", FreqDistri = "Poisson", count_hist_bins = 20, execute_freq_analysis = 1)
    expect_equal(counts_data(), csv$data$counts)
    expect_equal(unname(freq_po_fit()$estimate), mean(csv$data$counts))
    expect_match(output$selected_freq_params$html, "Mean")
    expect_match(output$selected_freq_params$html, "Beta")
    expect_match(output$freq_stats$html, "Poisson|Negative Binomial")
    expect_equal(freq_moments()$variance, var(csv$data$counts))
    expect_true(is.character(output$freq_fit_plot) || is.list(output$freq_fit_plot))
    expect_true(is.character(output$count_hist) || is.list(output$count_hist))
    #severity: negative, zero and missing values are dropped and the fits use the cleaned data
    session$setInputs(severity_var = "sev", severity_hist_bins = 20, sev_fit_log_scale = FALSE, execute_sev_analysis = 1)
    cleaned <- csv$data$sev[!is.na(csv$data$sev) & csv$data$sev > 0]
    expect_equal(severity_data(), cleaned)
    expect_equal(sev_input()$dropped, 3)
    expect_equal(sev_gamma_fit()$estimate, fit_gamma_mle(cleaned)$estimate)
    expect_equal(sev_pareto_alpha(), length(cleaned) / sum(log(cleaned / min(cleaned))))
    #the lognormal fit is the maximum likelihood fit fitdistrplus gives
    expect_equal(unname(sev_lnorm_fit()$estimate), unname(fitdistrplus::fitdist(cleaned, "lnorm")$estimate))
    expect_match(output$sev_param_summary$html, "LogNormal")
    expect_match(output$sev_param_summary$html, "Gamma")
    expect_true(is.character(output$sev_fit_plot) || is.list(output$sev_fit_plot))
    session$setInputs(sev_fit_log_scale = TRUE)
    expect_true(is.character(output$sev_fit_plot) || is.list(output$sev_fit_plot))
    #the fitted Pareto cdf is zero below the smallest claim, never negative
    pareto <- sev_models()[[5]]
    expect_equal(pareto$cdf(c(0, min(cleaned) / 2)), c(0, 0))
  })
})

test_that("distribution fitting tool pairs each count with the weight of its own row", {
  d <- data.frame(counts = c(1, NA, 3, 4, 2, 5), w = c(2, 5, NA, 1, 3, -1))
  shiny::testServer(distribution_fitting_tool_Server, {
    upload(session, write_csv(d))
    session$setInputs(counts_var = "counts", counts_weighted_var = TRUE, counts_weights_var = "w", execute_freq_analysis = 1)
    #rows 2, 3 and 6 have a missing or negative count or weight
    expect_equal(counts_data(), c(1, 4, 2))
    expect_equal(weights_data(), c(2, 1, 3))
    expect_equal(freq_input()$dropped, 3)
    #with weights fitdistrplus optimises numerically, so the weighted mean is matched closely, not exactly
    expect_equal(unname(freq_po_fit()$estimate), sum(c(1, 4, 2) * c(2, 1, 3)) / 6, tolerance = 1e-6)
    #the weighted fit is fixed when the analysis runs, not when the switch changes
    session$setInputs(counts_weighted_var = FALSE)
    expect_equal(weights_data(), c(2, 1, 3))
  })
})

test_that("distribution fitting tool explains unusable columns instead of failing", {
  d <- data.frame(frac = c(0.5, 1.5, 2, 3), txt = c("a", "b", "c", "d"), same = 7, small = c(0.02, 0.4, 0.13, 0.9))
  shiny::testServer(distribution_fitting_tool_Server, {
    upload(session, write_csv(d))
    session$setInputs(counts_var = "frac", counts_weighted_var = FALSE, execute_freq_analysis = 1)
    expect_error(counts_data(), "whole numbers")
    session$setInputs(counts_var = "txt", execute_freq_analysis = 2)
    expect_error(counts_data(), "at least two")
    session$setInputs(severity_var = "same", execute_sev_analysis = 1)
    expect_error(severity_data(), "are the same")
    #claims below 1: the gamma fit falls back to rescaled claims and still fits
    session$setInputs(severity_var = "small", execute_sev_analysis = 2)
    expect_true(all(is.finite(sev_gamma_fit()$estimate)))
    expect_match(output$sev_param_summary$html, "Gamma")
  })
})

test_that("distribution fitting tool sliced and piecewise Pareto analyses run", {
  csv <- write_claims_csv()
  shiny::testServer(distribution_fitting_tool_Server, {
    upload(session, csv$path)
    session$setInputs(sliced_sev_var = "sev", sev_cens_fit_log_scale = FALSE, execute_sliced_sev_analysis = 1)
    session$setInputs(slicing_point_left = 2000, slicing_point_right = 8000)
    cleaned <- sliced_sev_data()
    #the Pareto alphas above each slicing point are the usual MLEs
    above <- cleaned[cleaned > 2000]
    expect_equal(paretoX1Alpha(), length(above) / sum(log(above / 2000)))
    expect_true(is.numeric(paretoX2AlphaMod()) && is.finite(paretoX2AlphaMod()))
    expect_true(is.character(output$mean_excess_func_plot) || is.list(output$mean_excess_func_plot))
    expect_true(is.character(output$sliced_sev_cdf_plot) || is.list(output$sliced_sev_cdf_plot))
    expect_match(output$slc_sev_fitted_param_summary$html, "Tail above the second point")
    #the spliced cdfs are continuous at the slicing points
    cdfs <- sliced_cdfs()
    expect_equal(cdfs$one(2000 * (1 + 1e-9)), cdfs$one(2000), tolerance = 1e-6)
    expect_equal(cdfs$two(8000 * (1 + 1e-9)), cdfs$two(8000), tolerance = 1e-6)
    #slicing points in the wrong order give a message, not a fit
    session$setInputs(slicing_point_left = 8000, slicing_point_right = 2000)
    expect_error(slicing_points(), "above the first")
    #piecewise Pareto with three sliders
    session$setInputs(piecewise_pareto_var = "sev", num_pareto_slices = 3, piecewise_pareto_fit_log_scale = FALSE, execute_piecewise_sev_analysis = 1)
    expect_match(as.character(dynamic_sliders()), "slider_3")
    session$setInputs(slider_1 = 1500, slider_2 = 4000, slider_3 = 12000)
    #the piecewise fit works on the sorted claims
    expect_equal(piecewise_sev_data(), sort(cleaned))
    expect_equal(piecwise_pareto_mu(), c(min(cleaned), 1500, 4000, 12000))
    expect_equal(piecwise_pareto_alpha(), piecewise_pareto_alpha(sort(cleaned), c(min(cleaned), 1500, 4000, 12000)))
    expect_equal(predicted_piecwise_cdf(), piecewise_pareto_cdf(sort(cleaned), piecwise_pareto_mu(), piecwise_pareto_alpha()))
    expect_match(output$piecewise_pareto_ks_test$html, "K-S distance")
    expect_true(is.character(output$piecewise_pareto_cdf_plot) || is.list(output$piecewise_pareto_cdf_plot))
    #a threshold already set is kept when the number of thresholds changes
    session$setInputs(num_pareto_slices = 4)
    expect_match(as.character(dynamic_sliders()), "data-from=\"1500\"")
    session$setInputs(num_pareto_slices = 3)
    #sliders that are not increasing give NaN alphas (with a warning) and a message instead of the table
    suppressWarnings({
      session$setInputs(slider_1 = 1500, slider_2 = 1500, slider_3 = 12000)
      alphas <- piecwise_pareto_alpha()
    })
    expect_true(all(is.nan(alphas)))
    expect_error(piecewise_ready(), "different")
    session$setInputs(num_pareto_slices = 1, slider_1 = 3000)
    expect_equal(piecwise_pareto_mu(), c(min(cleaned), 3000))
    expect_length(piecwise_pareto_alpha(), 2)
  })
})

test_that("distribution fitting tool fits a lognormal body to claims below 1", {
  set.seed(3)
  d <- data.frame(sev = exp(rnorm(500, -1, 0.8)))
  shiny::testServer(distribution_fitting_tool_Server, {
    upload(session, write_csv(d))
    session$setInputs(sliced_sev_var = "sev", execute_sliced_sev_analysis = 1)
    session$setInputs(slicing_point_left = 1, slicing_point_right = 2)
    #the body's meanlog is negative; it used to be held at 0.0001
    expect_lt(slc_sev_censored_lnorm_fit()$par[1], -0.5)
  })
})

write_glm_csv <- function() {
  set.seed(2)
  d <- data.frame(y = rpois(200, 3), x1 = rnorm(200), x2 = runif(200), w = sample(1:5, 200, TRUE), e = runif(200, 0.5, 2))
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  list(path = path, data = d)
}

test_that("GLM fitting tool fits the same model as glm()", {
  csv <- write_glm_csv()
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = csv$path, name = "glm.csv"), submit = 1)
    expect_equal(nrow(selected_data()), 200)
    #the offset column used as it is
    session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", offset = "e", offset_log = FALSE, weights = "w", formula = "x1 + x2", fit_model = 1)
    reference <- glm(y ~ offset(e) + x1 + x2, data = csv$data, weights = csv$data$w, family = poisson(link = "log"))
    expect_equal(coef(fitted_model()), coef(reference))
    expect_equal(AIC(fitted_model()), AIC(reference))
    expect_match(output$model_summary, "poisson")
    expect_match(output$coefficients_table$html, "Relativity")
    expect_match(output$model_stats$html, "AIC")
    #the stored model keeps the AIC of the model it was fitted with
    session$setInputs(save_formula_1 = 1)
    expect_match(output$aic_output_1$html, dft_fmt(AIC(reference), 7), fixed = TRUE)
    session$setInputs(visualize_variable = "x1", number_of_bands_input = 10, execute_visualization = 1)
    expect_true(is.character(output$fitness_plot) || is.list(output$fitness_plot))
    #the offset as the log of an exposure column
    session$setInputs(offset_log = TRUE, fit_model = 2)
    expect_equal(coef(fitted_model()), coef(glm(y ~ offset(log(e)) + x1 + x2, data = csv$data, weights = csv$data$w, family = poisson)))
    #with an exposure offset and weights, the chart compares rates: sum(w * y) / sum(w * e) per band
    session$setInputs(visualize_variable = "x2", number_of_bands_input = 2, band_method = "width", execute_visualization = 2)
    band <- cut(csv$data$x2, breaks = 2, include.lowest = TRUE, dig.lab = 6)
    expect_equal(fitness_data()$actual,
                 as.numeric(tapply(csv$data$w * csv$data$y, band, sum) / tapply(csv$data$w * csv$data$e, band, sum)))
    #a second stored model; the model with the lower AIC is marked
    session$setInputs(formula = "x1", offset_log = TRUE, fit_model = 3, save_formula_2 = 1)
    expect_match(output$aic_output_2$html, "AIC")
    expect_match(paste(output$aic_output_1$html, output$aic_output_2$html), "lower")
    #loading a stored model does not fail (the inputs it sets are not simulated by testServer)
    session$setInputs(load_formula_1 = 1)
    #another family and link, without offset and weights (e is positive, as the Gamma family requires)
    session$setInputs(response_variable = "e", glm_distribution = "Gamma", link_function = "inverse", offset = "None", weights = "None", formula = "x1", fit_model = 4)
    expect_equal(coef(fitted_model()), coef(glm(e ~ x1, data = csv$data, family = Gamma(link = "inverse"))))
    #a formula that does not fit gives no model and a message in the summary
    session$setInputs(formula = "not_a_column", fit_model = 5)
    expect_null(fitted_model())
    expect_match(output$model_summary, "failed")
  })
})

test_that("GLM fitting tool checks the formula, reads text binomial responses and predicts every row", {
  set.seed(7)
  d <- data.frame(claim = sample(c("no", "yes"), 150, TRUE, prob = c(0.7, 0.3)), age = rnorm(150, 40, 10),
                  grp = sample(c("a", "b"), 150, TRUE), y = rpois(150, 2))
  #a level that only appears in a row the fit leaves out
  d$grp[1] <- "c"
  d$y[1] <- NA
  path <- write_csv(d)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "b.csv"), submit = 1)
    session$setInputs(response_variable = "claim", glm_distribution = "binomial", link_function = "logit",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    expect_equal(coef(fitted_model()), coef(glm(factor(claim) ~ age, data = d, family = binomial)))
    expect_match(output$coefficients_table$html, "Odds ratio")
    #the terms must not repeat the response
    session$setInputs(formula = "claim ~ age", fit_model = 2)
    expect_null(fitted_model())
    expect_match(output$model_summary, "only the terms")
    #"." stands for every other column
    session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", formula = ".", fit_model = 3)
    expect_setequal(attr(terms(fitted_model()), "term.labels"), c("claim", "age", "grp"))
    #predictions cover every row, NA for the row left out, without failing on its level
    predictions <- model_predictions(fit_result())
    expect_length(predictions, 150)
    expect_true(is.na(predictions[1]))
    expect_equal(predictions, unname(fitted(fitted_model())))
    #the call names the family, so update() on the downloaded model refits it, given the data
    expect_match(paste(deparse(fitted_model()$call), collapse = ""), "poisson(link = \"log\")", fixed = TRUE)
    expect_equal(coef(update(fitted_model(), . ~ . - grp, data = d, na.action = na.omit)),
                 coef(glm(y ~ claim + age, data = d, family = poisson)))
    #the model does not carry the Shiny session (and its inputs, such as a password) in its environment
    expect_identical(environment(formula(fitted_model())), globalenv())
    expect_identical(environment(fitted_model()$terms), globalenv())
    #warnings of the fit are kept and shown, not swallowed
    session$setInputs(response_variable = "age", glm_distribution = "binomial", link_function = "logit", formula = "y", fit_model = 4)
    expect_true(length(fit_result()$warnings) > 0 || is.null(fitted_model()))
  })
})

test_that("GLM fitting tool charts a text binomial response with more than two values as glm() codes it", {
  set.seed(11)
  d <- data.frame(outcome = sample(c("none", "small", "large"), 240, TRUE), age = rnorm(240, 40, 10),
                  grp = sample(c("a", "b", "c"), 240, TRUE))
  d$age[1] <- NA
  path <- write_csv(d)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "m.csv"), submit = 1)
    session$setInputs(response_variable = "outcome", glm_distribution = "binomial", link_function = "logit",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    #glm() takes the first level ("large") as failure and the two others as success, and the tool says so
    reference <- glm(factor(outcome) ~ age, data = d, family = binomial)
    expect_equal(coef(fitted_model()), coef(reference))
    expect_match(paste(fit_result()$warnings, collapse = " "), "3 values: 'large' is failure")
    session$setInputs(visualize_variable = "grp", number_of_bands_input = 10, execute_visualization = 1)
    #per band, the actual is the share of responses other than "large" and the predicted its fitted mean
    used <- !is.na(d$age)
    expect_equal(fitness_data()$band, c("a", "b", "c"))
    expect_equal(fitness_data()$actual, as.numeric(tapply(d$outcome[used] != "large", d$grp[used], mean)))
    expect_equal(fitness_data()$predicted, as.numeric(tapply(fitted(reference), d$grp[used], mean)))
    expect_true(all(fitness_data()$actual >= 0 & fitness_data()$actual <= 1))
    expect_equal(attr(fitness_data(), "dropped"), 1)
  })
})

test_that("GLM fitting tool leaves a logical binomial response as it is, TRUE being success", {
  #every response TRUE: made a factor, TRUE would be the first level, that is failure
  d <- data.frame(hit = TRUE, x = 1:20)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = write_csv(d), name = "l.csv"), submit = 1)
    expect_true(is.logical(selected_data()$hit))
    session$setInputs(response_variable = "hit", glm_distribution = "binomial", link_function = "logit",
                      offset = "None", weights = "None", formula = "", fit_model = 1)
    expect_equal(unname(fitted_model()$y), rep(1, 20))
    expect_true(all(model_predictions(fit_result()) > 0.99))
    session$setInputs(visualize_variable = "x", number_of_bands_input = 2, band_method = "width", execute_visualization = 1)
    expect_equal(fitness_data()$actual, c(1, 1))
  })
})

test_that("GLM fitting tool saves settings without the password and restores column choices after import", {
  csv <- write_glm_csv()
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(db_password = "secret", formula = "x1 + x2", glm_distribution = "poisson", link_function = "log", csv_sep = ",")
    saved <- readRDS(output$DownloadDataHandlerConf)
    expect_identical(saved$tool, glm_settings_tool)
    expect_null(saved$inputs$db_password)
    expect_equal(saved$inputs$formula, "x1 + x2")
    #column choices loaded before any import wait for the data
    settings_file <- tempfile(fileext = ".rds")
    saveRDS(list(tool = glm_settings_tool, version = 1L, inputs = list(response_variable = "y", weights = "w", formula = "x1")), settings_file)
    session$setInputs(load_config = list(datapath = settings_file, name = "s.rds"))
    expect_equal(pending_columns(), list(response_variable = "y", weights = "w"))
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = csv$path, name = "glm.csv"), submit = 1)
    session$flushReact()
    expect_equal(pending_columns(), list())
    #a file that is not a settings file is refused without an error: text, a truncated file,
    #an object that is not a list, and lists with no usable input
    junk <- tempfile(fileext = ".rds")
    writeLines("not a settings file", junk)
    session$setInputs(load_config = list(datapath = junk, name = "junk.rds"))
    expect_equal(pending_columns(), list())
    truncated <- tempfile(fileext = ".rds")
    writeBin(readBin(settings_file, "raw", 20), truncated)
    others <- list(1:3, new.env(), list(1, 2), list(tool = glm_settings_tool, inputs = "x"),
                   list(response_variable = list("y"), formula = NA_character_, number_of_bands_input = "ten"))
    files <- c(truncated, vapply(others, function(x) {
      path <- tempfile(fileext = ".rds")
      saveRDS(x, path)
      path
    }, character(1)))
    for (i in seq_along(files)) {
      session$setInputs(load_config = list(datapath = files[i], name = paste0("junk", i, ".rds")))
      expect_equal(pending_columns(), list())
    }
  })
})

test_that("GLM fitting tool database import checks for the driver package and reports errors", {
  shiny::testServer(GLMFittingToolServer, {
    expect_false(glm_tool_db_package_available("Not a database"))
    #a connection that cannot be opened gives no data instead of an error
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = file.path(tempdir(), "does_not_exist", "x.sqlite"), sql_query = "SELECT 1", submit = 1)
    expect_null(selected_data())
    #a SQLite file that does not exist is not created
    missing_file <- file.path(tempdir(), "no_such_database.sqlite")
    session$setInputs(db_name = missing_file, submit = 2)
    expect_null(selected_data())
    expect_false(file.exists(missing_file))
  })
  #values in an ODBC connection string are braced
  expect_equal(glm_tool_odbc_value("srv;Trusted_Connection=yes"), "{srv;Trusted_Connection=yes}")
  expect_equal(glm_tool_odbc_value("a}b"), "{a}}b}")
})

test_that("GLM fitting tool imports a table from SQLite through the DBI helper", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  DBI::dbWriteTable(con, "claims", data.frame(id = 1:5, amount = c(10, 20, 30, 40, 50)))
  DBI::dbDisconnect(con)
  expect_equal(glm_tool_query_dbi(RSQLite::SQLite(), "SELECT * FROM claims", dbname = db), data.frame(id = 1:5, amount = c(10, 20, 30, 40, 50)))
  shiny::testServer(GLMFittingToolServer, {
    expect_true(glm_tool_db_package_available("SQLite"))
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = db, sql_query = "SELECT id, amount FROM claims WHERE amount > 20", submit = 1)
    expect_equal(selected_data(), data.frame(id = 3:5, amount = c(30, 40, 50)))
  })
})
