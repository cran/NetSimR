#the distribution fitting and GLM fitting tools with unusual data, driven through shiny::testServer()

fit_csv <- function(d) {
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  path
}

fit_lines <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

fit_upload <- function(session, path, sep = ",", dec = ".") {
  session$setInputs(file1 = list(datapath = path, name = basename(path)), data_includes_header = TRUE, sep = sep,
                    quote = intToUtf8(34), dec = dec)
}

fit_import <- function(session, path) {
  session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = basename(path)), submit = 1)
}

test_that("distribution fitting tool fits the exact Gamma MLE to claims of 1e12", {
  set.seed(9)
  base <- rgamma(300, shape = 2)
  shiny::testServer(distribution_fitting_tool_Server, {
    fit_upload(session, fit_csv(data.frame(sev = base * 1e12)))
    session$setInputs(severity_var = "sev", execute_sev_analysis = 1)
    #the optimiser gave a shape of 141.5 here, against 2.1175 for the same claims at any scale up to 1e8
    expect_equal(sev_gamma_fit()$estimate[["shape"]], fit_gamma_mle(base)$estimate[["shape"]], tolerance = 1e-8)
    expect_equal(sev_gamma_fit()$estimate[["shape"]] * sev_gamma_fit()$estimate[["scale"]], mean(severity_data()))
  })
})

test_that("distribution fitting tool reads points as thousands separators with a decimal comma", {
  path <- fit_lines(c("sev;n", "1.234;2", "2.500;3", "15.000,5;1", "800;4", "0,5;2"))
  shiny::testServer(distribution_fitting_tool_Server, {
    fit_upload(session, path, sep = ";", dec = ",")
    expect_true("sev" %in% numeric_columns())
    session$setInputs(severity_var = "sev", execute_sev_analysis = 1)
    expect_equal(severity_data(), c(1234, 2500, 15000.5, 800, 0.5))
  })
})

test_that("both tools read a file with a separator at the end of each row", {
  #read.csv() took the first column as row names, so sev showed the counts; repeated values failed
  path <- fit_lines(c("sev,n", "100,1,", "250,2,", "250,3,", "80,4,"))
  shiny::testServer(distribution_fitting_tool_Server, {
    fit_upload(session, path)
    expect_equal(names(data()), c("sev", "n", "V3"))
    session$setInputs(severity_var = "sev", execute_sev_analysis = 1)
    expect_equal(severity_data(), c(100, 250, 250, 80))
  })
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, path)
    expect_equal(selected_data()$sev, c(100, 250, 250, 80))
    expect_equal(selected_data()$n, 1:4)
  })
  #a file with only the header, and the preview cap: the table shows the first 100 rows of 10,001
  shiny::testServer(distribution_fitting_tool_Server, {
    fit_upload(session, fit_lines("sev,n"))
    expect_error(data(), "no data rows")
    fit_upload(session, fit_csv(data.frame(sev = seq_len(10001), region = "North")))
    expect_match(output$data_overview$html, "The preview shows the first 100")
    html <- output$data_table$html
    expect_match(html, "Showing the first 100 of 10,001 rows.", fixed = TRUE)
    expect_equal(lengths(regmatches(html, gregexpr("<tr>", html, fixed = TRUE))), 101)
  })
})

test_that("GLM fitting tool leaves the offset and weights columns out of every formula with a dot", {
  set.seed(8)
  d <- data.frame(y = rpois(200, 2), x1 = rnorm(200), grp = sample(c("a", "b"), 200, TRUE),
                  expo = runif(200, 0.2, 2), w = sample(1:3, 200, TRUE))
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, fit_csv(d))
    expected <- list(". - x1" = "grp", ".^2" = c("x1", "grp", "x1:grp"), ". + x1:grp" = c("x1", "grp", "x1:grp"), "x1 + ." = c("x1", "grp"))
    for (i in seq_along(expected)) {
      session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", offset = "expo",
                        offset_log = TRUE, weights = "w", formula = names(expected)[i], fit_model = i)
      expect_setequal(attr(terms(fitted_model()), "term.labels"), expected[[i]])
    }
    session$setInputs(formula = ". - x1", fit_model = 10)
    expect_equal(coef(fitted_model()), coef(glm(y ~ offset(log(expo)) + grp, family = poisson, weights = w, data = d)))
    expect_identical(environment(formula(fitted_model())), globalenv())
  })
})

test_that("GLM fitting tool leaves text IDs out of a dot and warns when one is named", {
  set.seed(4)
  n <- 300
  d <- data.frame(y = rpois(n, 2), x1 = rnorm(n), id = sprintf("P%05d", seq_len(n)), region = sample(letters[1:5], n, TRUE))
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, fit_csv(d))
    session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", offset = "None",
                      weights = "None", formula = ".", fit_model = 1)
    #one coefficient per row would make a 300 x 300 model matrix (2,000 rows took about 100 seconds)
    expect_setequal(attr(terms(fitted_model()), "term.labels"), c("x1", "region"))
    expect_equal(fit_result()$left_out, c(id = "300 different values"))
    #named in the formula, the column is used, with a warning
    session$setInputs(formula = "x1 + id", fit_model = 2)
    expect_length(coef(fitted_model()), n + 1)
    expect_match(paste(fit_result()$warnings, collapse = " "), "'id' is text with 300 different values")
    #removed from the dot, it gives no warning
    session$setInputs(formula = ". - id", fit_model = 3)
    expect_false(any(grepl("'id'", fit_result()$warnings)))
  })
  expect_true(glm_many_values(as.character(1:101)))
  expect_true(glm_many_values(factor(rep(1:40, length.out = 60))))
  expect_false(glm_many_values(rep(letters[1:5], 2)))
  expect_false(glm_many_values(rep(letters, 10)))
  expect_false(glm_many_values(seq_len(1000)))
})

test_that("GLM fitting tool explains formulas and columns it cannot fit", {
  set.seed(5)
  d <- data.frame(y = rpois(50, 2), x1 = rnorm(50), grp = sample(c("a", "b"), 50, TRUE), one_level = "only", all_na = NA)
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, fit_csv(d))
    failure <- function(response, formula, family = "gaussian", k) {
      session$setInputs(response_variable = response, glm_distribution = family, link_function = glm_family_links[[family]][1],
                        offset = "None", weights = "None", formula = formula, fit_model = k)
      expect_null(fitted_model())
      fit_result()$error
    }
    #these gave "<text>:2:0: unexpected end of input", "non-numeric argument to binary operator",
    #"contrasts can be applied only to factors with 2 or more levels" and "object 'fit' not found"
    expect_equal(failure("y", "x1 +", k = 1), "the formula could not be read: unexpected end of input")
    expect_match(failure("grp", "x1", k = 2), "the response 'grp' has text values, such as 'a'; the gaussian family needs numbers")
    expect_match(failure("grp", "x1", family = "poisson", k = 3), "poisson family needs numbers")
    expect_match(failure("y", "x1 + one_level", family = "poisson", k = 4), "'one_level' has a single value ('only')", fixed = TRUE)
    expect_match(failure("y", "x1 + all_na", k = 5), "no row has a value in every column the model uses: 'all_na' has no values")
    expect_match(failure("all_na", "x1", k = 6), "'all_na' has no values")
    expect_match(output$model_summary, "formula could not be read|no values")
    #a text response is fine for a binomial model; "." leaves out the empty and single-valued columns
    session$setInputs(response_variable = "grp", glm_distribution = "binomial", link_function = "logit", formula = ".", fit_model = 7)
    expect_setequal(attr(terms(fitted_model()), "term.labels"), c("y", "x1"))
    expect_equal(fit_result()$left_out, c(one_level = "a single value", all_na = "no values"))
  })
})

test_that("GLM fitting tool reads numbers with thousands separators as numbers", {
  path <- fit_lines(c("y,amount,x,flag", "1,\"1,200\",0.5,TRUE", "0,\"15,000\",1.2,FALSE", "2,\"3,400\",0.1,TRUE",
                      "1,999,0.3,FALSE", "3,\"2,500\",0.9,TRUE"))
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, path)
    expect_equal(selected_data()$amount, c(1200, 15000, 3400, 999, 2500))
    expect_true(is.logical(selected_data()$flag))
    session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", offset = "None",
                      weights = "None", formula = "amount", fit_model = 1)
    expect_equal(names(coef(fitted_model())), c("(Intercept)", "amount"))
  })
})

test_that("GLM fitting tool labels the chart by whether the offset is an exposure", {
  set.seed(6)
  d <- data.frame(y = rpois(100, 2), x1 = rnorm(100), grp = sample(c("a", "b"), 100, TRUE), e = runif(100, 0.5, 2))
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, fit_csv(d))
    #the chart's titles, and its alternative text, which names them
    chart <- function(link, offset_log, k) {
      family <- if (link == "log") "poisson" else "gaussian"
      session$setInputs(response_variable = "y", glm_distribution = family, link_function = link, offset = "e",
                        offset_log = offset_log, weights = "None", formula = "x1", fit_model = k,
                        visualize_variable = "grp", number_of_bands_input = 10, execute_visualization = k)
      plot <- output$fitness_plot
      expect_match(plot$src, "^data:image/png;base64,")
      expect_equal(fitness_labels()$x, "grp")
      paste(c(unlist(fitness_labels()), plot$alt), collapse = " | ")
    }
    #a raw offset with the identity link is not an exposure: the chart shows row counts and plain averages
    labels <- chart("identity", FALSE, 1)
    expect_match(labels, "Average y")
    expect_match(labels, "Rows")
    expect_false(grepl("exposure", labels, ignore.case = TRUE))
    expect_equal(fitness_data()$exposure, as.numeric(table(d$grp)))
    labels <- chart("log", TRUE, 2)
    expect_match(labels, "y per unit of exposure")
    expect_match(labels, "Exposure (e)", fixed = TRUE)
    labels <- chart("log", FALSE, 3)
    expect_match(labels, "Exposure (exp(e))", fixed = TRUE)
  })
})

test_that("distribution fitting tool explains all-zero counts and shows large totals in full", {
  shiny::testServer(distribution_fitting_tool_Server, {
    fit_upload(session, fit_csv(data.frame(n = rep(0, 20))))
    session$setInputs(counts_var = "n", counts_weighted_var = FALSE, execute_freq_analysis = 1)
    #both fits failed with "the function mle failed to estimate the parameters, with the error code 100"
    expect_error(counts_data(), "All the claim counts are zero")
    fit_upload(session, fit_csv(data.frame(n = c(1, 2, 3), w = c(1e9, 6e8, 1e7))))
    session$setInputs(counts_var = "n", counts_weighted_var = TRUE, counts_weights_var = "w", execute_freq_analysis = 2)
    expect_match(output$freq_stats$html, "1,610,000,000", fixed = TRUE)
    expect_false(grepl("e+09", output$freq_stats$html, fixed = TRUE))
  })
})

test_that("distribution fitting tool suggests the model that the fits support", {
  shiny::testServer(distribution_fitting_tool_Server, {
    suggestion <- function(counts, k) {
      fit_upload(session, fit_csv(data.frame(n = counts)))
      session$setInputs(counts_var = "n", counts_weighted_var = FALSE, execute_freq_analysis = k)
      gsub("\\s+", " ", gsub("<[^>]+>", " ", output$freq_stats$html))
    }
    #the sample variance (2.33) is above the mean (1.67), but the Negative Binomial tends to the Poisson,
    #which the table marks as the lowest AIC; the tile suggested the Negative Binomial
    expect_match(suggestion(c(0, 2, 3), 1), "Suggested model Poisson Too little overdispersion for the Negative Binomial")
    expect_true(freq_nb_fit()$capped)
    set.seed(12)
    expect_match(suggestion(rnbinom(300, size = 1, mu = 4), 2), "Suggested model Negative Binomial Lower AIC: the counts are overdispersed")
    expect_lt(freq_nb_fit()$aic, freq_po_fit()$aic)
    expect_match(suggestion(c(2, 2, 3, 3, 2, 3), 3), "Suggested model Poisson The variance is not above the mean")
  })
})

test_that("the mean excess chart explains that it needs three different claim sizes", {
  shiny::testServer(distribution_fitting_tool_Server, {
    #the largest claim has no mean excess, so two different claims left a single point and an empty chart
    fit_upload(session, fit_csv(data.frame(s = c(1, 2, 2))))
    session$setInputs(sliced_sev_var = "s", sev_cens_fit_log_scale = TRUE, execute_sliced_sev_analysis = 1)
    expect_error(output$mean_excess_func_plot, "at least three different claim sizes")
    fit_upload(session, fit_csv(data.frame(s = c(1, 2, 3))))
    session$setInputs(execute_sliced_sev_analysis = 2)
    expect_match(output$mean_excess_func_plot$src, "^data:image/png;base64,")
  })
})

test_that("GLM fitting tool labels bands readably and refuses text with too many values", {
  set.seed(7)
  n <- 600
  d <- data.frame(y = rpois(n, 2), id = sprintf("policy_%04d", sample(1:300, n, TRUE)), age = round(runif(n, 18, 80), 4),
                  si = sample(c(1000, 25000, 250000), n, TRUE))
  shiny::testServer(GLMFittingToolServer, {
    fit_import(session, fit_csv(d))
    session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", offset = "None",
                      weights = "None", formula = "age", fit_model = 1)
    #text is not grouped into bands: an ID drew hundreds of bars
    session$setInputs(visualize_variable = "id", number_of_bands_input = 10, band_method = "quantile", execute_visualization = 1)
    expect_error(fitness_data(), paste0("'id' has ", length(unique(d$id)),
                                        " different values: choose a numeric variable, or one with at most 100 values."), fixed = TRUE)
    #bands read "18-24", not "(18.0006,23.9723]", and hold the rows cut() puts in them
    session$setInputs(visualize_variable = "age", execute_visualization = 2)
    breaks <- unique(quantile(d$age, seq(0, 1, 0.1), names = FALSE))
    expect_equal(fitness_data()$band, glm_band_labels(breaks))
    expect_match(fitness_data()$band[1], paste0("^18", intToUtf8(8211)))
    expect_equal(fitness_data()$exposure, as.numeric(table(cut(d$age, breaks, include.lowest = TRUE))))
    session$setInputs(band_method = "width", execute_visualization = 3)
    expect_equal(fitness_data()$exposure, as.numeric(table(cut(d$age, 10, include.lowest = TRUE))))
    expect_false(any(grepl("(", fitness_data()$band, fixed = TRUE)))
    #a numeric variable with few values: one bar each, with thousands separators
    session$setInputs(visualize_variable = "si", execute_visualization = 4)
    expect_equal(fitness_data()$band, c("1,000", "25,000", "250,000"))
    expect_match(output$fitness_plot$src, "^data:image/png;base64,")
  })
})

test_that("piecewise Pareto layer 1 includes the smallest claim", {
  x <- c(100, 100, 150, 200, 300, 400, 800, 1600, 5000, 12000)
  shiny::testServer(distribution_fitting_tool_Server, {
    fit_upload(session, fit_csv(data.frame(sev = x)))
    session$setInputs(piecewise_pareto_var = "sev", num_pareto_slices = 1, piecewise_pareto_fit_log_scale = FALSE,
                      execute_piecewise_sev_analysis = 1)
    session$setInputs(slider_1 = 400)
    expect_equal(piecwise_pareto_mu(), c(100, 400))
    #both claims at 100 count in layer 1, as the table's Claims column counts them
    expect_equal(piecwise_pareto_alpha()[1], sum(x < 400) / sum(log(pmin(x, 400) / 100)))
    expect_match(output$fitted_sliced_pareto$html, "includes, the smallest claim")
    #with the smallest claim as the only threshold, the fit is the Severity tab's Pareto
    session$setInputs(severity_var = "sev", execute_sev_analysis = 1)
    expect_equal(piecewise_pareto_alpha(piecewise_sev_data(), min(x)), sev_pareto_alpha())
  })
})
