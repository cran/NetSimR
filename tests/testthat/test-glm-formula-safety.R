#the GLM fitting tool's formula: model.frame() and glm() run whatever it calls, so the terms typed in
#the formula box, or read from a shared settings file, may only use the data's columns and a small
#set of formula functions (glm_formula_problem())

#insurance-like data whose column names need backticks in a formula
glm_formula_data <- function(n = 400) {
  set.seed(31)
  d <- data.frame(age = sample(18:80, n, TRUE), region = sample(c("North", "South", "East", "West"), n, TRUE),
                  exposure = round(runif(n, 0.2, 1), 3), stringsAsFactors = FALSE)
  d[["claim count"]] <- rpois(n, d$exposure * exp(-2 + 0.01 * d$age))
  d[["a+b"]] <- rnorm(n)
  d[["1st"]] <- runif(n)
  d[["NA"]] <- sample(0:1, n, TRUE)
  d[["T"]] <- rnorm(n)
  d
}

glm_formula_csv <- function(d) {
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  path
}

glm_formula_import <- function(session, path, k = 1) {
  session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = basename(path)), submit = k)
}

#the notifications of a testServer session, which the mock session otherwise drops
glm_formula_notes <- function(session) {
  log <- new.env()
  log$notes <- list()
  session$sendNotification <- function(type, message) {
    if (identical(type, "show")) log$notes[[length(log$notes) + 1]] <- message
  }
  log
}

test_that("glm_formula_problem() accepts the formulas a model needs and names what it refuses", {
  columns <- c("y", "age", "region", "exposure", "claim count", "a+b", "1st", "NA", "T")
  fine <- c(".", "1", "0 + age", "age - 1", "age*region", "age:region", "age + region + age:region", "(age + region)^2",
            "age/region", "I(age^2)", "I(age * 2 + 1)", "log(age)", "log(age, 10)", "exp(age)", "sqrt(age)",
            "poly(age, 2)", "poly(age, degree = 2, raw = TRUE)", "factor(region)", "as.factor(`NA`)",
            "interaction(region, `NA`)", "offset(log(exposure))", "I(pmin(age, 50)) + I(pmax(age, 50))",
            "`claim count` + `a+b` + `1st` + `NA` + `T`", ". - age", "age + -1")
  for (text in fine) {
    expect_null(glm_formula_problem(text, columns), label = text)
    expect_null(glm_formula_problem(stats::as.formula(paste("y ~", text), env = globalenv()), columns), label = text)
  }
  allowed <- "it may only use the data's columns with + - * : / ^ and I(), log(), exp(), sqrt(), pmin(), pmax(), poly(), factor(), as.factor(), interaction() and offset()"
  expect_identical(glm_formula_problem("age + system('echo hi', intern = TRUE)", columns),
                   paste0("the formula may not call system(); ", allowed))
  #the contents of I() are checked: that is where an expression is evaluated as it is
  expect_match(glm_formula_problem("age + I(age * 0 + file.create('marker.txt'))", columns), "may not call file.create()", fixed = TRUE)
  expect_match(glm_formula_problem("age + I(stop(Sys.getenv('R_HOME')))", columns), "may not call stop()", fixed = TRUE)
  expect_match(glm_formula_problem("log(I(exp(system('x'))))", columns), "may not call system()", fixed = TRUE)
  #a function that is harmless but not on the list, a call through ::, a function literal, and
  #the subsetting, assignment and control operators
  expect_match(glm_formula_problem("abs(age)", columns), "may not call abs()", fixed = TRUE)
  expect_match(glm_formula_problem("age + base::system('x')", columns), "may not call base::system()", fixed = TRUE)
  expect_match(glm_formula_problem("age + (function(x) x)(1)", columns), "may not call (function(x) x)()", fixed = TRUE)
  expect_match(glm_formula_problem("age + I((function(x) x)(1))", columns), "may not call (function(x) x)()", fixed = TRUE)
  expect_match(glm_formula_problem("age + region[1]", columns), "may not call [()", fixed = TRUE)
  expect_match(glm_formula_problem("age + mtcars$mpg", columns), "may not call $()", fixed = TRUE)
  expect_match(glm_formula_problem("age + {1}", columns), "may not call {()", fixed = TRUE)
  expect_match(glm_formula_problem("I(x <- 1)", columns), "may not call <-()", fixed = TRUE)
  expect_match(glm_formula_problem("age %in% 1", columns), "may not call %in%()", fixed = TRUE)
  #names that are not columns of the data, text and other constants
  expect_identical(glm_formula_problem("age + pi", columns), "'pi' is not a column of the data")
  expect_identical(glm_formula_problem("age + not_a_column", columns), "'not_a_column' is not a column of the data")
  expect_match(glm_formula_problem("factor(region, 'a')", columns), "may not contain \"a\"", fixed = TRUE)
  expect_match(glm_formula_problem("age + 1i", columns), "may not contain 0+1i", fixed = TRUE)
  #before the data is imported (a settings file) any name is allowed, but no other call is
  expect_null(glm_formula_problem("anything + `some column` + log(other)"))
  expect_match(glm_formula_problem("anything + I(system('x'))"), "may not call system()", fixed = TRUE)
  #text that does not parse runs nothing: it is left for the fit to report
  expect_null(glm_formula_problem("age +", columns))
  expect_null(glm_formula_problem("age + )", columns))
  #an empty argument is passed over
  expect_null(glm_formula_problem("poly(age, )", columns))
  expect_match(glm_formula_problem("poly(age, , system('x'))", columns), "may not call system()", fixed = TRUE)
})

test_that("GLM fitting tool still fits every legitimate formula, with column names that need backticks", {
  d <- glm_formula_data()
  path <- glm_formula_csv(d)
  reference <- function(text) {
    glm(stats::as.formula(paste("`claim count` ~", text), env = globalenv()), family = poisson, data = d)
  }
  shiny::testServer(GLMFittingToolServer, {
    glm_formula_import(session, path)
    expect_identical(names(selected_data()), names(d))
    formulas <- c(".", "age*region", "age:region", "I(age^2)", "log(exposure)", "poly(age, 2)", "factor(region)",
                  "offset(log(exposure))", "I(pmin(age, 50))", "`a+b` + `1st` + `NA` + `T`", "1", "")
    for (i in seq_along(formulas)) {
      session$setInputs(response_variable = "claim count", glm_distribution = "poisson", link_function = "log",
                        offset = "None", weights = "None", formula = formulas[i], fit_model = i)
      model <- fitted_model()
      expect_s3_class(model, "glm")
      if (!formulas[i] %in% c(".", "")) {
        expect_equal(coef(model), coef(reference(formulas[i])), label = formulas[i])
      }
    }
    #the tool's own offset term, and a column named T (which is not TRUE)
    session$setInputs(offset = "exposure", offset_log = TRUE, formula = "age + T", fit_model = 20)
    expect_equal(coef(fitted_model()), coef(reference("offset(log(exposure)) + age + T")))
    expect_true("T" %in% names(coef(fitted_model())))
    #the response of one model is a term of another
    session$setInputs(response_variable = "T", glm_distribution = "gaussian", link_function = "identity",
                      offset = "None", formula = "`claim count` + `a+b` + `1st` + `NA`", fit_model = 21)
    expect_equal(coef(fitted_model()), coef(glm(T ~ `claim count` + `a+b` + `1st` + `NA`, data = d)))
  })
})

test_that("GLM fitting tool refuses a formula that calls anything but the formula functions", {
  d <- glm_formula_data()
  path <- glm_formula_csv(d)
  #the marker is named in the formula, so its name must not depend on the machine: a temporary
  #path can hold a ~ (the short name of a Windows user directory, RUNNER~1 on the test runners),
  #and the tool refuses a formula containing ~ before it looks at what the formula calls
  marker_dir <- tempfile("glm-formula-marker-")
  dir.create(marker_dir)
  old_dir <- setwd(marker_dir)
  on.exit(setwd(old_dir), add = TRUE)
  marker <- "marker.txt"
  secret <- "glm-formula-secret-4711"
  Sys.setenv(NETSIMR_FORMULA_TEST_SECRET = secret)
  on.exit(Sys.unsetenv("NETSIMR_FORMULA_TEST_SECRET"), add = TRUE)
  shiny::testServer(GLMFittingToolServer, {
    log <- glm_formula_notes(session)
    glm_formula_import(session, path)
    refused <- function(formula, k) {
      log$notes <- list()
      session$setInputs(response_variable = "claim count", glm_distribution = "poisson", link_function = "log",
                        offset = "None", weights = "None", formula = formula, fit_model = k)
      expect_null(fitted_model())
      #reported like any other fitting problem: a notification and the summary, no crash
      expect_length(log$notes, 1)
      expect_identical(log$notes[[1]]$type, "error")
      expect_match(as.character(log$notes[[1]]$html), "Model fitting failed", fixed = TRUE)
      expect_match(output$model_summary, "Model fitting failed", fixed = TRUE)
      fit_result()$error
    }
    #the exploit hidden inside I(): the model fitted and the file was created
    error <- refused(paste0("age + I(age * 0 + file.create(", deparse(marker), "))"), 1)
    expect_false(file.exists(marker))
    expect_match(error, "the formula may not call file.create(); it may only use the data's columns", fixed = TRUE)
    #the value of an environment variable shown in the error message
    error <- refused("age + I(stop(Sys.getenv('NETSIMR_FORMULA_TEST_SECRET')))", 2)
    expect_no_match(error, secret, fixed = TRUE)
    expect_match(error, "may not call stop()", fixed = TRUE)
    #a call outside I(), one through ::, and a harmless function that is not on the list
    expect_match(refused("age + system('echo hi', intern = TRUE)", 3), "may not call system()", fixed = TRUE)
    expect_match(refused("age + base::Sys.getenv('HOME')", 4), "may not call base::Sys.getenv()", fixed = TRUE)
    expect_match(refused("abs(age)", 5), "may not call abs(); it may only use the data's columns with + - * : / ^ and I(), log()", fixed = TRUE)
    #a name that is not a column would be looked up outside the data
    expect_identical(refused("age + pi", 6), "'pi' is not a column of the data")
    expect_error(output$coefficients_table, "The model could not be fitted: 'pi' is not a column of the data", fixed = TRUE)
    #fitting works again afterwards
    session$setInputs(formula = "age", fit_model = 7)
    expect_s3_class(fitted_model(), "glm")
  })
  expect_false(file.exists(marker))
})

test_that("GLM fitting tool refuses a settings file whose formula would run code, at load", {
  d <- glm_formula_data()
  path <- glm_formula_csv(d)
  marker <- tempfile(fileext = ".txt")
  hostile <- tempfile(fileext = ".txt")
  write_settings_file(list(glm_distribution = "poisson", link_function = "log",
                           formula = paste0("age + I(age * 0 + file.create(", deparse(marker), "))")),
                      hostile, glm_settings_tool, glm_settings_version)
  by_colon <- tempfile(fileext = ".txt")
  write_settings_file(list(formula = "age + base::system('x')"), by_colon, glm_settings_tool, glm_settings_version)
  fine <- tempfile(fileext = ".txt")
  write_settings_file(list(glm_distribution = "poisson", link_function = "log", formula = "poly(age, 2) + `a+b`"),
                      fine, glm_settings_tool, glm_settings_version)
  shiny::testServer(GLMFittingToolServer, {
    log <- glm_formula_notes(session)
    sent <- list()
    session$sendInputMessage <- function(inputId, message) sent[[inputId]] <<- message
    glm_formula_import(session, path)
    session$setInputs(response_variable = "claim count", glm_distribution = "gaussian", link_function = "identity",
                      offset = "None", weights = "None", formula = "age")
    sent <- list()
    #the hostile file: a notification naming the call, and nothing of the file applied
    session$setInputs(load_config = list(datapath = hostile, name = "hostile.txt"))
    expect_length(log$notes, 1)
    expect_identical(log$notes[[1]]$type, "error")
    expect_match(as.character(log$notes[[1]]$html), "'hostile.txt' was not loaded. The formula may not call file.create(); it may only use", fixed = TRUE)
    expect_length(sent, 0)
    expect_equal(pending_columns(), list())
    #the next Fit uses the formula typed before, and nothing has run
    session$setInputs(fit_model = 1)
    expect_s3_class(fitted_model(), "glm")
    expect_identical(fit_result()$spec$formula, "age")
    expect_identical(fitted_model()$family$family, "gaussian")
    expect_false(file.exists(marker))
    log$notes <- list()
    session$setInputs(load_config = list(datapath = by_colon, name = "colon.txt"))
    expect_match(as.character(log$notes[[1]]$html), "may not call base::system()", fixed = TRUE)
    expect_length(sent, 0)
    #a settings file with a legitimate formula is applied
    log$notes <- list()
    session$setInputs(load_config = list(datapath = fine, name = "fine.txt"))
    expect_identical(log$notes[[1]]$type, "message")
    expect_identical(sent$formula$value, "poly(age, 2) + `a+b`")
    expect_identical(sent$glm_distribution$value, "poisson")
  })
  expect_false(file.exists(marker))
})
