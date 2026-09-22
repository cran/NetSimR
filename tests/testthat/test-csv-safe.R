#CSV downloads: text a spreadsheet would read as a formula is written as text

test_that("csv_safe() keeps formula-like text and column names as text and leaves numbers alone", {
  df <- data.frame(
    note = c("=HYPERLINK(\"http://example.com\",\"x\")", "+1+2", "-2+3", "@SUM(A1)", "\tcmd", "\rcmd",
             "normal text", NA),
    amount = c(-5, -0.5, 1, 2, 3, 4, 5, NA),
    region = factor(c("=A1", "North", "South", "East", "West", "North", "South", "East")),
    stringsAsFactors = FALSE
  )
  names(df)[2] <- "=amount"
  safe <- csv_safe(df)
  expect_identical(names(safe), c("note", "'=amount", "region"))
  expect_identical(safe$note[1:6], paste0("'", df$note[1:6]))
  expect_identical(safe$note[7], "normal text")
  expect_true(is.na(safe$note[8]))
  #numbers, including negative ones, are written as numbers
  expect_identical(safe[[2]], df[[2]])
  expect_identical(safe$region[1:2], c("'=A1", "North"))
})

test_that("the GLM tool's data-with-predictions download writes formula-like cells as text", {
  set.seed(3)
  n <- 200
  d <- data.frame(policy_note = rep(c("=HYPERLINK(\"http://example.com\",\"open\")", "ordinary"), n / 2),
                  age = sample(18:80, n, TRUE), exposure = round(runif(n, 0.2, 1), 3),
                  change = round(rnorm(n), 2), stringsAsFactors = FALSE)
  d$claims <- rpois(n, d$exposure * 0.3)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write.csv(d, path, row.names = FALSE)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claims", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    expect_s3_class(fitted_model(), "glm")
    downloaded <- utils::read.csv(output$download_data_with_predictions, stringsAsFactors = FALSE)
    expect_identical(unique(downloaded$policy_note),
                     c("'=HYPERLINK(\"http://example.com\",\"open\")", "ordinary"))
    #negative numbers stay numbers
    expect_true(is.numeric(downloaded$change))
    expect_equal(downloaded$change, d$change)
    expect_true(is.numeric(downloaded$prediction))
  })
})
