test_that("the documented examples give the ceded amounts", {
  expect_equal(apply_deductible_limit(c(100, 50, 20), "Limited Layer", 40, 20), c(20, 10, 0))
  expect_equal(apply_deductible_limit(c(100, 50, 20), "Limited Layer", 10, 30), c(30, 30, 10))
})

test_that("no reinsurance structure returns the claims unchanged", {
  claims <- c(0, 5, 40, 100, 250)
  expect_identical(apply_deductible_limit(claims, "No Reinsurance Structure", 40, 20), claims)
})

test_that("an unlimited layer cedes everything above the deductible", {
  claims <- c(0, 5, 40, 100, 250)
  expect_equal(apply_deductible_limit(claims, "Unlimited Layer", 40, 20), c(0, 0, 0, 60, 210))
})

test_that("a limited layer cedes at most the limit above the deductible", {
  claims <- c(0, 5, 40, 100, 250)
  expect_equal(apply_deductible_limit(claims, "Limited Layer", 40, 100), c(0, 0, 0, 60, 100))
})

test_that("an excluded layer keeps what the limited layer does not take", {
  claims <- c(0, 5, 40, 100, 250)
  ceded <- apply_deductible_limit(claims, "Limited Layer", 40, 100)
  retained <- apply_deductible_limit(claims, "Exclude Layer", 40, 100)
  expect_equal(retained, claims - ceded)
  expect_equal(retained, c(0, 5, 40, 40, 150))
})

test_that("the structure must be one of the options, given as a single string", {
  options_text <- "must be one of 'No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer'."
  expect_error(apply_deductible_limit(c(1, 2), "Something Else", 1, 1), "Unknown reinsurance structure")
  expect_error(apply_deductible_limit(c(1, 2), "Something Else", 1, 1), "(got 'Something Else')", fixed = TRUE)
  expect_error(apply_deductible_limit(c(1, 2), "limited", 1, 1), options_text, fixed = TRUE)
  #NA and two structures used to give "missing value where TRUE/FALSE needed" and
  #"the condition has length > 1"
  for (bad in list(NA, NA_character_, c("Limited Layer", "Exclude Layer"), character(0), NULL, 1)) {
    expect_error(apply_deductible_limit(1:3, bad, 1, 1), options_text, fixed = TRUE, info = deparse(bad))
  }
})

test_that("a negative deductible or limit is an error", {
  #a limit of -20 used to give -20 for every claim
  expect_error(apply_deductible_limit(c(100, 50, 20), "Limited Layer", 40, -20), "limit must not be negative")
  expect_error(apply_deductible_limit(c(100, 50, 20), "Exclude Layer", 40, -20), "limit must not be negative")
  expect_error(apply_deductible_limit(c(100, 50, 20), "Unlimited Layer", -1, 20), "deductible must not be negative")
  expect_error(apply_deductible_limit(c(100, 50, 20), "Limited Layer", -1, 20), "deductible must not be negative")
  #amounts a structure does not use are not checked
  expect_equal(apply_deductible_limit(100, "Unlimited Layer", 40, -5), 60)
  expect_identical(apply_deductible_limit(c(1, 2), "No Reinsurance Structure", -1, -1), c(1, 2))
  #zero and infinite amounts are valid
  expect_equal(apply_deductible_limit(c(100, 50), "Limited Layer", 0, Inf), c(100, 50))
  expect_equal(apply_deductible_limit(c(100, 50), "Limited Layer", Inf, 10), c(0, 0))
})

test_that("the functions are vectorised and keep zero-length input", {
  expect_equal(apply_deductible_limit(numeric(0), "Limited Layer", 1, 1), numeric(0))
  expect_equal(apply_deductible_limit(1000, "Unlimited Layer", 0, NA), 1000)
})
