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

test_that("an unknown structure is an error", {
  expect_error(apply_deductible_limit(c(1, 2), "Something Else", 1, 1), "Unknown reinsurance structure")
})

test_that("the functions are vectorised and keep zero-length input", {
  expect_equal(apply_deductible_limit(numeric(0), "Limited Layer", 1, 1), numeric(0))
  expect_equal(apply_deductible_limit(1000, "Unlimited Layer", 0, NA), 1000)
})
