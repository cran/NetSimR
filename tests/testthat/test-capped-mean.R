#reference values from the package's original example script (tests/testthat/test1.R)

test_that("Log-Normal capped mean, exposure curve and ILF", {
  expect_equal(LNormCappedMean(1000, c(6, 6.5, 7), 1.5), c(502.9684, 617.6103, 724.4991), tolerance = 1e-6)
  expect_equal(ExposureCurveLNorm(1000, 6, 1.5), 0.4047553, tolerance = 1e-6)
  expect_equal(ExposureCurveLNorm(c(1000, 1100, 1200), 6, c(1.5, 1.5, 1.6)),
               c(0.4047553, 0.4258355, 0.3840963), tolerance = 1e-6)
  expect_equal(ILFLNorm(1000, c(1200, 1300, 1400), 6, 1.5), c(1.100310, 1.145149, 1.186982), tolerance = 1e-6)
})

test_that("Gamma capped mean, exposure curve and ILF", {
  shape <- 1000 * 1000 / 100 / 100
  scale <- 1000 / 100 / 100
  expect_equal(GammaCappedMean(1000, shape, scale), 960.139, tolerance = 1e-6)
  expect_equal(ExposureCurveGamma(1000, shape, scale), 0.960139, tolerance = 1e-6)
  expect_equal(ExposureCurveGamma(c(1000, 500, 200), shape, scale), c(0.960139, 0.5, 0.2), tolerance = 1e-6)
  expect_equal(ILFGamma(500, 1000, shape, scale), 1.920278, tolerance = 1e-6)
})

test_that("Pareto capped mean, exposure curve and ILF", {
  expect_equal(ParetoCappedMean(800, 100, 1.1), 287.7476, tolerance = 1e-6)
  expect_equal(ParetoCappedMean(c(800, 900, 1000), 100, 1.1), c(287.7476, 297.2584, 305.6718), tolerance = 1e-6)
  expect_equal(ExposureCurvePareto(800, 100, 1.1), 0.2615887, tolerance = 1e-6)
  expect_equal(ILFPareto(800, 900, 100, 1.1), 1.033053, tolerance = 1e-6)
  expect_equal(ILFPareto(800, 900, c(100, 110), 1.1), c(1.033053, 1.034296), tolerance = 1e-6)
})

test_that("sliced Log-Normal Pareto mean and exposure curve", {
  expect_equal(SlicedLNormParetoMean(6, 1.5, 2000, 1.8), 1056.475, tolerance = 1e-6)
  expect_equal(SlicedLNormParetoMean(c(5, 5.5, 6), 1.5, 1000, 1.1), c(1306.190, 2130.408, 3228.298), tolerance = 1e-6)
  expect_equal(ExposureCurveSlicedLNormPareto(c(800, 900, 1000), 5, 1.5, 1000, 1.1),
               c(0.2036355, 0.2130104, 0.2212809), tolerance = 1e-6)
})

test_that("pure IBNR with a Log-Normal reporting delay", {
  dates <- data.frame(
    inceptionDate = as.POSIXct(c("01/01/2006", "01/07/2006", "01/01/2007"), format = "%d/%m/%Y"),
    expiryDate = as.POSIXct(c("31/12/2006", "30/06/2007", "31/12/2007"), format = "%d/%m/%Y")
  )
  valuation <- as.POSIXct("30/10/2007", format = "%d/%m/%Y")
  ibnr <- PureIBNRLNorm(dates$inceptionDate, dates$expiryDate, valuation, 4, 1.5)
  expect_s3_class(ibnr, "data.frame")
  expect_equal(ibnr$UnearnedDuration, c(0, 0, 62))
  expect_equal(ibnr$PureIBNRDuration, c(28.12, 52.17, 98.84), tolerance = 1e-3)
  expect_equal(ibnr$UnearnedDurationRatio, c(0, 0, 0.17033), tolerance = 1e-3)
  expect_equal(ibnr$PureIBNRDurationRatio, c(0.07725, 0.14332, 0.27154), tolerance = 1e-3)
})
