#regressions for the review of the analytic severity and pure IBNR functions

test_that("sliced quantiles of probabilities above 1 are NaN with a warning, whatever the Pareto shape", {
  #(1 - q)^(1 / PShape) is a real number for q > 1 when 1 / PShape is a whole number, so
  #these used to give -7357.59, 54134.1 and 2165.4, and -2725.3 without a warning
  expect_warning(g1 <- qSlicedGammaPareto(1.1, 2, 0.001, 1000, 1), "NaNs produced")
  expect_true(is.nan(g1))
  expect_warning(g2 <- qSlicedGammaPareto(c(1.1, 1.5), 2, 0.001, 1000, 0.5), "NaNs produced")
  expect_true(all(is.nan(g2)))
  expect_warning(l1 <- qSlicedLNormPareto(1.1, 6, 1.5, 1000, 1), "NaNs produced")
  expect_true(is.nan(l1))
  for (shape in c(1, 0.5, 1 / 3, 1.5)) {
    expect_true(all(is.nan(suppressWarnings(qSlicedGammaPareto(c(1.1, 2, Inf), 2, 0.001, 1000, shape)))), info = shape)
    expect_true(all(is.nan(suppressWarnings(qSlicedLNormPareto(c(1.1, 2, Inf), 6, 1.5, 1000, shape)))), info = shape)
  }
  #a vector with valid and invalid probabilities gives NaN only in the invalid positions
  valid <- c(0.5, 0.99, 1)
  expect_warning(g <- qSlicedGammaPareto(c(-0.1, valid, 1.1, NA), 2, 0.001, 1000, 1), "NaNs produced")
  expect_true(is.nan(g[1]) && is.nan(g[5]) && is.na(g[6]) && !is.nan(g[6]))
  expect_equal(g[2:4], qSlicedGammaPareto(valid, 2, 0.001, 1000, 1), tolerance = 1e-10)
  expect_equal(g[4], Inf)
  expect_warning(l <- qSlicedLNormPareto(c(-0.1, valid, 1.1, NA), 6, 1.5, 1000, 1), "NaNs produced")
  expect_true(is.nan(l[1]) && is.nan(l[5]) && is.na(l[6]) && !is.nan(l[6]))
  expect_equal(l[2:4], qSlicedLNormPareto(valid, 6, 1.5, 1000, 1), tolerance = 1e-10)
  #the valid quantiles still invert the cdf above the slice point
  expect_equal(pSlicedGammaPareto(g[2:3], 2, 0.001, 1000, 1), valid[1:2], tolerance = 1e-10)
  expect_equal(pSlicedLNormPareto(l[2:3], 6, 1.5, 1000, 1), valid[1:2], tolerance = 1e-10)
})

test_that("the sliced capped mean at an infinite cap is the mean, also when the tail weight underflows", {
  #the slice point is so far into the attritional tail that S(SlicePoint) is 0, and
  #0 * ParetoCappedMean(Inf) used to give NaN for a Pareto shape at or below 1
  expect_equal(pgamma(1e6, 2, 0.1, lower.tail = FALSE), 0)
  expect_equal(plnorm(1e30, 6, 1.5, lower.tail = FALSE), 0)
  for (shape in c(0.8, 1, 1.5)) {
    expect_equal(SlicedGammaParetoCappedMean(Inf, 2, 0.1, 1e6, shape), SlicedGammaParetoMean(2, 0.1, 1e6, shape), tolerance = 1e-10, info = shape)
    expect_equal(SlicedLNormParetoCappedMean(Inf, 6, 1.5, 1e30, shape), SlicedLNormParetoMean(6, 1.5, 1e30, shape), tolerance = 1e-10, info = shape)
    #and when the tail has weight
    expect_equal(SlicedGammaParetoCappedMean(Inf, 1.1, 0.0006, 2000, shape), SlicedGammaParetoMean(1.1, 0.0006, 2000, shape), tolerance = 1e-10, info = shape)
    expect_equal(SlicedLNormParetoCappedMean(Inf, 6.5, 1.4, 2000, shape), SlicedLNormParetoMean(6.5, 1.4, 2000, shape), tolerance = 1e-10, info = shape)
  }
  expect_equal(SlicedGammaParetoCappedMean(Inf, 2, 0.1, 1e6, c(0.8, 1)), c(Inf, Inf))
  expect_equal(SlicedGammaParetoCappedMean(Inf, 2, 0.1, 1e6, 1.5), 20, tolerance = 1e-10)
  #a finite cap above such a slice point is still the attritional capped mean
  expect_equal(SlicedGammaParetoCappedMean(5000, 2, 0.1, 1e6, c(0.8, 1, 1.5)), rep(GammaCappedMean(5000, 2, 0.1), 3), tolerance = 1e-10)
  expect_equal(SlicedLNormParetoCappedMean(5000, 6, 1.5, 1e30, c(0.8, 1, 1.5)), rep(LNormCappedMean(5000, 6, 1.5), 3), tolerance = 1e-10)
  #the whole vector, with finite and infinite caps and shapes on both sides of 1
  expect_equal(SlicedGammaParetoCappedMean(c(Inf, 5000, Inf, 5000), 1.1, 0.0006, 2000, c(0.8, 0.8, 1.6, 1.6)),
               c(Inf, SlicedGammaParetoCappedMean(5000, 1.1, 0.0006, 2000, 0.8), SlicedGammaParetoMean(1.1, 0.0006, 2000, 1.6), SlicedGammaParetoCappedMean(5000, 1.1, 0.0006, 2000, 1.6)), tolerance = 1e-10)
})

test_that("the LogNormal exposure curve is 1 at an infinite amount, also when the mean overflows", {
  #LNormCappedMean(Inf, 0, 40) and the mean exp(0 + 40^2 / 2) are both Inf, so this used to give NaN
  expect_equal(exp(0 + 0.5 * 40 * 40), Inf)
  expect_equal(ExposureCurveLNorm(Inf, 0, 40), 1)
  expect_equal(ExposureCurveLNorm(Inf, 6, 1.5), 1)
  #a finite amount with an overflowing mean still gives 0, and a finite mean is unchanged
  expect_equal(ExposureCurveLNorm(1000, 0, 40), 0)
  expect_equal(ExposureCurveLNorm(c(1000, Inf, 0), 6, 1.5), c(LNormCappedMean(1000, 6, 1.5) / exp(6 + 0.5 * 1.5^2), 1, 0), tolerance = 1e-10)
  expect_true(is.na(ExposureCurveLNorm(c(NA, Inf), 6, 1.5)[1]))
})

test_that("the exposure curves' recycling errors name the function that was called", {
  #these used to name GammaCappedMean(x, shape, rate) and LNormCappedMean(x, mu, sigma)
  err <- tryCatch(ExposureCurveGamma(1:2, 1:3, 1), error = identity)
  expect_match(conditionMessage(err), "cannot be recycled")
  expect_identical(conditionCall(err)[[1]], as.name("ExposureCurveGamma"))
  err <- tryCatch(ExposureCurveLNorm(1:2, 1:3, 1), error = identity)
  expect_match(conditionMessage(err), "cannot be recycled")
  expect_identical(conditionCall(err)[[1]], as.name("ExposureCurveLNorm"))
  #recycling keeps the shape of the input, as in the other exported functions
  m <- matrix(c(1000, 2000, 3000, 4000), 2)
  expect_equal(ExposureCurveGamma(m, 2, 0.001), matrix(GammaCappedMean(c(m), 2, 0.001) * 0.001 / 2, 2), tolerance = 1e-10)
  expect_named(ExposureCurveLNorm(c(low = 1000, high = 2000), 6, 1.5), c("low", "high"))
  expect_length(ExposureCurveGamma(1000, c(1, 2), c(0.001, 0.002, 0.003, 0.004)), 4)
})

test_that("the unearned duration carries no floating-point noise and zero-length input gives numeric columns", {
  #the unearned duration of a period expiring at 04:24:04 is 171 days and 15844 seconds; the
  #POSIXct fractions of a day used to leave about 1e-12 of noise after the subtractions
  utc <- function(x) as.POSIXct(x, tz = "UTC")
  expiry <- utc("2006-12-31 04:24:04")
  for (inception in list(utc("2006-01-01"), utc("2006-01-01 12:00:00"))) {
    for (ibnr in list(PureIBNRGamma(inception, expiry, utc("2006-07-13"), 7, 0.15), PureIBNRLNorm(inception, expiry, utc("2006-07-13"), 4, 1.5))) {
      expect_equal(ibnr$UnearnedDuration, 171 + 15844 / 86400, tolerance = 1e-10)
      expect_true(ibnr$UnearnedDuration == round(ibnr$UnearnedDuration, 10))
      expect_equal(ibnr$UnearnedDurationRatio, round(ibnr$UnearnedDuration / as.numeric(difftime(expiry, inception, units = "days")), 5), tolerance = 1e-10)
    }
  }
  #whole days are unchanged
  expect_equal(PureIBNRGamma(as.Date("2007-01-01"), as.Date("2007-12-31"), as.Date("2007-10-30"), 7, 0.15)$UnearnedDuration, 62)
  #the ratio columns used to be logical(0)
  for (empty in list(PureIBNRGamma(as.Date(character(0)), as.Date("2006-12-31"), as.Date("2007-10-30"), 7, 0.15),
                     PureIBNRLNorm(as.Date("2006-01-01"), as.Date("2006-12-31"), as.Date(character(0)), 4, 1.5))) {
    expect_equal(nrow(empty), 0)
    for (column in names(empty)) expect_identical(empty[[column]], numeric(0), info = column)
  }
})
