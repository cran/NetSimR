#pure IBNR and unearned exposure from a reporting delay distribution

example_dates <- function() {
  list(
    inception = as.POSIXct(c("01/01/2006", "01/07/2006", "01/01/2007"), format = "%d/%m/%Y"),
    expiry = as.POSIXct(c("31/12/2006", "30/06/2007", "31/12/2007"), format = "%d/%m/%Y"),
    valuation = as.POSIXct("30/10/2007", format = "%d/%m/%Y")
  )
}

test_that("PureIBNRGamma reproduces the documented example", {
  d <- example_dates()
  ibnr <- PureIBNRGamma(d$inception, d$expiry, d$valuation, 7, 0.15)
  expect_s3_class(ibnr, "data.frame")
  expect_named(ibnr, c("UnearnedDuration", "PureIBNRDuration", "UnearnedDurationRatio", "PureIBNRDurationRatio"))
  expect_equal(ibnr$UnearnedDuration, c(0, 0, 62))
  expect_equal(ibnr$PureIBNRDuration, c(0, 0.01, 46.67))
  expect_equal(ibnr$UnearnedDurationRatio, c(0, 0, 0.17033))
  expect_equal(ibnr$PureIBNRDurationRatio, c(0, 0.00003, 0.12821))
})

test_that("PureIBNRLNorm reproduces the documented example", {
  d <- example_dates()
  ibnr <- PureIBNRLNorm(d$inception, d$expiry, d$valuation, 4, 1.5)
  expect_equal(ibnr$UnearnedDuration, c(0, 0, 62))
  expect_equal(ibnr$PureIBNRDuration, c(28.12, 52.17, 98.84))
  expect_equal(ibnr$UnearnedDurationRatio, c(0, 0, 0.17033))
  expect_equal(ibnr$PureIBNRDurationRatio, c(0.07725, 0.14332, 0.27154))
})

test_that("pure IBNR is the capped mean of the delay between the earned window and the valuation date", {
  d <- example_dates()
  #the third period is still running at the valuation date: delays range from 0 to 302 days
  max_delay <- as.numeric(difftime(d$valuation, d$inception[3], units = "days"))
  expect_equal(max_delay, 302)
  expect_equal(PureIBNRGamma(d$inception[3], d$expiry[3], d$valuation, 7, 0.15)$PureIBNRDuration, round(GammaCappedMean(302, 7, 0.15), 2))
  expect_equal(PureIBNRLNorm(d$inception[3], d$expiry[3], d$valuation, 4, 1.5)$PureIBNRDuration, round(LNormCappedMean(302, 4, 1.5), 2))
  #an expired period: delays range from valuation - expiry to valuation - inception
  lo <- as.numeric(difftime(d$valuation, d$expiry[1], units = "days"))
  hi <- as.numeric(difftime(d$valuation, d$inception[1], units = "days"))
  expect_equal(PureIBNRLNorm(d$inception[1], d$expiry[1], d$valuation, 4, 1.5)$PureIBNRDuration, round(LNormCappedMean(hi, 4, 1.5) - LNormCappedMean(lo, 4, 1.5), 2))
})

test_that("pure IBNR edge cases", {
  d <- example_dates()
  #a period that has not started yet is fully unearned with no IBNR
  future <- PureIBNRGamma(d$inception[3], d$expiry[3], as.POSIXct("2006-06-30"), 7, 0.15)
  expect_equal(future$UnearnedDuration, 364)
  expect_equal(future$UnearnedDurationRatio, 1)
  expect_equal(future$PureIBNRDuration, 0)
  expect_equal(future$PureIBNRDurationRatio, 0)
  #a period that expired long ago has (almost) no pure IBNR left
  old <- PureIBNRLNorm(d$inception[1], d$expiry[1], as.POSIXct("2030-01-01"), 4, 1.5)
  expect_equal(old$UnearnedDuration, 0)
  expect_lt(old$PureIBNRDuration, 1)
  #a zero-length period gives zero ratios rather than NaN
  zero <- PureIBNRGamma(d$inception[1], d$inception[1], d$valuation, 7, 0.15)
  expect_equal(unlist(zero), c(UnearnedDuration = 0, PureIBNRDuration = 0, UnearnedDurationRatio = 0, PureIBNRDurationRatio = 0))
})
