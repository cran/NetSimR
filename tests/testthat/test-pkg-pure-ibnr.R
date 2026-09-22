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
  #the ratios come from the unrounded durations (dividing the durations rounded to 0.01 days
  #gave 0.00003 for the second period)
  expect_equal(ibnr$PureIBNRDurationRatio, c(0, 0.00002, 0.12821))
})

test_that("PureIBNRLNorm reproduces the documented example", {
  d <- example_dates()
  ibnr <- PureIBNRLNorm(d$inception, d$expiry, d$valuation, 4, 1.5)
  expect_equal(ibnr$UnearnedDuration, c(0, 0, 62))
  #the second period's delays used to gain an hour across daylight saving changes, giving
  #52.17 (Europe), 52.18 (UTC) or 52.19 (Sydney) depending on the machine's time zone
  expect_equal(ibnr$PureIBNRDuration, c(28.12, 52.18, 98.84))
  expect_equal(ibnr$UnearnedDurationRatio, c(0, 0, 0.17033))
  expect_equal(ibnr$PureIBNRDurationRatio, c(0.07726, 0.14336, 0.27155))
})

test_that("pure IBNR is the capped mean of the delay between the earned window and the valuation date", {
  d <- example_dates()
  #calendar days between two dates; difftime() counts elapsed hours, which differ by one
  #across a daylight saving change in time zones such as America/New_York
  calendar_days <- function(to, from) as.numeric(as.Date(format(to)) - as.Date(format(from)))
  #the third period is still running at the valuation date: delays range from 0 to 302 days
  max_delay <- calendar_days(d$valuation, d$inception[3])
  expect_equal(max_delay, 302)
  expect_equal(PureIBNRGamma(d$inception[3], d$expiry[3], d$valuation, 7, 0.15)$PureIBNRDuration, round(GammaCappedMean(302, 7, 0.15), 2))
  expect_equal(PureIBNRLNorm(d$inception[3], d$expiry[3], d$valuation, 4, 1.5)$PureIBNRDuration, round(LNormCappedMean(302, 4, 1.5), 2))
  #an expired period: delays range from valuation - expiry to valuation - inception
  lo <- calendar_days(d$valuation, d$expiry[1])
  hi <- calendar_days(d$valuation, d$inception[1])
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

test_that("Date, POSIXct and mixed dates give the same results", {
  expected <- PureIBNRGamma(as.Date("2020-01-01"), as.Date("2020-12-31"), as.Date("2021-02-15"), 7, 0.15)
  expect_equal(expected$UnearnedDuration, 0)
  expect_equal(expected$PureIBNRDuration, 7.26)
  #the reported case: POSIXct seconds were compared with Date days, giving Unearned 365 and IBNR 0
  mixed <- PureIBNRGamma(as.POSIXct("2020-01-01", tz = "UTC"), as.POSIXct("2020-12-31", tz = "UTC"), as.Date("2021-02-15"), 7, 0.15)
  expect_equal(mixed, expected)
  expect_equal(PureIBNRGamma(as.Date("2020-01-01"), as.Date("2020-12-31"), as.POSIXct("2021-02-15", tz = "Asia/Tokyo"), 7, 0.15), expected)
  for (tz in c("UTC", "Europe/London", "America/New_York", "Australia/Sydney")) {
    local <- function(x) as.POSIXct(x, tz = tz)
    expect_equal(PureIBNRGamma(local("2020-01-01"), local("2020-12-31"), local("2021-02-15"), 7, 0.15), expected)
  }
  expect_equal(PureIBNRLNorm(as.POSIXct("2020-01-01", tz = "UTC"), as.POSIXct("2020-12-31", tz = "UTC"), as.Date("2021-02-15"), 4, 1.5),
               PureIBNRLNorm(as.Date("2020-01-01"), as.Date("2020-12-31"), as.Date("2021-02-15"), 4, 1.5))
})

test_that("days are counted on the local clock, so daylight saving adds no fraction of a day", {
  #1 January (GMT) to 30 June (BST) is 181 days, not 180.9583
  london <- function(x) as.POSIXct(x, tz = "Europe/London")
  ibnr <- PureIBNRGamma(london("2020-01-01"), london("2020-06-30"), london("2020-05-01"), 7, 0.15)
  expect_equal(ibnr, PureIBNRGamma(as.Date("2020-01-01"), as.Date("2020-06-30"), as.Date("2020-05-01"), 7, 0.15))
  expect_equal(ibnr$UnearnedDuration, 60)
  expect_equal(ibnr$UnearnedDurationRatio, round(60 / 181, 5))
  #a time of day still counts as a fraction of a day
  noon <- PureIBNRGamma(as.Date("2020-01-01"), as.Date("2020-12-31"), as.POSIXct("2020-12-30 12:00", tz = "UTC"), 7, 0.15)
  expect_equal(noon$UnearnedDuration, 0.5)
})

test_that("pure IBNR refuses non-date input, periods that end before they start and invalid parameters", {
  d <- example_dates()
  #plain numbers would be read as seconds, and strings as dates in an unknown format
  expect_error(PureIBNRGamma(18262, 18627, 18673, 7, 0.15), "IncDate must be a Date or POSIXct date")
  expect_error(PureIBNRLNorm(d$inception, "2007-12-31", d$valuation, 4, 1.5), "ExpDate must be a Date or POSIXct date")
  expect_error(PureIBNRLNorm(d$inception, d$expiry, 13816, 4, 1.5), "ValDate must be a Date or POSIXct date")
  #an expiry before the inception used to give a negative pure IBNR
  expect_error(PureIBNRGamma(as.Date("2020-12-31"), as.Date("2020-01-01"), as.Date("2021-02-15"), 7, 0.15), "ExpDate must not be before IncDate")
  expect_error(PureIBNRLNorm(d$expiry, d$inception, d$valuation, 4, 1.5), "ExpDate must not be before IncDate")
  expect_error(PureIBNRGamma(d$inception, d$expiry, d$valuation, 7, -0.15), "rate must be positive")
  expect_error(PureIBNRGamma(d$inception, d$expiry, d$valuation, 0, 0.15), "shape must be positive")
  expect_error(PureIBNRLNorm(d$inception, d$expiry, d$valuation, 4, -1.5), "sigma must be positive")
  #missing dates give missing results
  missing <- PureIBNRLNorm(as.Date(c("2020-01-01", NA)), as.Date("2020-12-31"), as.Date("2021-02-15"), 4, 1.5)
  expect_false(anyNA(missing[1, ]))
  expect_true(all(is.na(missing[2, ])))
  expect_error(PureIBNRLNorm(d$inception, d$expiry, d$valuation, "4", 1.5), "mu must be numeric")
})

test_that("dates and parameters are recycled to one row each, and other lengths are an error", {
  d <- example_dates()
  #one period with two shapes: the ratios of the second row used to repeat the first row's
  two_shapes <- PureIBNRGamma(d$inception[3], d$expiry[3], d$valuation, c(7, 70), 0.15)
  expect_equal(nrow(two_shapes), 2)
  for (i in 1:2) {
    single <- PureIBNRGamma(d$inception[3], d$expiry[3], d$valuation, c(7, 70)[i], 0.15)
    expect_equal(unlist(two_shapes[i, ]), unlist(single))
  }
  #one period valued at two dates
  valuations <- as.POSIXct(c("30/06/2006", "30/10/2007"), format = "%d/%m/%Y")
  two_dates <- PureIBNRLNorm(d$inception[1], d$expiry[1], valuations, 4, 1.5)
  for (i in 1:2) {
    expect_equal(unlist(two_dates[i, ]), unlist(PureIBNRLNorm(d$inception[1], d$expiry[1], valuations[i], 4, 1.5)))
  }
  #lengths that do not recycle used to be recycled with a warning, or silently
  expect_error(PureIBNRGamma(d$inception, d$expiry[1:2], d$valuation, 7, 0.15), "cannot be recycled")
  expect_error(PureIBNRLNorm(d$inception[1:2], d$expiry[1:2], d$valuation, c(4, 5, 6), 1.5), "cannot be recycled")
  err <- tryCatch(PureIBNRGamma(d$inception, d$expiry[1:2], d$valuation, 7, 0.15), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("PureIBNRGamma"))
})
