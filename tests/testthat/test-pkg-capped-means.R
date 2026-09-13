#capped means, exposure curves and ILFs of the Gamma, LogNormal and Pareto severity distributions

#numerical reference: E[min(X, cap)] is the integral of the survival function from 0 to cap
capped_mean_reference <- function(survival, cap) {
  integrate(survival, 0, cap, rel.tol = 1e-10, subdivisions = 1000L)$value
}

test_that("GammaCappedMean matches the documented values and numerical integration", {
  expect_equal(GammaCappedMean(700, 1, 0.0005), 590.623820562573, tolerance = 1e-9)
  expect_equal(GammaCappedMean(1000, 1.5, 0.0006), 890.810899151153, tolerance = 1e-9)
  for (case in list(c(500, 0.5, 5e-4), c(2000, 1.5, 6e-4), c(1e5, 7, 1e-3), c(1000, 50, 0.15))) {
    reference <- capped_mean_reference(function(x) pgamma(x, case[2], case[3], lower.tail = FALSE), case[1])
    expect_equal(GammaCappedMean(case[1], case[2], case[3]), reference, tolerance = 1e-8)
  }
})

test_that("GammaCappedMean handles edge cases", {
  #a cap of zero caps everything, an infinite cap caps nothing
  expect_equal(GammaCappedMean(0, 1.5, 6e-4), 0)
  expect_equal(GammaCappedMean(Inf, 1.5, 6e-4), 1.5 / 6e-4)
  #large shapes used to overflow gamma() and return NaN
  expect_equal(GammaCappedMean(1000, 200, 0.1), 1000, tolerance = 1e-9)
  expect_true(is.finite(GammaCappedMean(3000, 200, 0.1)))
  #tiny caps are returned (almost) unchanged rather than lost to cancellation
  expect_equal(GammaCappedMean(1e-6, 50, 5e-4), 1e-6, tolerance = 1e-9)
  #every argument is recycled, including a single infinite cap
  expect_equal(GammaCappedMean(c(500, 1000), 1.5, 6e-4), c(GammaCappedMean(500, 1.5, 6e-4), GammaCappedMean(1000, 1.5, 6e-4)))
  expect_equal(GammaCappedMean(Inf, c(1, 2), 1), c(1, 2))
  expect_true(is.na(GammaCappedMean(NA, 1, 1)))
  expect_error(GammaCappedMean("a", 1, 1))
})

test_that("Gamma exposure curve and ILF are ratios of capped means", {
  expect_equal(ExposureCurveGamma(700, 1, 0.0005), GammaCappedMean(700, 1, 0.0005) * 0.0005 / 1)
  expect_equal(ExposureCurveGamma(Inf, 1.5, 6e-4), 1)
  expect_equal(ILFGamma(1000, 700, 1, 0.0005), GammaCappedMean(700, 1, 0.0005) / GammaCappedMean(1000, 1, 0.0005))
  expect_equal(ILFGamma(500, 500, 1.5, 6e-4), 1)
})

test_that("LNormCappedMean matches the documented values and numerical integration", {
  expect_equal(LNormCappedMean(2000, 6, 1.5), 699.161503960373, tolerance = 1e-9)
  expect_equal(LNormCappedMean(1000, 5, 1.6), 298.992474726974, tolerance = 1e-9)
  for (case in list(c(1000, 6, 1.5), c(5000, 5, 1.6), c(100, 4, 0.5), c(1e5, 8, 2))) {
    reference <- capped_mean_reference(function(x) plnorm(x, case[2], case[3], lower.tail = FALSE), case[1])
    expect_equal(LNormCappedMean(case[1], case[2], case[3]), reference, tolerance = 1e-8)
  }
})

test_that("LNormCappedMean handles edge cases", {
  expect_equal(LNormCappedMean(0, 6, 1.5), 0)
  expect_equal(LNormCappedMean(Inf, 6, 1.5), exp(6 + 1.5^2 / 2))
  #a cap far above the mean gives the mean instead of losing precision to cancellation
  expect_equal(LNormCappedMean(1e12, -2, 0.5), exp(-2 + 0.5^2 / 2), tolerance = 1e-12)
  expect_equal(LNormCappedMean(1000, c(6, 6.5, 7), 1.5), c(502.9684, 617.6103, 724.4991), tolerance = 1e-6)
  expect_equal(LNormCappedMean(Inf, c(0, 1), 1), exp(c(0.5, 1.5)))
  expect_true(is.na(LNormCappedMean(NA, 6, 1.5)))
})

test_that("LogNormal exposure curve, ILF and erf", {
  expect_equal(ExposureCurveLNorm(2000, 6, 1.5), LNormCappedMean(2000, 6, 1.5) / exp(6 + 1.5^2 / 2))
  expect_equal(ExposureCurveLNorm(Inf, 6, 1.5), 1)
  expect_equal(ILFLNorm(1000, 2000, 6, 1.5), LNormCappedMean(2000, 6, 1.5) / LNormCappedMean(1000, 6, 1.5))
  x <- seq(-3, 3, by = 0.5)
  expect_equal(erf(x), 2 * pnorm(x * sqrt(2)) - 1)
  expect_equal(erf(0), 0)
  expect_equal(erf(-x), -erf(x))
})

test_that("ParetoCappedMean matches the documented values and the closed form", {
  expect_equal(ParetoCappedMean(600, 200, 1.2), 397.258438239770, tolerance = 1e-9)
  expect_equal(ParetoCappedMean(1000, 500, 0.8), 871.745887492587, tolerance = 1e-9)
  #general closed form for shape != 1
  cap <- c(800, 900, 1000); scale <- 100; shape <- 1.1
  expect_equal(ParetoCappedMean(cap, scale, shape), (shape * scale - cap * (scale / cap)^shape) / (shape - 1))
  expect_equal(ParetoCappedMeanCalc(cap, scale, shape), ParetoCappedMean(cap, scale, shape))
  #numerical reference: X has survival (scale / x)^shape above scale, so E[min(X, cap)] = scale + integral
  reference <- 100 + integrate(function(x) (100 / x)^1.1, 100, 800)$value
  expect_equal(ParetoCappedMean(800, 100, 1.1), reference, tolerance = 1e-8)
})

test_that("ParetoCappedMean at shape == 1 uses the exact limit and is continuous", {
  #E[min(X, cap)] = scale * (1 + log(cap / scale)) when shape == 1
  expect_equal(ParetoCappedMean(800, 100, 1), 100 * (1 + log(8)))
  expect_equal(ParetoCappedMean(c(800, 900, 1000), 100, 1), 100 * (1 + log(c(8, 9, 10))))
  reference <- 100 + integrate(function(x) 100 / x, 100, 800)$value
  expect_equal(ParetoCappedMean(800, 100, 1), reference, tolerance = 1e-10)
  #the documented example used to be approximated by averaging shape +/- 0.0001
  expect_equal(ParetoCappedMean(800, 100, 1), 307.944, tolerance = 1e-6)
  expect_lt(abs(ParetoCappedMean(1000, 100, 1 + 1e-7) - ParetoCappedMean(1000, 100, 1)), 1e-3)
  expect_lt(abs(ParetoCappedMean(1000, 100, 1 - 1e-7) - ParetoCappedMean(1000, 100, 1)), 1e-3)
  #shape == 1 mixed with other shapes in one call
  expect_equal(ParetoCappedMean(c(600, 800), 200, c(1.2, 1)), c(ParetoCappedMean(600, 200, 1.2), 200 * (1 + log(4))))
})

test_that("ParetoCappedMean handles infinite caps and invalid input", {
  #an infinite cap gives the Pareto mean when it exists and Inf otherwise
  expect_equal(ParetoCappedMean(Inf, 100, 1.2), 1.2 * 100 / 0.2)
  expect_equal(ParetoCappedMean(Inf, 100, c(1.2, 1, 0.8)), c(600, Inf, Inf))
  expect_equal(ParetoCappedMeanCalc(Inf, c(100, 200), 1.5), c(300, 600))
  expect_error(ParetoCappedMean(1:2, 1:3, 1))
  expect_true(is.na(ParetoCappedMean(NA, 100, 1.2)))
})

test_that("Pareto exposure curve and ILF", {
  expect_equal(ExposureCurvePareto(700, 500, 1.2), ParetoCappedMean(700, 500, 1.2) * 0.2 / 1.2 / 500)
  expect_equal(ExposureCurvePareto(800, 100, 1.1), 0.2615887, tolerance = 1e-6)
  #no finite mean, so no exposure curve, when shape <= 1
  expect_equal(ExposureCurvePareto(c(700, 700), 500, c(1, 0.8)), c(0, 0))
  expect_equal(ExposureCurvePareto(Inf, 500, 1.2), 1)
  expect_equal(ILFPareto(700, 1200, 500, 1.2), ParetoCappedMean(1200, 500, 1.2) / ParetoCappedMean(700, 500, 1.2))
  expect_equal(ILFPareto(800, 900, c(100, 110), 1.1), c(1.033053, 1.034296), tolerance = 1e-6)
})

test_that("IGamma is the upper incomplete gamma function", {
  expect_equal(IGamma(1, 1), exp(-1))
  expect_equal(IGamma(0.1, 2), gamma(0.1) * pgamma(2, 0.1, lower.tail = FALSE))
  expect_equal(IGamma(2.5, 0), gamma(2.5))
  expect_equal(IGamma(50, 30), gamma(50) * pgamma(30, 50, lower.tail = FALSE))
})

test_that("IGamma does not overflow when gamma(a) does but the result is finite", {
  #gamma(180) overflows, so this used to return Inf; the true value is about 6.18e102
  a <- 180; x <- 1000
  #numerical reference on the log scale: integral of t^(a - 1) exp(-t) from x to Inf, scaled by its value at x
  logScale <- (a - 1) * log(x) - x
  integral <- integrate(function(t) exp((a - 1) * log(t) - t - logScale), x, Inf, rel.tol = 1e-12)$value
  expect_true(is.finite(IGamma(a, x)))
  expect_equal(log(IGamma(a, x)), logScale + log(integral), tolerance = 1e-10)
  #a true value beyond the double range is still Inf, now without a warning from gamma()
  expect_no_warning(expect_equal(IGamma(200, 150), Inf))
})

test_that("the ILFGamma example has xLow below xHigh", {
  expect_gt(ILFGamma(700, 1000, 1, 0.0005), 1)
  expect_gt(ILFGamma(1000, 1200, 1.5, 0.0006), 1)
})

test_that("ParetoCappedMean returns the cap when it is at or below the scale", {
  #no claim is below the scale, so min(X, cap) = cap
  expect_equal(ParetoCappedMean(50, 100, 2), 50)
  expect_equal(ParetoCappedMean(80, 100, 2), 80)
  expect_equal(ParetoCappedMean(50, 100, 1), 50)
  expect_equal(ParetoCappedMean(0, 100, 2), 0)
  expect_equal(ParetoCappedMean(100, 100, c(0.5, 1, 2)), c(100, 100, 100))
  #mixed with caps above the scale in one call, every argument recycled
  expect_equal(ParetoCappedMean(c(50, 800, 80), c(100, 100, 200), c(2, 1.1, 1)),
               c(50, ParetoCappedMean(800, 100, 1.1), 80))
  #numerical reference: the integral of the survival function, which is 1 below the scale
  for (case in list(c(50, 100, 2), c(80, 100, 0.5), c(150, 100, 2), c(5000, 100, 0.7), c(5000, 100, 1 + 1e-4))) {
    survival <- function(x) ifelse(x < case[2], 1, (case[2] / x)^case[3])
    reference <- integrate(survival, 0, case[1], rel.tol = 1e-11, subdivisions = 1000L)$value
    expect_equal(ParetoCappedMean(case[1], case[2], case[3]), reference, tolerance = 1e-8)
  }
  #the exposure curve and the ILF use the corrected capped mean
  expect_equal(ExposureCurvePareto(50, 100, 2), 0.25)
  expect_equal(ILFPareto(50, 150, 100, 2), (100 * (1 + (1 - 100 / 150))) / 50)
  expect_true(is.finite(ILFPareto(50, 150, 100, 2)))
})

test_that("ParetoCappedMean is accurate for shapes close to 1", {
  #series of scale * (1 + (exp(t * L) - 1) / t) in t = 1 - shape, with L = log(cap / scale)
  L <- log(8)
  for (e in c(1e-15, 1e-12, 1e-9, 1e-6, -1e-6, -1e-12)) {
    t <- -e
    reference <- 100 * (1 + L + t * L^2 / 2 + t^2 * L^3 / 6)
    expect_equal(ParetoCappedMean(800, 100, 1 + e), reference, tolerance = 1e-13)
  }
  #shape 1 + 1e-12 used to give 307.9503 instead of 307.9442 through cancellation
  expect_lt(abs(ParetoCappedMean(800, 100, 1 + 1e-12) - 100 * (1 + L)), 1e-9)
  #an infinite cap gives the mean scale * shape / (shape - 1), using the shape as stored in double precision
  shape <- 1 + 1e-12
  expect_equal(ParetoCappedMean(Inf, 100, shape), 100 * shape / (shape - 1), tolerance = 1e-12)
})
