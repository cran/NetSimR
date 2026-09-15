#sliced Gamma-Pareto and sliced LogNormal-Pareto distributions

#the two families share the same interface: attritional parameters, a slice point and a Pareto shape
families <- list(
  GammaPareto = list(
    p = function(x, s, shape) pSlicedGammaPareto(x, 1.1, 0.0006, s, shape),
    d = function(x, s, shape) dSlicedGammaPareto(x, 1.1, 0.0006, s, shape),
    q = function(u, s, shape) qSlicedGammaPareto(u, 1.1, 0.0006, s, shape),
    capped = function(cap, s, shape) SlicedGammaParetoCappedMean(cap, 1.1, 0.0006, s, shape),
    mean = function(s, shape) SlicedGammaParetoMean(1.1, 0.0006, s, shape),
    exposure = function(x, s, shape) ExposureCurveSlicedGammaPareto(x, 1.1, 0.0006, s, shape),
    ilf = function(lo, hi, s, shape) ILFSlicedGammaPareto(lo, hi, 1.1, 0.0006, s, shape),
    attritional_capped = function(cap) GammaCappedMean(cap, 1.1, 0.0006),
    attritional_p = function(x) pgamma(x, 1.1, 0.0006)
  ),
  LNormPareto = list(
    p = function(x, s, shape) pSlicedLNormPareto(x, 6.5, 1.4, s, shape),
    d = function(x, s, shape) dSlicedLNormPareto(x, 6.5, 1.4, s, shape),
    q = function(u, s, shape) qSlicedLNormPareto(u, 6.5, 1.4, s, shape),
    capped = function(cap, s, shape) SlicedLNormParetoCappedMean(cap, 6.5, 1.4, s, shape),
    mean = function(s, shape) SlicedLNormParetoMean(6.5, 1.4, s, shape),
    exposure = function(x, s, shape) ExposureCurveSlicedLNormPareto(x, 6.5, 1.4, s, shape),
    ilf = function(lo, hi, s, shape) ILFSlicedLNormPareto(lo, hi, 6.5, 1.4, s, shape),
    attritional_capped = function(cap) LNormCappedMean(cap, 6.5, 1.4),
    attritional_p = function(x) plnorm(x, 6.5, 1.4)
  )
)

for (name in names(families)) {
  f <- families[[name]]
  s <- 2000
  shape <- 1.6

  test_that(paste(name, "cdf, pdf and quantile function are consistent"), {
    x <- c(100, 500, 1000, 1999, 2000, 2001, 5000, 20000, 1e6)
    p <- f$p(x, s, shape)
    expect_true(all(diff(p) >= 0))
    expect_true(all(p >= 0 & p <= 1))
    expect_equal(f$p(0, s, shape), 0)
    expect_equal(f$p(Inf, s, shape), 1)
    #below the slice point the attritional distribution applies, the cdf is continuous at the slice point
    expect_equal(f$p(c(500, 1500, 2000), s, shape), f$attritional_p(c(500, 1500, 2000)))
    expect_equal(f$p(2000 + 1e-9, s, shape), f$p(2000, s, shape), tolerance = 1e-8)
    #the quantile function inverts the cdf on both sides of the slice point
    u <- c(0.05, 0.3, 0.5, 0.8, 0.95, 0.999)
    expect_equal(f$p(f$q(u, s, shape), s, shape), u)
    expect_equal(f$q(f$p(x, s, shape), s, shape), x, tolerance = 1e-8)
    #the pdf integrates to the cdf
    for (upper in c(1000, 3000, 30000)) {
      expect_equal(integrate(function(t) f$d(t, s, shape), 0, upper, rel.tol = 1e-10, subdivisions = 1000L)$value, f$p(upper, s, shape), tolerance = 1e-8)
    }
    #the Pareto tail has survival S(s) * (s / x)^shape
    expect_equal(1 - f$p(4000, s, shape), (1 - f$attritional_p(s)) * (s / 4000)^shape)
  })

  test_that(paste(name, "capped mean, mean, exposure curve and ILF are consistent"), {
    #below the slice point only the attritional distribution matters
    expect_equal(f$capped(c(500, 1500, 2000), s, shape), f$attritional_capped(c(500, 1500, 2000)))
    #numerical reference: E[min(X, cap)] is the integral of the survival function
    for (cap in c(1000, 3000, 20000)) {
      reference <- integrate(function(t) 1 - f$p(t, s, shape), 0, cap, rel.tol = 1e-10, subdivisions = 1000L)$value
      expect_equal(f$capped(cap, s, shape), reference, tolerance = 1e-8)
    }
    #the capped mean converges to the mean, and an infinite cap gives the mean directly
    expect_equal(f$capped(1e15, s, shape), f$mean(s, shape), tolerance = 1e-6)
    expect_equal(f$capped(Inf, s, shape), f$mean(s, shape))
    expect_equal(f$exposure(3000, s, shape), f$capped(3000, s, shape) / f$mean(s, shape))
    expect_equal(f$exposure(Inf, s, shape), 1)
    expect_equal(f$ilf(1000, 3000, s, shape), f$capped(3000, s, shape) / f$capped(1000, s, shape))
    expect_equal(f$ilf(3000, 3000, s, shape), 1)
    #a Pareto tail with shape <= 1 has no finite mean, so no exposure curve
    expect_equal(f$mean(s, c(1, 0.8)), c(Inf, Inf))
    expect_equal(f$exposure(c(3000, 3000), s, c(1, 0.8)), c(0, 0))
    #but the capped mean is still finite and increases with the cap
    capped <- f$capped(c(3000, 5000, 1e5), s, 1)
    expect_true(all(is.finite(capped)) && all(diff(capped) > 0))
    #vector inputs are recycled
    expect_equal(f$capped(c(1000, 3000), c(2000, 2000), c(1.6, 1.6)), c(f$capped(1000, s, shape), f$capped(3000, s, shape)))
  })
}

test_that("sliced Gamma-Pareto documented examples give the same values as before", {
  expect_equal(SlicedGammaParetoMean(1, 0.0005, 1000, 1.2), 3819.59197913790, tolerance = 1e-9)
  expect_equal(SlicedGammaParetoCappedMean(3000, 1, 0.0005, 1000, 1.2), 1385.15513397199, tolerance = 1e-9)
  expect_equal(ExposureCurveSlicedGammaPareto(3000, 1, 0.0005, 1000, 1.2), 0.362644790736163, tolerance = 1e-9)
  expect_equal(ILFSlicedGammaPareto(2000, 3000, 1, 0.0005, 1000, 1.2), 1.17434401145585, tolerance = 1e-9)
  expect_equal(pSlicedGammaPareto(3000, 1, 0.0005, 1000, 1.2), 0.837704210322272, tolerance = 1e-9)
  expect_equal(qSlicedGammaPareto(0.5, 1, 0.0005, 1000, 1.2), 1174.63326478321, tolerance = 1e-9)
  expect_equal(dSlicedGammaPareto(3000, 1, 0.0005, 1000, 1.2), 6.49183158710909e-05, tolerance = 1e-9)
})

test_that("sliced functions recycle every argument, including vector parameters", {
  #one value per row, so each argument has its own length; ifelse() used to take its length
  #from the claim amount and slice point only and drop the other parameters' extra values
  elementwise <- function(f, ...) {
    args <- data.frame(...)
    vapply(seq_len(nrow(args)), function(i) do.call(f, unname(as.list(args[i, ]))), numeric(1))
  }
  check <- function(f, ...) expect_equal(f(...), elementwise(f, ...), tolerance = 1e-12)
  shapes <- c(1.2, 2, 3)
  gamma_pars <- list(GShape = c(1.2, 2, 0.8), GRate = 4e-4, SlicePoint = 3000, PShape = 1.4)
  for (f in list(pSlicedGammaPareto, dSlicedGammaPareto, SlicedGammaParetoCappedMean, ExposureCurveSlicedGammaPareto)) {
    check(f, 5000, c(1.2, 2, 0.8), 4e-4, 3000, 1.4)
    check(f, 5000, 1.2, c(4e-4, 1e-3, 2e-4), 3000, 1.4)
    check(f, 5000, 1.2, 4e-4, 3000, shapes)
    check(f, c(1000, 5000), 1.2, 4e-4, 3000, c(1.4, 1.4, 2, 2))
  }
  check(qSlicedGammaPareto, 0.9, 1, 5e-4, 1000, shapes)
  check(qSlicedGammaPareto, 0.9, c(1, 2, 3), 5e-4, 1000, 1.2)
  check(SlicedGammaParetoMean, c(1.2, 2, 0.8), 4e-4, 3000, 1.4)
  check(SlicedGammaParetoMean, 1.2, 4e-4, 3000, shapes)
  check(ILFSlicedGammaPareto, 2000, 5000, c(1.2, 2, 0.8), 4e-4, 3000, 1.4)
  check(ILFSlicedGammaPareto, 2000, 5000, 1.2, 4e-4, 3000, shapes)
  for (f in list(pSlicedLNormPareto, dSlicedLNormPareto, SlicedLNormParetoCappedMean, ExposureCurveSlicedLNormPareto)) {
    check(f, 5000, c(6, 7, 7.5), 1.5, 3000, 1.4)
    check(f, 5000, 6, c(1.5, 1, 2), 3000, 1.4)
    check(f, 5000, 6, 1.5, 3000, shapes)
    check(f, c(1000, 5000), 6, 1.5, 3000, c(1.4, 1.4, 2, 2))
  }
  check(qSlicedLNormPareto, 0.9, 6, 1.5, 1000, shapes)
  check(qSlicedLNormPareto, 0.9, c(6, 7, 8), 1.5, 1000, 1.2)
  check(SlicedLNormParetoMean, 6, 1.5, 3000, shapes)
  check(ILFSlicedLNormPareto, 2000, 5000, c(6, 7, 7.5), 1.5, 3000, 1.4)
  check(ILFSlicedLNormPareto, 2000, 5000, 6, 1.5, 3000, shapes)
  #the reported case: the second value used to be 0.3313
  expect_equal(ExposureCurveSlicedGammaPareto(5000, c(1.2, 2), 4e-4, 3000, 1.4), c(0.5193966168, 0.4641285645), tolerance = 1e-9)
  #lengths that do not recycle are an error rather than silently truncated
  expect_error(pSlicedGammaPareto(1:2, 1:3, 4e-4, 3000, 1.4))
})

test_that("sliced quantile functions keep their precision as q approaches 1", {
  #1 - q is exact for q = 1 - 2^-40, so the Pareto tail gives the quantile in closed form
  q <- 1 - 2^-40
  upper <- pgamma(1000, 1, 5e-4, lower.tail = FALSE)
  expect_equal(qSlicedGammaPareto(q, 1, 5e-4, 1000, 1.2), 1000 * (upper * 2^40)^(1 / 1.2), tolerance = 1e-13)
  upper <- plnorm(1000, 6, 1.5, lower.tail = FALSE)
  expect_equal(qSlicedLNormPareto(q, 6, 1.5, 1000, 1.2), 1000 * (upper * 2^40)^(1 / 1.2), tolerance = 1e-13)
  #the quantile function inverts the survival function far in the tail
  x <- qSlicedGammaPareto(1 - 1e-12, 1, 5e-4, 1000, 1.2)
  expect_equal(pgamma(1000, 1, 5e-4, lower.tail = FALSE) * (1000 / x)^1.2, 1e-12, tolerance = 1e-10)
  expect_equal(qSlicedGammaPareto(1, 1, 5e-4, 1000, 1.2), Inf)
})

test_that("sliced pdfs do not overflow for large slice points or shapes", {
  #SlicePoint^PShape overflowed, so these used to be NaN
  expect_equal(dSlicedGammaPareto(2e10, 2, 1e-10, 1e10, 40),
               pgamma(1e10, 2, 1e-10, lower.tail = FALSE) * 40 / 2e10 * 0.5^40, tolerance = 1e-12)
  expect_equal(dSlicedGammaPareto(2e10, 2, 1e-10, 1e10, 40), 1.338338e-21, tolerance = 1e-6)
  expect_equal(dSlicedLNormPareto(2e10, 20, 1, 1e10, 40),
               plnorm(1e10, 20, 1, lower.tail = FALSE) * 40 / 2e10 * 0.5^40, tolerance = 1e-12)
  expect_equal(dSlicedGammaPareto(Inf, 1, 5e-4, 1000, 1.2), 0)
})

test_that("an infinite slice point gives the attritional distribution", {
  x <- c(0, 500, 5000, 1e6, Inf)
  u <- c(0, 0.5, 0.99, 1)
  for (tailShape in c(1.5, 0.8)) {
    #the means used to be NaN (0 * Inf) for PShape > 1 and Inf for PShape <= 1
    expect_equal(SlicedGammaParetoMean(2, 0.001, Inf, tailShape), 2000)
    expect_equal(SlicedLNormParetoMean(6, 1.5, Inf, tailShape), exp(6 + 1.5^2 / 2))
    expect_equal(ExposureCurveSlicedGammaPareto(x, 2, 0.001, Inf, tailShape), ExposureCurveGamma(x, 2, 0.001))
    expect_equal(ExposureCurveSlicedLNormPareto(x, 6, 1.5, Inf, tailShape), ExposureCurveLNorm(x, 6, 1.5))
    expect_equal(SlicedGammaParetoCappedMean(x, 2, 0.001, Inf, tailShape), GammaCappedMean(x, 2, 0.001))
    expect_equal(SlicedLNormParetoCappedMean(x, 6, 1.5, Inf, tailShape), LNormCappedMean(x, 6, 1.5))
    expect_equal(ILFSlicedGammaPareto(500, 5000, 2, 0.001, Inf, tailShape), ILFGamma(500, 5000, 2, 0.001))
    expect_equal(ILFSlicedLNormPareto(500, 5000, 6, 1.5, Inf, tailShape), ILFLNorm(500, 5000, 6, 1.5))
    expect_equal(pSlicedGammaPareto(x, 2, 0.001, Inf, tailShape), pgamma(x, 2, 0.001))
    expect_equal(pSlicedLNormPareto(x, 6, 1.5, Inf, tailShape), plnorm(x, 6, 1.5))
    expect_equal(dSlicedGammaPareto(x, 2, 0.001, Inf, tailShape), dgamma(x, 2, 0.001))
    expect_equal(dSlicedLNormPareto(x, 6, 1.5, Inf, tailShape), dlnorm(x, 6, 1.5))
    expect_equal(qSlicedGammaPareto(u, 2, 0.001, Inf, tailShape), qgamma(u, 2, 0.001))
    expect_equal(qSlicedLNormPareto(u, 6, 1.5, Inf, tailShape), qlnorm(u, 6, 1.5))
  }
  #a finite slice point with PShape <= 1 still has no finite mean
  expect_equal(SlicedGammaParetoMean(2, 0.001, c(1000, Inf), 0.8), c(Inf, 2000))
})

test_that("sliced LogNormal-Pareto documented examples give the same values as before", {
  expect_equal(SlicedLNormParetoMean(6, 1.5, 1000, 1.2), 1865.63324717588, tolerance = 1e-9)
  expect_equal(SlicedLNormParetoMean(c(5, 5.5, 6), 1.5, 1000, 1.1), c(1306.190, 2130.408, 3228.298), tolerance = 1e-6)
  expect_equal(SlicedLNormParetoCappedMean(1200, 6, 1.5, 1000, 1.2), 551.762053427871, tolerance = 1e-9)
  expect_equal(ExposureCurveSlicedLNormPareto(1200, 6, 1.5, 1000, 1.2), 0.295750547039782, tolerance = 1e-9)
  expect_equal(ILFSlicedLNormPareto(800, 1200, 6, 1.5, 1000, 1.2), 1.24391350785947, tolerance = 1e-9)
  expect_equal(pSlicedLNormPareto(1200, 6, 1.5, 1000, 1.2), 0.781021467708666, tolerance = 1e-9)
  expect_equal(qSlicedLNormPareto(0.5, 6, 1.5, 1000, 1.2), 403.428793492735, tolerance = 1e-9)
  expect_equal(dSlicedLNormPareto(1200, 6, 1.5, 1000, 1.2), 2.18978532291334e-04, tolerance = 1e-9)
})
