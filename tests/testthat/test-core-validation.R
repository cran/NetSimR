#input checks, recycling and zero-length input of the analytic severity functions

#each function with a valid set of arguments: `amounts` are the claim amounts or probabilities
#(first, in the function's argument order) and `pars` the distribution parameters that must be positive
analytic_cases <- list(
  IGamma = list(f = IGamma, amounts = list(), pars = list(a = 2), after = list(x = 1)),
  GammaCappedMean = list(f = GammaCappedMean, amounts = list(cap = 1000), pars = list(shape = 2, rate = 0.001)),
  ExposureCurveGamma = list(f = ExposureCurveGamma, amounts = list(x = 1000), pars = list(shape = 2, rate = 0.001)),
  ILFGamma = list(f = ILFGamma, amounts = list(xLow = 500, xHigh = 1000), pars = list(shape = 2, rate = 0.001)),
  LNormCappedMean = list(f = LNormCappedMean, amounts = list(cap = 1000), pars = list(mu = 6, sigma = 1.5)),
  ExposureCurveLNorm = list(f = ExposureCurveLNorm, amounts = list(x = 1000), pars = list(mu = 6, sigma = 1.5)),
  ILFLNorm = list(f = ILFLNorm, amounts = list(xLow = 500, xHigh = 1000), pars = list(mu = 6, sigma = 1.5)),
  ParetoCappedMeanCalc = list(f = ParetoCappedMeanCalc, amounts = list(cap = 2000), pars = list(scale = 500, shape = 1.5)),
  ParetoCappedMean = list(f = ParetoCappedMean, amounts = list(cap = 2000), pars = list(scale = 500, shape = 1.5)),
  ExposureCurvePareto = list(f = ExposureCurvePareto, amounts = list(x = 2000), pars = list(scale = 500, shape = 1.5)),
  ILFPareto = list(f = ILFPareto, amounts = list(xLow = 1000, xHigh = 2000), pars = list(scale = 500, shape = 1.5)),
  SlicedGammaParetoMean = list(f = SlicedGammaParetoMean, amounts = list(), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  SlicedGammaParetoCappedMean = list(f = SlicedGammaParetoCappedMean, amounts = list(cap = 2000), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  ExposureCurveSlicedGammaPareto = list(f = ExposureCurveSlicedGammaPareto, amounts = list(x = 2000), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  ILFSlicedGammaPareto = list(f = ILFSlicedGammaPareto, amounts = list(xLow = 1000, xHigh = 2000), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  pSlicedGammaPareto = list(f = pSlicedGammaPareto, amounts = list(x = 2000), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  qSlicedGammaPareto = list(f = qSlicedGammaPareto, amounts = list(q = 0.9), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  dSlicedGammaPareto = list(f = dSlicedGammaPareto, amounts = list(x = 2000), pars = list(GShape = 2, GRate = 0.001, SlicePoint = 1000, PShape = 1.5)),
  SlicedLNormParetoMean = list(f = SlicedLNormParetoMean, amounts = list(), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5)),
  SlicedLNormParetoCappedMean = list(f = SlicedLNormParetoCappedMean, amounts = list(cap = 2000), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5)),
  ExposureCurveSlicedLNormPareto = list(f = ExposureCurveSlicedLNormPareto, amounts = list(x = 2000), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5)),
  ILFSlicedLNormPareto = list(f = ILFSlicedLNormPareto, amounts = list(xLow = 1000, xHigh = 2000), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5)),
  pSlicedLNormPareto = list(f = pSlicedLNormPareto, amounts = list(x = 2000), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5)),
  qSlicedLNormPareto = list(f = qSlicedLNormPareto, amounts = list(q = 0.9), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5)),
  dSlicedLNormPareto = list(f = dSlicedLNormPareto, amounts = list(x = 2000), pars = list(mu = 6, sigma = 1.5, SlicePoint = 1000, shape = 1.5))
)

call_case <- function(case, amounts = case$amounts, pars = case$pars) {
  do.call(case$f, c(amounts, pars, case$after))
}

test_that("non-positive parameters are an error rather than a plausible number", {
  #the reported cases
  expect_error(ParetoCappedMean(2000, 500, -2), "shape must be positive")
  expect_error(pSlicedGammaPareto(2000, 2, 0.001, 1000, -1), "PShape must be positive")
  expect_error(dSlicedGammaPareto(2000, 2, 0.001, 1000, -1), "PShape must be positive")
  expect_error(qSlicedGammaPareto(0.9, 2, 0.001, 1000, -1), "PShape must be positive")
  expect_error(SlicedGammaParetoCappedMean(2000, 2, 0.001, 1000, -1), "PShape must be positive")
  expect_error(SlicedGammaParetoMean(2, 0.001, -1000, 2), "SlicePoint must be positive")
  expect_error(GammaCappedMean(1000, 2, -0.001), "rate must be positive")
  expect_error(ExposureCurveGamma(1000, 2, -0.001), "rate must be positive")
  expect_error(ExposureCurvePareto(1000, -500, 1.5), "scale must be positive")
  #every positive parameter of every function, at zero and below, including within a vector
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    expect_no_error(call_case(case))
    for (par in setdiff(names(case$pars), "mu")) {
      for (bad in list(0, -1, c(1, -1))) {
        pars <- case$pars
        pars[[par]] <- bad
        expect_error(call_case(case, pars = pars), paste(par, "must be positive"), info = paste(name, par))
      }
    }
  }
  #the error names the function that was called
  err <- tryCatch(ExposureCurveGamma(1000, 2, -0.001), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("ExposureCurveGamma"))
})

test_that("missing parameters give NA rather than an error", {
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    for (par in names(case$pars)) {
      pars <- case$pars
      pars[[par]] <- NA
      expect_true(is.na(call_case(case, pars = pars)), info = paste(name, par))
    }
  }
})

test_that("negative claim amounts are an error for capped means, exposure curves and ILFs", {
  expect_error(GammaCappedMean(-5, 2, 0.001), "cap must be non-negative")
  expect_error(IGamma(2, -1), "x must be non-negative")
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    if (grepl("^[pqd]Sliced", name)) next
    for (amount in names(case$amounts)) {
      amounts <- case$amounts
      amounts[[amount]] <- -1
      expect_error(call_case(case, amounts = amounts), paste(amount, "must be non-negative"), info = name)
    }
    #a zero amount is allowed
    amounts <- lapply(case$amounts, function(a) 0)
    expect_no_error(call_case(case, amounts = amounts))
  }
})

test_that("sliced cdfs and pdfs are 0 below zero and quantiles outside [0, 1] are NaN, as in base R", {
  expect_equal(pSlicedGammaPareto(c(-1, -Inf), 2, 0.001, 1000, 1.5), c(0, 0))
  expect_equal(dSlicedGammaPareto(c(-1, -Inf), 2, 0.001, 1000, 1.5), c(0, 0))
  expect_equal(pSlicedLNormPareto(c(-1, -Inf), 6, 1.5, 1000, 1.5), c(0, 0))
  expect_equal(dSlicedLNormPareto(c(-1, -Inf), 6, 1.5, 1000, 1.5), c(0, 0))
  expect_equal(suppressWarnings(qSlicedGammaPareto(c(-0.1, 1.1), 2, 0.001, 1000, 1.5)), c(NaN, NaN))
  expect_equal(suppressWarnings(qSlicedLNormPareto(c(-0.1, 1.1), 6, 1.5, 1000, 1.5)), c(NaN, NaN))
})

test_that("zero-length input gives numeric(0)", {
  #ParetoCappedMean and the sliced functions used to fail with "arguments imply differing number of rows"
  expect_identical(ParetoCappedMean(numeric(0), 500, 1.5), numeric(0))
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    first <- if (length(case$amounts)) "amounts" else "pars"
    args <- case[[first]]
    args[[1]] <- numeric(0)
    result <- if (first == "amounts") call_case(case, amounts = args) else call_case(case, pars = args)
    expect_identical(result, numeric(0), info = name)
  }
})

test_that("matrix and named input keep their shape", {
  m <- matrix(c(1000, 2000, 3000, 4000), 2)
  #a matrix cap used to give logical(0), as data.frame() renamed its columns
  expect_equal(ParetoCappedMean(m, 500, 1.5), matrix(ParetoCappedMean(c(m), 500, 1.5), 2))
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    if (length(case$amounts) == 0 || name == "IGamma") next
    amounts <- case$amounts
    amounts[[1]] <- if (grepl("^q", name)) m / 5000 else m
    result <- call_case(case, amounts = amounts)
    expect_equal(dim(result), c(2, 2), info = name)
    elementwise <- vapply(c(amounts[[1]]), function(a) { amounts[[1]] <- a; call_case(case, amounts = amounts) }, numeric(1))
    expect_equal(c(result), elementwise, info = name)
  }
  expect_named(SlicedGammaParetoCappedMean(c(low = 500, high = 5000), 2, 0.001, 1000, 1.5), c("low", "high"))
  expect_named(ExposureCurvePareto(c(low = 500, high = 5000), 400, 1.5), c("low", "high"))
})

test_that("non-numeric arguments are an error that names them", {
  #these used to fail with "non-numeric argument to binary operator"
  expect_error(GammaCappedMean("700", 1, 0.0005), "cap must be numeric")
  expect_error(ParetoCappedMean(600, "200", 1.2), "scale must be numeric")
  expect_error(SlicedLNormParetoMean(6, 1.5, 1000, list(1.2)), "shape must be numeric")
  #every argument of every function, including mu and the claim amounts of the sliced cdfs
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    args <- c(case$amounts, case$pars, case$after)
    for (arg in names(args)) {
      bad <- args
      bad[[arg]] <- as.character(bad[[arg]])
      expect_error(do.call(case$f, bad), paste(arg, "must be numeric"), info = paste(name, arg))
    }
  }
  expect_error(erf("a"), "x must be numeric")
  err <- tryCatch(LNormCappedMean(1000, "6", 1.5), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("LNormCappedMean"))
  #missing values of any type still give NA
  expect_true(is.na(GammaCappedMean(NA_character_, 1, 1)))
  expect_true(is.na(LNormCappedMean(1000, NA, 1.5)))
})

test_that("lengths that do not recycle are an error for every function", {
  #GammaCappedMean, LNormCappedMean, IGamma and the ILFs used to recycle with a warning
  expect_error(GammaCappedMean(c(1, 2), 1, c(1, 2, 3)), "cannot be recycled")
  for (name in names(analytic_cases)) {
    case <- analytic_cases[[name]]
    args <- c(case$amounts, case$pars, case$after)
    bad <- args
    bad[[1]] <- rep(bad[[1]], 2)
    bad[[2]] <- rep(bad[[2]], 3)
    expect_error(do.call(case$f, bad), "cannot be recycled", info = name)
    #lengths that do recycle give the longest length
    good <- args
    good[[1]] <- rep(good[[1]], 2)
    good[[2]] <- rep(good[[2]], 4)
    expect_length(do.call(case$f, good), 4)
  }
  err <- tryCatch(ILFGamma(1:2, 1:3, 1, 1), error = identity)
  expect_identical(conditionCall(err)[[1]], as.name("ILFGamma"))
})
