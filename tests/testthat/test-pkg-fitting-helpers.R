#internal helpers of the distribution fitting tool

test_that("piecewise Pareto alphas and cdf match the Pareto package", {
  #reference values computed with Pareto::PiecewisePareto_ML_Estimator_Alpha() and Pareto::pPiecewisePareto()
  losses <- c(1000, 1200, 1500, 2000, 2500, 3100, 4000, 5200, 7000, 9500, 15000, 24000, 40000, 90000)
  #no loss at the first threshold: the alphas are the Pareto package's
  expect_equal(piecewise_pareto_alpha(losses, c(900, 3000, 10000)),
               c(0.368737480773024, 0.650500699119758, 0.822291874889598), tolerance = 1e-12)
  #a loss at the first threshold counts in the first layer, which the Pareto package leaves out
  #(its first alpha is 0.330996082409759); the alphas of the other layers are the package's
  t <- c(1000, 3000, 10000)
  alpha <- piecewise_pareto_alpha(losses, t)
  expect_equal(alpha, c(0.413745103012199, 0.650500699119758, 0.822291874889598), tolerance = 1e-12)
  expect_equal(alpha[1], sum(losses < 3000) / sum(log(pmin(losses, 3000) / 1000)))
  xs <- c(500, 1000, 1500, 3000, 5000, 10000, 20000, 1e6)
  expect_equal(piecewise_pareto_cdf(xs, t, alpha),
               c(0, 0, 0.154442582071987, 0.365263581059718, 0.544717821592101, 0.709958309352466, 0.835969009108178, 0.993425288839876),
               tolerance = 1e-12)
  #a single threshold
  alpha1 <- piecewise_pareto_alpha(losses, 2000)
  expect_equal(alpha1, 0.669785466754186, tolerance = 1e-12)
  expect_equal(piecewise_pareto_cdf(c(1000, 2000, 4000), 2000, alpha1), c(0, 0, 0.371399844887915), tolerance = 1e-12)
  #with the smallest loss as the only threshold, the one layer is the Severity tab's Pareto: n / sum(log(x / min(x)))
  expect_equal(piecewise_pareto_alpha(losses, min(losses)), length(losses) / sum(log(losses / min(losses))))
})

test_that("piecewise Pareto cdf is a proper distribution function", {
  losses <- exp(seq(log(1000), log(500000), length.out = 60))
  t <- c(1000, 5000, 50000)
  alpha <- piecewise_pareto_alpha(losses, t)
  expect_length(alpha, 3)
  expect_true(all(alpha > 0))
  x <- sort(c(losses, t, t * 1.0001, 1e9))
  p <- piecewise_pareto_cdf(x, t, alpha)
  expect_true(all(diff(p) >= 0))
  expect_true(all(p >= 0 & p < 1))
  #the first layer is a plain Pareto with threshold t[1]
  expect_equal(piecewise_pareto_cdf(3000, t, alpha), 1 - (1000 / 3000)^alpha[1])
  #the cdf is continuous at the thresholds
  expect_equal(piecewise_pareto_cdf(t[2] * (1 + 1e-12), t, alpha), piecewise_pareto_cdf(t[2], t, alpha), tolerance = 1e-9)
  expect_equal(piecewise_pareto_cdf(numeric(0), t, alpha), numeric(0))
  expect_true(is.nan(piecewise_pareto_cdf(NA, t, alpha)))
})

test_that("piecewise Pareto helpers warn and return NaN on invalid input", {
  losses <- c(1000, 1200, 1500, 2000, 2500, 3100, 4000, 5200)
  expect_warning(a <- piecewise_pareto_alpha(losses, c(2000, 1000)), "ascending")
  expect_true(all(is.nan(a)))
  expect_warning(a <- piecewise_pareto_alpha(losses, c(1000, 6000)), "max")
  expect_true(all(is.nan(a)))
  expect_warning(a <- piecewise_pareto_alpha(losses, c(0, 1000)), "positive")
  expect_true(is.nan(a))
  expect_warning(p <- piecewise_pareto_cdf(c(1500, 3000), c(1000, 2000), c(NaN, NaN)))
  expect_true(all(is.nan(p)))
  expect_warning(p <- piecewise_pareto_cdf(c(1500, 3000), c(1000, 2000), c(1, 0)))
  expect_true(all(is.nan(p)))
})

test_that("empirical cdf and mean excess helpers match their definitions", {
  set.seed(1)
  claims <- round(exp(rnorm(200, 7, 1.2)))
  points <- sort(unique(c(0, quantile(claims, c(0.1, 0.5, 0.9), names = FALSE), claims[1:20], max(claims))))
  expect_equal(empirical_cdf_at(points, claims), vapply(points, function(x) mean(claims <= x), numeric(1)))
  mean_excess <- mean_excess_at(points, claims)
  expected <- vapply(points, function(u) mean(claims[claims > u]) - u, numeric(1))
  defined <- points < max(claims)
  expect_equal(mean_excess[defined], expected[defined])
  expect_true(is.nan(mean_excess[points == max(claims)]))
})

test_that("weighted empirical cdf counts each claim weight times", {
  claims <- c(3, 1, 2, 2)
  weights <- c(1, 2, 3, 4)
  points <- c(0, 1, 1.5, 2, 3, 4)
  expect_equal(empirical_cdf_at(points, claims, weights),
               empirical_cdf_at(points, rep(claims, weights)))
})

test_that("ks_distance matches stats::ks.test", {
  set.seed(4)
  x <- rlnorm(80, 5, 1.1)
  expect_equal(ks_distance(x, function(q) plnorm(q, 5, 1)), unname(ks.test(x, "plnorm", 5, 1)$statistic))
  expect_true(is.na(ks_distance(numeric(0), pnorm)))
})

test_that("data columns are converted to numbers and uploaded files read cleanly", {
  expect_equal(dft_as_numeric(c(" 1,234.5", "12", "abc", "", "1,23", "-3")), c(1234.5, 12, NA, NA, NA, -3))
  expect_equal(dft_as_numeric(c("1,5", "2"), dec = ","), c(1.5, 2))
  #with a decimal comma, points separate thousands
  expect_equal(dft_as_numeric(c("1.234", "2.500", "15.000,5", "-1.234.567,25", "1,5", "0,001", "0.5"), dec = ","),
               c(1234, 2500, 15000.5, -1234567.25, 1.5, 0.001, 0.5))
  expect_equal(dft_as_numeric(c(TRUE, FALSE)), c(1, 0))
  df <- data.frame(a = c("1", "2", "x"), b = c("1", "2", "3"), c = c("q", "w", "e"))
  expect_equal(dft_numeric_columns(df), "b")
  #a byte order mark, a blank name and a repeated name
  path <- tempfile(fileext = ".csv")
  con <- file(path, "wb")
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  writeBin(charToRaw("Claim Amount,,Claim Amount\n1,2,3\n4,5,6\n"), con)
  close(con)
  expect_equal(names(dft_read_data(path)), c("Claim Amount", "V2", "Claim Amount_1"))
  #a decimal comma with a semicolon separator
  path <- tempfile(fileext = ".csv")
  writeLines(c("sev;n", "1,5;2", "2,25;3"), path)
  expect_equal(dft_read_data(path, sep = ";", dec = ",")$sev, c(1.5, 2.25))
  #a separator at the end of each data row: read.csv() made the first column the row names and
  #shifted the others (sev got the values of n), or failed on repeated values
  path <- tempfile(fileext = ".csv")
  writeLines(c("sev,n", "100,1,", "100,2,", "250,3,"), path)
  df <- dft_read_data(path)
  expect_equal(names(df), c("sev", "n", "V3"))
  expect_equal(df$sev, c(100, 100, 250))
  expect_equal(df$n, 1:3)
  #a header longer than the rows, blank lines before the header, a header only, and no header
  path <- tempfile(fileext = ".csv")
  writeLines(c("", "a,b,c", "1,2", "3,4"), path)
  df <- dft_read_data(path)
  expect_equal(names(df), c("a", "b", "c"))
  expect_equal(df$b, c(2, 4))
  expect_true(all(is.na(df$c)))
  path <- tempfile(fileext = ".csv")
  writeLines("a,b", path)
  expect_equal(dim(dft_read_data(path)), c(0, 2))
  path <- tempfile(fileext = ".csv")
  writeLines(c("1,2", "3,4"), path)
  expect_equal(dft_read_data(path, header = FALSE), data.frame(V1 = c(1L, 3L), V2 = c(2L, 4L)))
})

test_that("dft_fmt writes huge and tiny numbers in scientific notation", {
  dash <- intToUtf8(8212)
  expect_equal(dft_fmt(c(4.987e42, 1e-300, -2.5e20, 1e15, 1.234e14, 1234.5678, 0, 0.001234, NA, Inf)),
               c("4.987e+42", "1e-300", "-2.5e+20", "1e+15", "123,400,000,000,000", "1,235", "0", "0.001234", dash, dash))
  #counts and total weights in full, not 1.61e+09
  expect_equal(dft_fmt_count(1.61e9), "1,610,000,000")
  expect_equal(dft_fmt_count(1e20), "1e+20")
})

test_that("fit_gamma_mle fits claims far below 1", {
  set.seed(5)
  x <- rgamma(200, shape = 2, scale = 0.01)
  fit <- fit_gamma_mle(x)
  expect_true(all(is.finite(fit$estimate)))
  expect_equal(unname(fit$estimate[["shape"]] * fit$estimate[["scale"]]), mean(x), tolerance = 1e-3)
  #the profile likelihood fit, the fallback when the optimiser stops on its zero bounds, gives the exact MLE:
  #its mean equals the sample mean and it is a maximum of the log-likelihood
  set.seed(6)
  counts <- pmax(1, rpois(1000, 3))
  expect_equal(fit_gamma_mle(counts)$estimate, fit_gamma_profile(counts)$estimate, tolerance = 1e-4)
  fit <- fit_gamma_profile(counts)
  expect_named(fit$estimate, c("scale", "shape"))
  expect_equal(unname(fit$estimate[["shape"]] * fit$estimate[["scale"]]), mean(counts), tolerance = 1e-10)
  expect_error(fit_gamma_profile(c(2, 2, 2)), "not all equal")
  loglik <- function(p) sum(dgamma(counts, shape = p[2], scale = p[1], log = TRUE))
  #the estimate is a maximum: moving either parameter lowers the log-likelihood
  for (step in list(c(1.01, 1), c(0.99, 1), c(1, 1.01), c(1, 0.99))) {
    expect_true(loglik(fit$estimate) > loglik(fit$estimate * step))
  }
})

test_that("fit_gamma_mle gives the exact maximum at every scale of the claims", {
  #an optimiser started at scale = 1, shape = 1 reported success with a shape of 7.5 at 1e9 and 141.5 at 1e12
  set.seed(9)
  base <- rgamma(300, shape = 2)
  shape <- fit_gamma_mle(base)$estimate[["shape"]]
  for (s in 10^(-6:12)) {
    x <- base * s
    fit <- fit_gamma_mle(x)$estimate
    expect_equal(fit[["shape"]], shape, tolerance = 1e-9)
    #the likelihood equations: shape x scale = mean(x), log(shape) - digamma(shape) = log(mean(x)) - mean(log(x))
    expect_equal(fit[["shape"]] * fit[["scale"]], mean(x), tolerance = 1e-12)
    expect_equal(log(fit[["shape"]]) - digamma(fit[["shape"]]), log(mean(x)) - mean(log(x)), tolerance = 1e-9)
  }
})

test_that("fit_gamma_mle matches MASS::fitdistr", {
  #reference computed with MASS::fitdistr(x, "gamma", method = "L-BFGS-B", lower = c(0, 0), start = list(scale = 1, shape = 1)),
  #whose optimiser stops within about 1e-6 of the exact maximum that fit_gamma_mle() gives
  x <- c(120, 340, 560, 780, 1200, 1500, 2300, 3100, 4800, 9000)
  fit <- fit_gamma_mle(x)
  expect_named(fit$estimate, c("scale", "shape"))
  expect_equal(unname(fit$estimate), c(2576.48046106733, 0.919859381895703), tolerance = 1e-5)
  #the estimate is a stationary point of the log-likelihood
  loglik <- function(p) sum(dgamma(x, shape = p[2], scale = p[1], log = TRUE))
  grad <- c((loglik(fit$estimate + c(1e-3, 0)) - loglik(fit$estimate - c(1e-3, 0))) / 2e-3,
            (loglik(fit$estimate + c(0, 1e-6)) - loglik(fit$estimate - c(0, 1e-6))) / 2e-6)
  expect_lt(max(abs(grad)), 1e-3)
})
