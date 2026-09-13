#internal helpers of the distribution fitting tool

test_that("piecewise Pareto alphas and cdf match the Pareto package", {
  #reference values computed with Pareto::PiecewisePareto_ML_Estimator_Alpha() and Pareto::pPiecewisePareto()
  losses <- c(1000, 1200, 1500, 2000, 2500, 3100, 4000, 5200, 7000, 9500, 15000, 24000, 40000, 90000)
  t <- c(1000, 3000, 10000)
  alpha <- piecewise_pareto_alpha(losses, t)
  expect_equal(alpha, c(0.330996082409759, 0.650500699119758, 0.822291874889598), tolerance = 1e-12)
  xs <- c(500, 1000, 1500, 3000, 5000, 10000, 20000, 1e6)
  expect_equal(piecewise_pareto_cdf(xs, t, alpha),
               c(0, 0, 0.125591273634939, 0.304856071601121, 0.501388871688104, 0.682355204113239, 0.820358271582917, 0.992799577261430),
               tolerance = 1e-12)
  #a single threshold
  alpha1 <- piecewise_pareto_alpha(losses, 2000)
  expect_equal(alpha1, 0.608895878867442, tolerance = 1e-12)
  expect_equal(piecewise_pareto_cdf(c(1000, 2000, 4000), 2000, alpha1), c(0, 0, 0.344301672019081), tolerance = 1e-12)
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

test_that("fit_gamma_mle matches MASS::fitdistr with the same optimiser settings", {
  #reference computed with MASS::fitdistr(x, "gamma", method = "L-BFGS-B", lower = c(0, 0), start = list(scale = 1, shape = 1))
  x <- c(120, 340, 560, 780, 1200, 1500, 2300, 3100, 4800, 9000)
  fit <- fit_gamma_mle(x)
  expect_named(fit$estimate, c("scale", "shape"))
  expect_equal(unname(fit$estimate), c(2576.48046106733, 0.919859381895703), tolerance = 1e-6)
  #the estimate is a stationary point of the log-likelihood
  loglik <- function(p) sum(dgamma(x, shape = p[2], scale = p[1], log = TRUE))
  grad <- c((loglik(fit$estimate + c(1e-3, 0)) - loglik(fit$estimate - c(1e-3, 0))) / 2e-3,
            (loglik(fit$estimate + c(0, 1e-6)) - loglik(fit$estimate - c(0, 1e-6))) / 2e-6)
  expect_lt(max(abs(grad)), 1e-3)
})
