#the Negative Binomial fit of overdispersed counts: it has a size wherever the variance is above the mean

#the maximum of the profile likelihood in the size, with mu at the mean, found directly
nbinom_profile_max <- function(x, w = rep(1, length(x))) {
  mu <- sum(w * x) / sum(w)
  best <- optimize(function(log_size) -sum(w * dnbinom(x, size = exp(log_size), mu = mu, log = TRUE)),
                   c(-10, 25), tol = 1e-12)
  c(size = exp(best$minimum), loglik = -best$objective)
}

test_that("the Negative Binomial fit of overdispersed claim counts matches MASS::fitdistr", {
  #the claim counts of a 3,000 row test file (mean 0.364, variance 0.532), as the number of rows and the
  #total weight at each count; integer weights give the fit of the rows repeated
  counts <- c(0, 1, 2, 3, 4, 6, 7)
  rows <- c(2221, 559, 154, 50, 11, 4, 1)
  weights <- c(6797, 1615, 452, 171, 34, 13, 2)
  #the size was capped at 1e8, with the Poisson's log-likelihood (-2461.71): the sign of the score at 1e8,
  #a difference of digamma values 1e-8 apart, was rounding error
  fit <- fit_nbinom_mle(counts, rows)
  expect_false(fit$capped)
  #MASS::fitdistr() (reltol = 1e-15): size 0.806549832609132, log-likelihood -2372.20699263907 (AIC 4748.414);
  #its optimiser stops just below the maximum
  expect_equal(fit$estimate[["size"]], 0.806549832609132, tolerance = 1e-5)
  expect_equal(fit$loglik, -2372.20699263907, tolerance = 1e-10)
  expect_gte(fit$loglik, -2372.20699263907 - 1e-9)
  expect_equal(fit$aic, 4748.41398527814, tolerance = 1e-10)
  expect_lt(fit$aic, fit_poisson_mle(counts, rows)$aic)
  #weighted: MASS gives size 0.70684520790259, log-likelihood -7112.77472197045
  fit <- fit_nbinom_mle(counts, weights)
  expect_false(fit$capped)
  expect_equal(fit$estimate[["size"]], 0.70684520790259, tolerance = 1e-5)
  expect_equal(fit$loglik, -7112.77472197045, tolerance = 1e-10)
  expect_gte(fit$loglik, -7112.77472197045 - 1e-9)
  #samples with variance / mean 1.15 and 1.81 that were capped; MASS gives sizes 3.62 and 2.18
  fit <- fit_nbinom_mle(0:4, c(289, 141, 53, 14, 3))
  expect_false(fit$capped)
  expect_equal(fit$estimate[["size"]], 3.61805851319479, tolerance = 1e-5)
  expect_gte(fit$loglik, -522.495074279306 - 1e-9)
  fit <- fit_nbinom_mle(c(0, 1, 2, 3, 4, 8), c(18, 21, 4, 5, 1, 1))
  expect_false(fit$capped)
  expect_equal(fit$estimate[["size"]], 2.18197245206725, tolerance = 1e-5)
  expect_gte(fit$loglik, -72.1412159468468 - 1e-9)
})

test_that("no overdispersed sample is capped, and each fit is the maximum of the likelihood", {
  set.seed(31)
  for (i in 1:50) {
    n <- sample(c(30, 100, 500, 3000), 1)
    #the ratio of the variance to the mean is 1 + mu / size, from 1.02 up
    x <- rnbinom(n, size = runif(1, 0.2, 50), mu = runif(1, 0.05, 5))
    w <- if (i %% 5 == 0) sample(1:4, n, TRUE) else rep(1, n)
    mu <- sum(w * x) / sum(w)
    if (!(sum(w * (x - mu)^2) / sum(w) > mu)) next
    fit <- fit_nbinom_mle(x, if (i %% 5 == 0) w)
    best <- nbinom_profile_max(x, w)
    expect_false(fit$capped, label = paste("sample", i))
    expect_gte(fit$loglik, best[["loglik"]] - 1e-8 * abs(best[["loglik"]]))
    if (best[["size"]] < 1e4) expect_equal(fit$estimate[["size"]], best[["size"]], tolerance = 1e-4, label = paste("sample", i))
  }
})

test_that("the Negative Binomial fit has a size wherever the variance is barely above the mean", {
  #the variance (divided by n) is above the mean exactly when the likelihood has a maximum in the size
  #(variance / mean 1.026)
  counts <- 0:6
  rows <- c(372, 360, 185, 62, 17, 3, 1)
  x <- rep(counts, rows)
  mu <- mean(x)
  expect_gt(mean((x - mu)^2), mu)
  fit <- fit_nbinom_mle(counts, rows)
  expect_false(fit$capped)
  #MASS::fitdistr() gives size 38.5306050165 and log-likelihood -1314.11318938644, above the Poisson's (-1314.27748)
  expect_equal(fit$estimate[["size"]], 38.5306050165, tolerance = 1e-4)
  expect_gte(fit$loglik, -1314.11318938644 - 1e-9)
  expect_gt(fit$loglik, fit_poisson_mle(counts, rows)$loglik + 0.1)
  expect_gte(fit$loglik, nbinom_profile_max(counts, rows)[["loglik"]] - 1e-9)
  #the variance just below the mean (0, 2, 3: 1.56 against 1.67) has no maximum
  expect_true(fit_nbinom_mle(c(0, 2, 3))$capped)
})

test_that("counts above the table limit give the sums of the table", {
  set.seed(5)
  x <- rnbinom(200, size = 0.7, mu = 40)
  w <- sample(1:3, 200, TRUE)
  full <- dft_nbinom_count_sums(x, w)
  #the closed forms from j = 10 up, for every count above 10
  split <- dft_nbinom_count_sums(x, w, limit = 10)
  #(the closed forms lose digits to cancellation at large sizes, about 1e-9 relative at 1e5)
  for (size in c(0.01, 0.7, 30, 1e5)) {
    expect_equal(split$ratio(size), full$ratio(size), tolerance = 1e-7)
    expect_equal(split$square(size), full$square(size), tolerance = 1e-7)
  }
  #the two sums against their definitions
  expect_equal(full$ratio(2), sum(w * vapply(x, function(y) sum((seq_len(y) - 1) / (2 + seq_len(y) - 1)), 0)))
  expect_equal(full$square(2), sum(w * (trigamma(2) - trigamma(2 + x))))
  #large counts (the fit only uses the closed forms above a million)
  big <- fit_nbinom_mle(c(2e6, 5e6, 1e7, 3e5, 8e6))
  expect_false(big$capped)
  expect_gte(big$loglik, nbinom_profile_max(c(2e6, 5e6, 1e7, 3e5, 8e6))[["loglik"]] - 1e-8)
})
