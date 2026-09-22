#maximum likelihood fits of the Frequency tab, against fitdistrplus

#the reference values were computed with fitdistrplus 1.2.6 (R 4.6.0), with the optimiser run to full
#precision (optim.method = "BFGS", control = list(reltol = 1e-15)); its default Nelder-Mead stops about
#1e-4 (relative) from the maximum. fitdistrplus::fitdist() counts the rows, not the weights, as n in the BIC
count_data <- function() {
  set.seed(10)
  d1 <- rpois(300, 3)
  set.seed(1)
  d2 <- rnbinom(500, size = 1.5, mu = 4)
  set.seed(2)
  d3 <- rnbinom(2000, size = 0.4, mu = 60)
  set.seed(4)
  x7 <- rnbinom(150, size = 2, mu = 3)
  w7 <- sample(1:5, 150, TRUE)
  set.seed(10)
  w1 <- sample(1:4, 300, TRUE)
  list(d1 = d1, d2 = d2, d3 = d3, d5 = c(0, 1, 0, 2, 0, 0, 1, 5, 0, 3), x7 = x7, w7 = w7, w1 = w1)
}

expect_fit <- function(fit, estimate, sd, loglik, aic = NULL, bic = NULL) {
  expect_equal(unname(fit$estimate), estimate, tolerance = 1e-6)
  expect_equal(unname(fit$sd), sd, tolerance = 1e-4)
  expect_equal(fit$loglik, loglik, tolerance = 1e-10)
  if (!is.null(aic)) expect_equal(fit$aic, aic, tolerance = 1e-10)
  if (!is.null(bic)) expect_equal(fit$bic, bic, tolerance = 1e-10)
}

test_that("the Poisson fit matches fitdistrplus", {
  d <- count_data()
  fit <- fit_poisson_mle(d$d1)
  expect_named(fit$estimate, "lambda")
  #the MLE is the mean (mean() may accumulate with extra precision, so not bit for bit)
  expect_equal(fit$estimate[["lambda"]], mean(d$d1), tolerance = 1e-12)
  expect_fit(fit, 3.03000000726144, 0.100498745500419, -573.028945852258, 1148.05789170452, 1151.76167417917)
  expect_fit(fit_poisson_mle(d$d2), 3.59400002580602, 0.084782067610116, -1444.22888067683)
  expect_fit(fit_poisson_mle(d$d3), 63.2415000000817, 0.17782224052285, -111818.907009002)
  #weighted: each row counts as many observations as its weight
  expect_fit(fit_poisson_mle(d$x7, d$w7), 3.25523021793035, 0.0825233540931092, -1305.66882596566,
             2613.33765193131, 2616.34828722541)
  expect_fit(fit_poisson_mle(d$d1, d$w1), 3.00953689550008, 0.0640326928397148, -1397.22694578277,
             bic = 2800.1576740402)
  expect_equal(fit_poisson_mle(c(1, 4, 2), c(2, 1, 3))$estimate[["lambda"]], 2)
})

test_that("the Negative Binomial fit matches fitdistrplus", {
  d <- count_data()
  fit <- fit_nbinom_mle(d$d2)
  expect_named(fit$estimate, c("size", "mu"))
  expect_false(fit$capped)
  expect_fit(fit, c(1.52157959847609, 3.59400014376992), c(0.14662251440355, 0.155454777929321),
             -1193.53323053985, 2391.06646107971, 2399.49567727655)
  #the MLE of mu is the mean, and the size and mu estimates are uncorrelated at the maximum
  #(on macOS arm64 mean() and the weighted mean differ in the last bit)
  expect_equal(fit$estimate[["mu"]], mean(d$d2), tolerance = 1e-12)
  expect_equal(fit$cor[1, 2], 0, tolerance = 1e-8)
  #large, very overdispersed counts
  expect_fit(fit_nbinom_mle(d$d3), c(0.408741757447421, 63.2415000000007), c(0.0118120611911759, 2.2190187139519),
             -9717.72690903802, 19439.453818076, 19450.6556229951)
  #ten small counts
  expect_fit(fit_nbinom_mle(d$d5), c(0.76539203395449, 1.20000039965445), c(0.726143481183746, 0.555102118268894),
             -15.1210584281315, 34.242116856263, 34.8472870422511)
  #weighted
  expect_fit(fit_nbinom_mle(d$x7, d$w7), c(1.57202507782423, 3.25523029697537), c(0.15981867630745, 0.144609640230492),
             -1098.50203989825, 2201.0040797965, 2207.02535038469)
  #integer weights give the fit of the rows repeated, except for the BIC's n
  repeated <- fit_nbinom_mle(rep(d$x7, d$w7))
  weighted <- fit_nbinom_mle(d$x7, d$w7)
  expect_equal(weighted$estimate, repeated$estimate, tolerance = 1e-10)
  expect_equal(weighted$loglik, repeated$loglik, tolerance = 1e-10)
  expect_equal(weighted$sd, repeated$sd, tolerance = 1e-8)
})

test_that("the Negative Binomial fit tends to the Poisson when the counts are not overdispersed", {
  d <- count_data()
  #fitdistrplus stops at an arbitrary large size (1.1e6, 3.1e6, 6.5e5) with a log-likelihood just below the Poisson's
  cases <- list(
    list(x = d$d1, w = NULL, fitdist = -573.028971732392, poisson = -573.028945852258),
    list(x = c(2, 3, 3, 2, 3, 2, 3, 2, 3, 3), w = NULL, fitdist = -14.6798519386281, poisson = -14.6798479668948),
    list(x = d$d1, w = d$w1, fitdist = -1397.22707401844, poisson = -1397.22694578277)
  )
  for (case in cases) {
    fit <- fit_nbinom_mle(case$x, case$w)
    expect_true(fit$capped)
    expect_equal(fit$estimate[["size"]], dft_nbinom_max_size)
    expect_equal(fit$estimate[["mu"]], fit_poisson_mle(case$x, case$w)$estimate[["lambda"]])
    #at least as high as fitdistrplus's, and the Poisson's to 1e-6
    expect_gte(fit$loglik, case$fitdist)
    expect_equal(fit$loglik, case$poisson, tolerance = 1e-6 / abs(case$poisson))
    expect_true(is.na(fit$sd[["size"]]))
    expect_equal(fit$sd[["mu"]], fit_poisson_mle(case$x, case$w)$sd[["lambda"]], tolerance = 1e-6)
    #the Poisson has the lower AIC
    expect_lt(fit_poisson_mle(case$x, case$w)$aic, fit$aic)
  }
  #counts all equal have no variance
  expect_true(fit_nbinom_mle(rep(2, 5))$capped)
  expect_error(fit_nbinom_mle(rep(0, 5)), "all zero")
})

test_that("count fits print as fitdistrplus summaries did", {
  d <- count_data()
  printed <- paste(capture.output(dft_print_count_fit(fit_nbinom_mle(d$d2))), collapse = "\n")
  expect_match(printed, "Negative Binomial distribution fitted by maximum likelihood")
  expect_match(printed, "Std. Error")
  expect_match(printed, "Correlation matrix")
  expect_match(printed, "AIC:")
  printed <- paste(capture.output(dft_print_count_fit(fit_nbinom_mle(c(2, 3, 3, 2, 3), c(1, 2, 1, 1, 1)))), collapse = "\n")
  expect_match(printed, "tends to the Poisson")
  expect_match(printed, "as many observations as its weight")
})
