test_that("frequency moments match the closed forms", {
  expect_equal(distribution_moments(freq_dist_options, "Poisson", 3), c(mean = 3, sd = sqrt(3)))
  #Negative Binomial as a Poisson-Gamma mixture with shape r and scale beta
  expect_equal(distribution_moments(freq_dist_options, "Negative_Binomial", c(4, 2.5)),
               c(mean = 10, sd = sqrt(4 * 2.5 * 3.5)))
  expect_equal(distribution_moments(freq_dist_options, "Binomial", c(10, 0.3)),
               c(mean = 3, sd = sqrt(10 * 0.3 * 0.7)))
  expect_equal(distribution_moments(freq_dist_options, "Fixed_number_of_Counts", 4), c(mean = 4, sd = 0))
})

test_that("severity moments match the closed forms", {
  expect_equal(distribution_moments(sev_dist_options, "Normal", c(100, 200)), c(mean = 100, sd = 200))
  expect_equal(distribution_moments(sev_dist_options, "LogNormal", c(6, 1.5)),
               c(mean = exp(6 + 1.5^2 / 2), sd = sqrt((exp(1.5^2) - 1) * exp(2 * 6 + 1.5^2))))
  expect_equal(distribution_moments(sev_dist_options, "Gamma", c(2, 500)), c(mean = 1000, sd = sqrt(2) * 500))
  expect_equal(distribution_moments(sev_dist_options, "Exponential", 0.01), c(mean = 100, sd = 100))
  expect_equal(distribution_moments(sev_dist_options, "Fixed_Severity", 250), c(mean = 250, sd = 0))
})

test_that("Pareto moments are infinite where they do not exist", {
  heavy <- distribution_moments(sev_dist_options, "Pareto", c(1.5, 100))
  expect_equal(heavy[["mean"]], 1.5 * 100 / 0.5)
  expect_true(is.infinite(heavy[["sd"]]))

  heavier <- distribution_moments(sev_dist_options, "Pareto", c(0.9, 100))
  expect_true(is.infinite(heavier[["mean"]]))
  expect_true(is.infinite(heavier[["sd"]]))

  light <- distribution_moments(sev_dist_options, "Pareto", c(3, 100))
  expect_equal(light, c(mean = 150, sd = 100 / 2 * sqrt(3 / 1)))
})

test_that("parameters may be given as a list, in paramIDs order", {
  expect_equal(distribution_moments(sev_dist_options, "Gamma", list(2, 500)), c(mean = 1000, sd = sqrt(2) * 500))
  expect_equal(distribution_moments(freq_dist_options, "Poisson", list(3)), c(mean = 3, sd = sqrt(3)))
})

test_that("missing or invalid parameters give NA moments", {
  na_moments <- c(mean = NA_real_, sd = NA_real_)
  expect_equal(distribution_moments(sev_dist_options, "Gamma", 2), na_moments)
  expect_equal(distribution_moments(sev_dist_options, "Gamma", list(2, NULL)), na_moments)
  expect_equal(distribution_moments(sev_dist_options, "Gamma", c(2, NA)), na_moments)
  expect_equal(distribution_moments(sev_dist_options, "Gamma", NULL), na_moments)
  expect_equal(distribution_moments(sev_dist_options, "Gamma", c("a", "b")), na_moments)
  expect_equal(distribution_moments(sev_dist_options, "Weibull", c(1, 2)), na_moments)
  expect_equal(distribution_moments(sev_dist_options, NULL, c(1, 2)), na_moments)
  #out of range or fractional where a whole number is required
  expect_equal(distribution_moments(sev_dist_options, "Exponential", 0), na_moments)
  expect_equal(distribution_moments(freq_dist_options, "Binomial", c(2.5, 0.3)), na_moments)
  expect_equal(distribution_moments(freq_dist_options, "Binomial", c(3, 1.5)), na_moments)
  expect_equal(distribution_moments(freq_dist_options, "Poisson", -1), na_moments)
})

test_that("the Normal truncated at zero matches the closed form", {
  mu <- 100
  sigma <- 200
  a <- -mu / sigma
  lambda <- dnorm(a) / pnorm(a, lower.tail = FALSE)
  expected <- c(mean = mu + sigma * lambda, sd = sigma * sqrt(1 + a * lambda - lambda^2))
  expect_equal(distribution_moments(sev_dist_options, "Normal", c(mu, sigma), truncate_at_zero = TRUE), expected)
  expect_gt(expected[["mean"]], mu)
  expect_lt(expected[["sd"]], sigma)

  #a Normal far above zero is barely changed by truncation
  expect_equal(distribution_moments(sev_dist_options, "Normal", c(1000, 10), truncate_at_zero = TRUE),
               c(mean = 1000, sd = 10), tolerance = 1e-6)
  #and one with no mass above zero has no moments
  expect_equal(distribution_moments(sev_dist_options, "Normal", c(-1000, 1), truncate_at_zero = TRUE),
               c(mean = NA_real_, sd = NA_real_))
})

test_that("truncation at zero is ignored for other distributions", {
  expect_equal(distribution_moments(sev_dist_options, "Gamma", c(2, 500), truncate_at_zero = TRUE),
               distribution_moments(sev_dist_options, "Gamma", c(2, 500)))
  expect_equal(distribution_moments(freq_dist_options, "Poisson", 3, truncate_at_zero = TRUE),
               c(mean = 3, sd = sqrt(3)))
})
