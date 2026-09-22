#the aggregate step: cap -> EEL layer per claim -> annual sum -> aggregate deductible ->
#aggregate limit and reinstatement capacity

#every gross / shortcuts combination of simulate_claims(), which must all give the same
#exact results for fixed claims
run_all_paths <- function(...) {
  paths <- expand.grid(gross = c(TRUE, FALSE), shortcuts = c(TRUE, FALSE))
  lapply(seq_len(nrow(paths)), function(i) {
    simulate_claims(..., gross = paths$gross[i], shortcuts = paths$shortcuts[i])
  })
}

#three claims of 100 in every simulation
three_claims <- function(...) {
  run_all_paths(5, "Fixed_number_of_Counts", 3, "Fixed_Severity", 100, seed = 1, ...)
}

expect_totals <- function(runs, total, used = NULL) {
  for (res in runs) {
    expect_equal(res$total_claims, rep(total, nrow(res)))
    if (!is.null(used)) expect_equal(res$number_of_reinstatements_used, rep(used, nrow(res)))
  }
}

test_that("the aggregate deductible comes off the EEL recoveries before the reinstatement capacity", {
  layer <- list(eel_layer = "limited", eel_deductible = 0, eel_limit = 100)
  no_reinstatements <- c(layer, list(eel_reinstatements = 0))
  #the three claims recover 300; min(max(300 - 50, 0), 100) = 100 (it was min(300, 100) - 50 = 50)
  expect_totals(do.call(three_claims, c(no_reinstatements, list(agg_layer = "unlimited", agg_deductible = 50))), 100, 0)
  expect_totals(do.call(three_claims, c(no_reinstatements, list(agg_layer = "limited", agg_deductible = 50, agg_limit = 1000))), 100, 0)
  expect_totals(do.call(three_claims, c(no_reinstatements, list(agg_layer = "limited", agg_deductible = 50, agg_limit = 70))), 70, 0)
  expect_totals(do.call(three_claims, c(no_reinstatements, list(agg_layer = "unlimited", agg_deductible = 250))), 50, 0)
  expect_totals(do.call(three_claims, c(no_reinstatements, list(agg_layer = "unlimited", agg_deductible = 350))), 0, 0)

  #one reinstatement: capacity 200; reinstatements used are counted after the deductible
  one <- c(layer, list(eel_reinstatements = 1))
  expect_totals(do.call(three_claims, c(one, list(agg_layer = "unlimited", agg_deductible = 50))), 200, 1)
  expect_totals(do.call(three_claims, c(one, list(agg_layer = "unlimited", agg_deductible = 150))), 150, 1)
  expect_totals(do.call(three_claims, c(one, list(agg_layer = "unlimited", agg_deductible = 250))), 50, 0.5)
  expect_totals(do.call(three_claims, c(one, list(agg_layer = "limited", agg_deductible = 50, agg_limit = 80))), 80, 0.8)
})

test_that("Binomial(7, 1) claims of 1,000 through 300 xs 800 follow the aggregate rule exactly", {
  #every claim recovers 200, so each simulation recovers 1,400
  for (reinstatements in c(0, 1, 3, 5)) {
    for (deductible in c(0, 100, 500, 1400, 2000)) {
      for (agg_limit in c(Inf, 700)) {
        expected <- min(max(1400 - deductible, 0), agg_limit, 300 * (reinstatements + 1))
        runs <- run_all_paths(
          3, "Binomial", c(7, 1), "Fixed_Severity", 1000, seed = 1,
          eel_layer = "limited", eel_deductible = 800, eel_limit = 300, eel_reinstatements = reinstatements,
          agg_layer = if (is.finite(agg_limit)) "limited" else "unlimited", agg_deductible = deductible,
          agg_limit = if (is.finite(agg_limit)) agg_limit
        )
        info <- paste("R", reinstatements, "D", deductible, "L", agg_limit)
        for (res in runs) {
          expect_equal(res$claim_counts, rep(7, 3), info = info)
          expect_equal(res$total_claims, rep(expected, 3), info = info)
          expect_equal(res$number_of_reinstatements_used, rep(min(expected / 300, reinstatements), 3), info = info)
          if ("gross_claims" %in% names(res)) expect_equal(res$gross_claims, rep(7000, 3), info = info)
        }
      }
    }
  }
})

test_that("combinations without a reinstatement capacity and an aggregate layer keep their order", {
  #no EEL layer: the aggregate layer applies to the gross total of 300
  expect_totals(three_claims(agg_layer = "limited", agg_deductible = 50, agg_limit = 200), 200)
  #an unlimited EEL layer has no capacity: 3 * 60 = 180, less 50
  expect_totals(three_claims(eel_layer = "unlimited", eel_deductible = 40, agg_layer = "limited",
                             agg_deductible = 50, agg_limit = 1000), 130)
  #an EEL exclusion keeps 100 - 50 of each claim: 150, less 30
  expect_totals(three_claims(eel_layer = "exclude", eel_deductible = 20, eel_limit = 50,
                             agg_layer = "unlimited", agg_deductible = 30), 120)
  #unlimited reinstatements: 3 * 60 = 180, less 50, within the aggregate limit of 100
  expect_totals(three_claims(eel_layer = "limited", eel_deductible = 20, eel_limit = 60,
                             agg_layer = "limited", agg_deductible = 50, agg_limit = 100), 100)
  #no aggregate layer: the capacity of 120 caps the recoveries of 180
  expect_totals(three_claims(eel_layer = "limited", eel_deductible = 20, eel_limit = 60, eel_reinstatements = 1), 120, 1)
  #an aggregate exclusion is taken out of the capped recoveries: 120 - min(120 - 30, 50) = 70
  expect_totals(three_claims(eel_layer = "limited", eel_deductible = 20, eel_limit = 60, eel_reinstatements = 1,
                             agg_layer = "exclude", agg_deductible = 30, agg_limit = 50), 70, 1)
})

test_that("the summary capacity is the reinstatement capacity within the aggregate limit", {
  settings <- layered_settings(reinsurance_structure_eel_limit_amount = 100, reinsurance_structure_eel_dedctible_amount = 0,
                               reinsuranceStructureReinstatementLimit = 0,
                               reinsurance_structure_al_dedctible_amount = 50, reinsurance_structure_al_limit_amount = 1000,
                               freqDistr = "Fixed_number_of_Counts", freq_params = 3,
                               sevDistr = "Fixed_Severity", sev_params = 100, numOfSimulations = 20)
  res <- do.call(simulate_function, settings)
  layer <- summarise_simulation(settings, res)$layer
  expect_equal(layer$capacity, 100)
  expect_equal(layer$exhaust_prob, 1)
  expect_equal(layer$reinstatements_all_used_prob, 1)
  html_file <- tempfile(fileext = ".html")
  on.exit(unlink(html_file), add = TRUE)
  write_simulation_report(html_file, settings, res)
  html <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("the aggregate deductible comes off first", html, fixed = TRUE))
})

# ---------- statistical comparison with a naive reference simulator ----------

#one claim-by-claim simulation of the documented model, with other sampling methods
reference_simulation <- function(n, cfg) {
  counts <- switch(cfg$freq,
    Poisson = stats::rpois(n, cfg$fp[1]),
    Negative_Binomial = stats::rnbinom(n, size = cfg$fp[1], prob = 1 / (1 + cfg$fp[2])),
    Binomial = stats::rbinom(n, cfg$fp[1], cfg$fp[2]))
  m <- sum(counts)
  claims <- switch(cfg$sev,
    LogNormal = exp(stats::rnorm(m, cfg$sp[1], cfg$sp[2])),
    Gamma = stats::rgamma(m, shape = cfg$sp[1], rate = 1 / cfg$sp[2]),
    Exponential = -log(stats::runif(m)) / cfg$sp[1],
    Pareto = cfg$sp[2] * (1 - stats::runif(m))^(-1 / cfg$sp[1]))
  if (!is.null(cfg$cap)) claims <- pmin(claims, cfg$cap)
  recovered <- pmin(pmax(claims - cfg$eel_d, 0), cfg$eel_l)
  sums <- vapply(split(recovered, factor(rep.int(seq_len(n), counts), levels = seq_len(n))), sum, numeric(1))
  capacity <- (cfg$reinst + 1) * cfg$eel_l
  if (cfg$al == "exclude") {
    capped <- pmin(sums, capacity)
    total <- capped - pmin(pmax(capped - cfg$al_d, 0), cfg$al_l)
  } else {
    capped <- pmin(pmax(sums - cfg$al_d, 0), if (cfg$al == "limited") cfg$al_l else Inf, capacity)
    total <- capped
  }
  list(total = unname(total), used = unname(pmin(capped / cfg$eel_l, cfg$reinst)))
}

#largest z-score between two samples: means, the share of zeros and the CDF at pooled quantiles
largest_z <- function(a, b) {
  n_a <- length(a)
  n_b <- length(b)
  z_prop <- function(pa, pb) {
    pooled <- (pa * n_a + pb * n_b) / (n_a + n_b)
    se <- sqrt(pooled * (1 - pooled) * (1 / n_a + 1 / n_b))
    if (se == 0) return(if (pa == pb) 0 else Inf)
    abs(pa - pb) / se
  }
  se_mean <- sqrt(stats::var(a) / n_a + stats::var(b) / n_b)
  z <- c(if (se_mean > 0) abs(mean(a) - mean(b)) / se_mean else if (mean(a) == mean(b)) 0 else Inf,
         z_prop(mean(a == 0), mean(b == 0)))
  for (q in stats::quantile(c(a, b), c(0.25, 0.5, 0.75, 0.9, 0.99), names = FALSE)) {
    z <- c(z, z_prop(mean(a <= q), mean(b <= q)))
  }
  max(z)
}

test_that("simulate_function agrees with a naive reference simulator on the aggregate order", {
  skip_on_cran()
  n <- 20000
  configs <- list(
    list(freq = "Poisson", fp = 3, sev = "LogNormal", sp = c(6, 1.5), eel_d = 300, eel_l = 500, reinst = 1,
         al = "unlimited", al_d = 400),
    list(freq = "Negative_Binomial", fp = c(2, 1.5), sev = "Gamma", sp = c(2, 400), eel_d = 500, eel_l = 800,
         reinst = 2, al = "limited", al_d = 300, al_l = 1500),
    list(freq = "Binomial", fp = c(10, 0.3), sev = "Pareto", sp = c(2, 200), cap = 5000, eel_d = 400,
         eel_l = 1000, reinst = 0, al = "limited", al_d = 100, al_l = 5000),
    list(freq = "Poisson", fp = 2, sev = "Exponential", sp = 0.002, eel_d = 200, eel_l = 400, reinst = 1,
         al = "exclude", al_d = 100, al_l = 300)
  )
  for (i in seq_along(configs)) {
    cfg <- configs[[i]]
    set.seed(100 + i)
    reference <- reference_simulation(n, cfg)
    runs <- run_all_paths(
      n, cfg$freq, cfg$fp, cfg$sev, cfg$sp, seed = i, severity_cap = cfg$cap,
      eel_layer = "limited", eel_deductible = cfg$eel_d, eel_limit = cfg$eel_l, eel_reinstatements = cfg$reinst,
      agg_layer = cfg$al, agg_deductible = cfg$al_d, agg_limit = cfg$al_l
    )
    for (res in runs) {
      expect_lt(largest_z(res$total_claims, reference$total), 5, label = paste("config", i, "totals"))
      expect_lt(largest_z(res$number_of_reinstatements_used, reference$used), 5,
                label = paste("config", i, "reinstatements used"))
    }
  }
})
