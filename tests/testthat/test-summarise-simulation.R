#results built by hand so that the expected figures can be worked out exactly
hand_results <- function(n = 1000) {
  data.frame(
    claim_counts = rep(0:4, length.out = n),
    total_claims = as.numeric(1:n),
    gross_claims = 2 * as.numeric(1:n)
  )
}

test_that("the summary has the documented fields with the right types", {
  settings <- base_settings(numOfSimulations = 1000)
  res <- do.call(simulate_function, settings)
  sm <- summarise_simulation(settings, res)

  expect_type(sm, "list")
  expect_named(sm, c("n", "role", "modelled_label", "totals", "stats", "percentiles",
                     "percentiles_dropped", "gross", "layer", "frequency"))
  expect_equal(sm$n, 1000)
  expect_equal(sm$role, "gross")
  expect_equal(sm$modelled_label, "Total claims")
  expect_equal(sm$totals, res$total_claims)

  expect_named(sm$stats, c("mean", "median", "sd", "cv", "min", "max", "var99", "var995", "tvar995",
                           "se", "mean_ci", "var995_ci", "beyond_var995", "zero_share"))
  expect_true(all(vapply(sm$stats, is.numeric, logical(1))))
  expect_length(sm$stats$mean_ci, 2)
  expect_length(sm$stats$var995_ci, 2)

  expect_s3_class(sm$percentiles, "data.frame")
  expect_named(sm$percentiles, c("prob", "return_period", "var", "tvar"))
  expect_type(sm$percentiles_dropped, "logical")
  expect_null(sm$gross)
  expect_null(sm$layer)
  expect_type(sm$frequency, "list")
})

test_that("the statistics follow their definitions", {
  totals <- as.numeric(1:1000)
  sm <- summarise_simulation(base_settings(), hand_results(1000))
  st <- sm$stats

  expect_equal(st$mean, 500.5)
  expect_equal(st$median, 500.5)
  expect_equal(st$sd, sd(totals))
  expect_equal(st$cv, sd(totals) / 500.5)
  expect_equal(st$min, 1)
  expect_equal(st$max, 1000)
  expect_equal(st$var99, unname(quantile(totals, 0.99, type = 7)))
  expect_equal(st$var995, unname(quantile(totals, 0.995, type = 7)))
  #TVaR 99.5% is the mean of the worst ceiling(1000 * 0.005) = 5 totals
  expect_equal(st$tvar995, mean(996:1000))
  expect_equal(st$se, sd(totals) / sqrt(1000))
  expect_equal(st$mean_ci, 500.5 + c(-1, 1) * 1.96 * sd(totals) / sqrt(1000))
  expect_length(st$var995_ci, 2)
  expect_true(st$var995_ci[1] <= st$var995 && st$var995 <= st$var995_ci[2])
  expect_equal(st$beyond_var995, 5)
  expect_equal(st$zero_share, 0)
})

test_that("VaR agrees with stats::quantile type 7 on random totals", {
  res <- run_simulation(numOfSimulations = 1000)
  sm <- summarise_simulation(base_settings(), res)
  expect_equal(sm$stats$var99, unname(quantile(res$total_claims, 0.99, type = 7)))
  expect_equal(sm$stats$var995, unname(quantile(res$total_claims, 0.995, type = 7)))
  expect_equal(sm$stats$median, unname(quantile(res$total_claims, 0.5, type = 7)))
  expect_equal(sm$percentiles$var, unname(quantile(res$total_claims, sm$percentiles$prob, type = 7)))
})

test_that("TVaR uses the worst ceiling(n * (1 - p)) totals", {
  sm <- summarise_simulation(base_settings(), hand_results(1000))
  pct <- sm$percentiles
  expect_equal(pct$tvar[pct$prob == 0.5], mean(501:1000))
  expect_equal(pct$tvar[pct$prob == 0.9], mean(901:1000))
  expect_equal(pct$tvar[pct$prob == 0.99], mean(991:1000))

  #ties at zero: 990 zero totals and 10 totals of 100
  tied <- c(rep(0, 990), rep(100, 10))
  sm_tied <- summarise_simulation(base_settings(), tied)
  expect_equal(sm_tied$stats$tvar995, 100)
  expect_equal(sm_tied$stats$var995, 100)
  expect_equal(sm_tied$stats$zero_share, 0.99)
  pct_tied <- sm_tied$percentiles
  expect_equal(pct_tied$tvar[pct_tied$prob == 0.99], 100)
  expect_equal(pct_tied$tvar[pct_tied$prob == 0.975], 10 * 100 / 25)

  #with a single total the TVaR is that total
  expect_equal(summarise_simulation(base_settings(), 42)$stats$tvar995, 42)
})

test_that("percentiles drop return periods with fewer than 10 simulations beyond them", {
  all_probs <- c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.995, 0.996, 0.998, 0.999)

  sm_1000 <- summarise_simulation(base_settings(), hand_results(1000))
  expect_equal(sm_1000$percentiles$prob, all_probs[all_probs <= 0.99])
  expect_true(sm_1000$percentiles_dropped)

  sm_10000 <- summarise_simulation(base_settings(), as.numeric(1:10000))
  expect_equal(sm_10000$percentiles$prob, all_probs)
  expect_false(sm_10000$percentiles_dropped)

  sm_20 <- summarise_simulation(base_settings(), as.numeric(1:20))
  expect_equal(sm_20$percentiles$prob, 0.5)
  expect_true(sm_20$percentiles_dropped)

  #the median row is always present
  sm_5 <- summarise_simulation(base_settings(), as.numeric(1:5))
  expect_equal(sm_5$percentiles$prob, 0.5)

  expect_equal(sm_1000$percentiles$return_period, 1 / (1 - sm_1000$percentiles$prob))
  expect_true(all(diff(sm_1000$percentiles$var) >= 0))
  expect_true(all(sm_1000$percentiles$tvar >= sm_1000$percentiles$var))
})

test_that("the role follows the combination of structures", {
  role_of <- function(eel, al) {
    sm <- summarise_simulation(list(reinsuranceStructureEEL = eel, reinsuranceStructureAL = al), as.numeric(1:50))
    c(sm$role, sm$modelled_label)
  }
  none <- "No Reinsurance Structure"
  expect_equal(role_of(none, none), c("gross", "Total claims"))
  expect_equal(role_of("Limited Layer", none), c("ceded", "Ceded"))
  expect_equal(role_of("Unlimited Layer", none), c("ceded", "Ceded"))
  expect_equal(role_of(none, "Unlimited Layer"), c("ceded", "Ceded"))
  expect_equal(role_of("Limited Layer", "Limited Layer"), c("ceded", "Ceded"))
  expect_equal(role_of("Exclude Layer", none), c("net", "Net"))
  expect_equal(role_of(none, "Exclude Layer"), c("net", "Net"))
  expect_equal(role_of("Exclude Layer", "Exclude Layer"), c("net", "Net"))
  expect_equal(role_of("Limited Layer", "Exclude Layer"), c("mixed", "After structures"))
  expect_equal(role_of("Exclude Layer", "Unlimited Layer"), c("mixed", "After structures"))
  #settings without structures count as gross
  expect_equal(summarise_simulation(list(), as.numeric(1:50))$role, "gross")
})

test_that("the gross block splits gross into ceded and net", {
  results <- hand_results(1000)
  totals <- results$total_claims
  gross <- results$gross_claims
  metrics <- c("Mean", "Share of gross mean", "Standard deviation", "Median", "VaR 99%", "VaR 99.5%", "TVaR 99.5%")

  ceded <- summarise_simulation(layered_settings(), results)$gross
  expect_type(ceded, "list")
  expect_named(ceded, c("table", "series"))
  expect_named(ceded$series, c("Gross", "Ceded", "Net"))
  expect_equal(ceded$series$Gross, gross)
  expect_equal(ceded$series$Ceded, totals)
  expect_equal(ceded$series$Net, gross - totals)
  expect_s3_class(ceded$table, "data.frame")
  expect_equal(ceded$table$metric, metrics)
  expect_named(ceded$table, c("metric", "Gross", "Ceded", "Net"))
  expect_true(all(vapply(ceded$table[-1], is.numeric, logical(1))))
  expect_equal(ceded$table$Gross[1], mean(gross))
  expect_equal(ceded$table$Ceded[1], mean(totals))
  expect_equal(ceded$table$Gross[2], 1)
  expect_equal(ceded$table$Ceded[2], 0.5)
  expect_equal(ceded$table$Net[3], sd(gross - totals))
  expect_equal(ceded$table$Ceded[4], 500.5)
  expect_equal(ceded$table$Ceded[5], unname(quantile(totals, 0.99)))
  expect_equal(ceded$table$Ceded[6], unname(quantile(totals, 0.995)))
  expect_equal(ceded$table$Ceded[7], mean(996:1000))

  net <- summarise_simulation(base_settings(reinsuranceStructureEEL = "Exclude Layer"), results)$gross
  expect_named(net$series, c("Gross", "Ceded", "Net"))
  expect_equal(net$series$Net, totals)
  expect_equal(net$series$Ceded, gross - totals)

  mixed <- summarise_simulation(layered_settings(reinsuranceStructureAL = "Exclude Layer"), results)$gross
  expect_named(mixed$series, c("Gross", "After structures", "Difference"))
  expect_equal(mixed$series$`After structures`, totals)
  expect_equal(mixed$series$Difference, gross - totals)
  expect_named(mixed$table, c("metric", "Gross", "After structures", "Difference"))

  #no gross block without a gross column, or when the totals are the gross claims
  expect_null(summarise_simulation(layered_settings(), totals)$gross)
  expect_null(summarise_simulation(base_settings(), results)$gross)
})

test_that("the layer block reports capacity, loss on line and reinstatements", {
  settings <- layered_settings(
    reinsurance_structure_eel_dedctible_amount = 100000,
    reinsurance_structure_eel_limit_amount = 200000,
    reinsuranceStructureReinstatementLimit = 2,
    reinsurance_structure_al_dedctible_amount = 100000,
    reinsurance_structure_al_limit_amount = 1000000
  )
  results <- data.frame(
    total_claims = c(0, 0, 100000, 300000, 500000),
    gross_claims = c(50000, 80000, 400000, 900000, 2000000),
    number_of_reinstatements_used = c(0, 0, 0.5, 1.5, 2)
  )
  layer <- summarise_simulation(settings, results)$layer
  expect_type(layer, "list")
  expect_named(layer, c("hit_prob", "avg_loss_when_hit", "expected_loss", "loss_on_line", "line_limit",
                        "line_limit_name", "capacity", "exhaust_prob", "reinstatements_avg",
                        "reinstatements_all_used_prob", "reinstatement_limit"))
  #(2 + 1) * 200,000 less the aggregate deductible of 100,000, within the aggregate limit
  expect_equal(layer$capacity, 500000)
  expect_equal(layer$line_limit, 1000000)
  expect_type(layer$line_limit_name, "character")
  expect_equal(layer$expected_loss, 180000)
  expect_equal(layer$loss_on_line, 180000 / 1000000)
  expect_equal(layer$hit_prob, 0.6)
  expect_equal(layer$avg_loss_when_hit, 300000)
  expect_equal(layer$exhaust_prob, 0.2)
  expect_equal(layer$reinstatement_limit, 2)
  expect_equal(layer$reinstatements_avg, 0.8)
  expect_equal(layer$reinstatements_all_used_prob, 0.2)

  #a smaller aggregate limit caps the capacity
  smaller <- summarise_simulation(utils::modifyList(settings, list(reinsurance_structure_al_limit_amount = 250000)), results)$layer
  expect_equal(smaller$capacity, 250000)
  expect_equal(smaller$line_limit, 250000)

  #unlimited reinstatements and no aggregate layer: unlimited capacity, loss on line on the EEL limit
  open <- summarise_simulation(
    layered_settings(reinsuranceStructureLimitedReinstatements = FALSE, reinsuranceStructureAL = "No Reinsurance Structure",
                     reinsurance_structure_eel_limit_amount = 5000),
    results
  )$layer
  expect_true(is.infinite(open$capacity))
  expect_equal(open$line_limit, 5000)
  expect_equal(open$loss_on_line, 180000 / 5000)
  expect_true(is.na(open$exhaust_prob))
  expect_true(is.na(open$reinstatements_avg))
  expect_true(is.na(open$reinstatement_limit))

  #an unlimited layer has no limit to relate the loss to
  unlimited <- summarise_simulation(
    base_settings(reinsuranceStructureEEL = "Unlimited Layer", reinsurance_structure_eel_dedctible_amount = 1000),
    results
  )$layer
  expect_true(is.infinite(unlimited$capacity))
  expect_true(is.na(unlimited$line_limit))
  expect_true(is.na(unlimited$loss_on_line))

  #no layer block unless the totals are ceded losses
  expect_null(summarise_simulation(base_settings(), results)$layer)
  expect_null(summarise_simulation(base_settings(reinsuranceStructureEEL = "Exclude Layer"), results)$layer)
  expect_null(summarise_simulation(layered_settings(reinsuranceStructureAL = "Exclude Layer"), results)$layer)
})

test_that("the frequency block is present only with claim counts", {
  results <- data.frame(claim_counts = c(0, 0, 1, 2, 3), total_claims = c(0, 0, 10, 20, 30))
  freq <- summarise_simulation(base_settings(), results)$frequency
  expect_named(freq, c("mean", "sd", "p_zero", "p99", "max"))
  expect_equal(freq$mean, 1.2)
  expect_equal(freq$sd, sd(c(0, 0, 1, 2, 3)))
  expect_equal(freq$p_zero, 0.4)
  expect_equal(freq$p99, unname(quantile(c(0, 0, 1, 2, 3), 0.99)))
  expect_equal(freq$max, 3)

  expect_null(summarise_simulation(base_settings(), results$total_claims)$frequency)
  expect_null(summarise_simulation(base_settings(), data.frame(total_claims = 1:10))$frequency)
})

test_that("a numeric vector of totals is accepted and NA totals are dropped", {
  sm <- summarise_simulation(base_settings(), as.numeric(1:1000))
  expect_equal(sm$n, 1000)
  expect_equal(sm$totals, as.numeric(1:1000))
  expect_null(sm$gross)
  expect_null(sm$frequency)

  with_na <- summarise_simulation(base_settings(), c(1, NA, 3, NA, 5))
  expect_equal(with_na$n, 3)
  expect_equal(with_na$stats$mean, 3)

  expect_error(summarise_simulation(base_settings(), numeric(0)))
  expect_error(summarise_simulation(base_settings(), data.frame(total_claims = c(NA_real_, NA_real_))))
  expect_error(summarise_simulation(base_settings(), data.frame(gross_claims = 1:5)), "total_claims")
})
