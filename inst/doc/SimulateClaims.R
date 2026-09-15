## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 4)

## ----basic run----------------------------------------------------------------
library("NetSimR")

claims <- simulate_claims(
  5000, frequency = "Poisson", frequency_params = 3,
  severity = "LogNormal", severity_params = c(8, 1.5), seed = 1
)
head(claims)

## ----summary statistics-------------------------------------------------------
tail_stats <- function(x) {
  worst <- sort(x, decreasing = TRUE)[seq_len(ceiling(0.005 * length(x)))]
  c(mean = mean(x), sd = sd(x), VaR99.5 = unname(quantile(x, 0.995)), TVaR99.5 = mean(worst))
}
round(tail_stats(claims$total_claims))

# the expected total is the mean number of claims times the mean claim size
3 * exp(8 + 1.5^2 / 2)

## ----histogram----------------------------------------------------------------
hist(claims$total_claims, breaks = 100, main = "Total claims per period", xlab = "Total claims")

## ----named parameters---------------------------------------------------------
named <- simulate_claims(
  5000, frequency = "negative binomial", frequency_params = c(beta = 1.5, r = 2),
  severity = "gamma", severity_params = c(shape = 2, scale = 5000), seed = 1
)
in_order <- simulate_claims(5000, "Negative_Binomial", c(2, 1.5), "Gamma", c(2, 5000), seed = 1)
identical(named, in_order)

## ----pareto tail--------------------------------------------------------------
tail <- simulate_claims(
  5000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5
)
round(rbind(lognormal = tail_stats(claims$total_claims), pareto_tail = tail_stats(tail$total_claims)))

## ----pareto mean--------------------------------------------------------------
c(expected = 3 * SlicedLNormParetoMean(8, 1.5, 100000, 1.5), simulated = mean(tail$total_claims))

## ----return periods-----------------------------------------------------------
return_periods <- function(x) data.frame(rp = 1 / (1 - ppoints(length(x))), loss = sort(x))
lognormal_rp <- return_periods(claims$total_claims)
pareto_rp <- return_periods(tail$total_claims)
keep <- lognormal_rp$rp <= 500
plot(loss ~ rp, data = pareto_rp[keep, ], type = "l", log = "x", col = "firebrick", lwd = 2,
     xlab = "Return period (years)", ylab = "", yaxt = "n")
axis(2, at = axTicks(2), labels = format(axTicks(2), big.mark = ",", scientific = FALSE), las = 1, cex.axis = 0.8)
lines(loss ~ rp, data = lognormal_rp[keep, ], col = "steelblue", lwd = 2)
legend("topleft", c("Pareto tail above 100,000", "Log-Normal"), col = c("firebrick", "steelblue"), lwd = 2, bty = "n")

## ----eel layer----------------------------------------------------------------
layer <- simulate_claims(
  5000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2
)
head(layer)

## ----layer metrics------------------------------------------------------------
# amounts ceded to the layer
round(c(largest = max(layer$total_claims), expected_loss = mean(layer$total_claims)))
# probabilities, and the expected loss as a share of the limit
round(c(
  chance_hit = mean(layer$total_claims > 0),
  loss_on_line = mean(layer$total_claims) / 50000,
  chance_all_reinstatements_used = mean(layer$number_of_reinstatements_used >= 2)
), 4)
# the net (retained) losses
round(tail_stats(layer$gross_claims - layer$total_claims))

## ----aggregate deductible example---------------------------------------------
simulate_claims(
  3, frequency = "Fixed_number_of_Counts", frequency_params = 3,
  severity = "Fixed_Severity", severity_params = 100,
  eel_layer = "limited", eel_deductible = 0, eel_limit = 100, eel_reinstatements = 0,
  agg_layer = "unlimited", agg_deductible = 50
)

## ----aggregate deductible-----------------------------------------------------
with_deductible <- simulate_claims(
  5000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2, agg_layer = "unlimited", agg_deductible = 25000
)
c(without = mean(layer$total_claims), with = mean(with_deductible$total_claims))

## ----share of large claims----------------------------------------------------
1 - pSlicedLNormPareto(20000, 8, 1.5, 100000, 1.5)

## ----gross false--------------------------------------------------------------
fast <- simulate_claims(
  20000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2, gross = FALSE
)
names(fast)
full <- simulate_claims(
  20000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2
)
round(rbind(gross_false = tail_stats(fast$total_claims), gross_true = tail_stats(full$total_claims)))

## ----seed---------------------------------------------------------------------
a <- simulate_claims(2000, "Poisson", 3, "Gamma", c(2, 5000), seed = 42)
b <- simulate_claims(2000, "Poisson", 3, "Gamma", c(2, 5000), seed = 42)
identical(a, b)

set.seed(42)
c1 <- simulate_claims(2000, "Poisson", 3, "Gamma", c(2, 5000))
set.seed(42)
c2 <- simulate_claims(2000, "Poisson", 3, "Gamma", c(2, 5000))
identical(c1, c2)

## ----parallel, eval=FALSE-----------------------------------------------------
# future::plan(future::multisession, workers = 2)
# p <- simulate_claims(2000, "Poisson", 3, "Gamma", c(2, 5000), seed = 42, parallel = TRUE)
# identical(p, a)  # TRUE
# future::plan(future::sequential)

