# NetSimR

[![R-CMD-check](https://github.com/NetSimAnalytics/NetSimR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NetSimAnalytics/NetSimR/actions/workflows/R-CMD-check.yaml)

NetSimR is an R package of actuarial functions for non-life insurance and
reinsurance pricing, reserving and capital modelling, with three Shiny tools
for simulating claims and fitting distributions and GLMs.

## Installation

```r
# the released version from CRAN
install.packages("NetSimR")

# the development version from GitHub
# install.packages("remotes")
remotes::install_github("NetSimAnalytics/NetSimR")
```

## What it does

- **Capped means, exposure curves and increased limit factors (ILFs)** for
  LogNormal, Gamma, Pareto, sliced LogNormal-Pareto and sliced Gamma-Pareto
  claim sizes.
- **Sliced distributions**: mean, density, cdf and quantile function of the
  sliced LogNormal-Pareto and Gamma-Pareto distributions.
- **Pure IBNR exposure** from a LogNormal or Gamma reporting delay.
- **Claims simulation** with a frequency-severity model, Pareto tail slices,
  a severity cap, each-and-every-loss layers with reinstatements and aggregate
  layers.
- **Three Shiny tools**: a claims simulator with reports and run comparison, a
  distribution fitting tool for claim counts and sizes, and a GLM fitting tool.

## Examples

```r
library(NetSimR)

# mean claim size capped at 600 for a Pareto with scale 200 and shape 1.2
ParetoCappedMean(600, 200, 1.2)

# increased limit factor from 700 to 1,000 for Gamma claim sizes
ILFGamma(700, 1000, 1, 0.0005)

# Gamma body sliced at 3,000 with a Pareto tail of shape 1.4
pSlicedGammaPareto(5000, 1.2, 0.0004, 3000, 1.4)

# pure IBNR exposure (in days) of three periods, with a Gamma reporting delay
PureIBNRGamma(
  as.Date(c("2006-01-01", "2006-07-01", "2007-01-01")),
  as.Date(c("2006-12-31", "2007-06-30", "2007-12-31")),
  as.Date("2007-10-30"),
  shape = 7, rate = 0.15
)

# 10,000 years of Poisson claim counts with LogNormal claim sizes, a Pareto tail
# above 100,000, and a layer of 50,000 excess of 20,000 on each claim with two
# reinstatements
ceded <- simulate_claims(
  10000, "Poisson", 3, "LogNormal", c(meanlog = 8, sdlog = 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2
)
mean(ceded$total_claims)
```

## Shiny tools

```r
run_shiny_simulator()                  # claims simulator
run_shiny_distribution_fitting_tool()  # frequency and severity fits
run_shiny_glm_fitting_tool()           # generalised linear models
```

## References

The methods are described in articles by Yiannis Parizas in *The Actuary*:
[Free for All](https://www.theactuary.com/2023/03/02/free-all) (2023),
[Escaping the triangle](https://www.theactuary.com/features/2019/06/2019/06/05/escaping-triangle)
(2019) and
[Take to excess](https://www.theactuary.com/features/2019/03/2019/03/06/taken-excess)
(2019).
