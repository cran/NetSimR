#' Type 7 quantiles of an already sorted vector
#'
#' Mirrors \code{stats::quantile(type = 7)} so that the sorted vector is reused
#' for every probability instead of being sorted again for each one.
#'
#' @param sorted A numeric vector sorted in increasing order, without NAs.
#' @param probs Probabilities in [0, 1].
#' @return A numeric vector of quantiles, one per probability.
#' @noRd
sorted_quantile <- function(sorted, probs) {
  n <- length(sorted)
  index <- 1 + max(n - 1, 0) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  qs <- sorted[lo]
  #interpolate only between different values, as stats::quantile does
  i <- which(index > lo & sorted[hi] != qs)
  h <- (index - lo)[i]
  qs[i] <- (1 - h) * qs[i] + h * sorted[hi[i]]
  qs
}

#' Decimal places for showing amounts of a given scale
#'
#' Whole numbers from 1,000 up and two decimals from 0.1; smaller amounts (e.g. results
#' in millions) get more decimals, so they keep about three significant digits instead
#' of showing as zero.
#'
#' @param x Amounts; the largest finite absolute value sets the scale.
#' @return A whole number of decimal places, from 0 to 10.
#' @noRd
display_digits <- function(x) {
  x <- abs(suppressWarnings(as.numeric(x)))
  x <- x[is.finite(x)]
  largest <- if (length(x) > 0) max(x) else 0
  if (largest >= 1000) return(0)
  if (largest >= 0.1 || largest == 0) return(2)
  min(10, 2 - floor(log10(largest)))
}

#' Return periods marked on the x axis of the return-period charts
#'
#' Shared by the report and the Compare tab; each chart keeps the ticks up to its own
#' longest return period (a tenth of the simulations, so 1 in 1,000,000 at the largest
#' run of 10,000,000 simulations).
#' @noRd
return_period_axis_ticks <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000,
                              1e5, 2e5, 5e5, 1e6)

#' Labels for amount ticks on a chart axis
#'
#' Whole numbers with thousands separators from 100 up, and scientific notation from
#' 1e15 up, where the fixed format would give labels too wide for the chart (e.g. the
#' totals of a Pareto severity with a tiny alpha).
#'
#' @param at The tick values.
#' @return A character vector of labels, one per tick.
#' @noRd
axis_amount_labels <- function(at) {
  largest <- max(c(abs(at[is.finite(at)]), 0))
  if (largest >= 1e15) return(format(at, scientific = TRUE, digits = 3, trim = TRUE))
  if (largest >= 100) formatC(at, format = "f", digits = 0, big.mark = ",") else format(at, trim = TRUE)
}

#' TVaR of an already sorted vector
#'
#' The average of the worst (1 - p) share of simulations, which stays correct when
#' many totals tie (for example at zero), unlike averaging everything at or above VaR.
#'
#' @param sorted A numeric vector sorted in increasing order, without NAs.
#' @param p A single probability.
#' @return The TVaR at \code{p}.
#' @noRd
sorted_tvar <- function(sorted, p) {
  n <- length(sorted)
  #1 - p is not exact in floating point (1000 * (1 - 0.995) is 5.000000000000004),
  #so round away that noise before taking the ceiling
  k <- max(1, ceiling(round(n * (1 - p), 8)))
  mean(sorted[seq.int(n, n - k + 1)])
}

#' Summarise a simulation run
#'
#' Computes every figure the report and the app show from the settings of a run
#' and its simulated totals. Each series is sorted once and the sorted vector is
#' reused for all percentiles, so a run of a million simulations takes a few
#' seconds at most. All numbers are returned unformatted.
#'
#' @param settings The list of \code{simulate_function} arguments used for the run.
#' @param results The data frame returned by \code{simulate_function}, or a numeric
#'   vector of total claims.
#' @return A list with the elements \code{n}, \code{role} ("gross", "ceded", "net"
#'   or "mixed"), \code{modelled_label}, \code{totals}, \code{stats},
#'   \code{percentiles}, \code{percentiles_dropped}, \code{gross}, \code{layer}
#'   and \code{frequency}. \code{gross}, \code{layer} and \code{frequency} are
#'   \code{NULL} when they do not apply. \code{gross} holds \code{table}, \code{series}
#'   and \code{unknown}, the number of simulations whose gross and modelled totals are
#'   both infinite, so that their difference is unknown (NA) and its column blank.
#' @noRd
summarise_simulation <- function(settings, results) {
  s <- settings
  if (is.numeric(results)) results <- data.frame(total_claims = results)
  results <- as.data.frame(results)
  if (!"total_claims" %in% names(results)) stop("The results need a total_claims column.", call. = FALSE)
  results <- results[!is.na(results$total_claims), , drop = FALSE]
  totals <- as.numeric(results$total_claims)
  n <- length(totals)
  if (n == 0) stop("There are no simulated totals to summarise.", call. = FALSE)
  column_or_null <- function(name) if (name %in% names(results)) as.numeric(results[[name]]) else NULL
  gross <- column_or_null("gross_claims")
  counts <- column_or_null("claim_counts")
  reinstatements_used <- column_or_null("number_of_reinstatements_used")

  is_blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))
  is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  #infinite totals give a NaN standard deviation; it is reported as missing (NA)
  sd_or_na <- function(x) {
    if (n < 2) return(NA_real_)
    value <- stats::sd(x)
    if (is.nan(value)) NA_real_ else value
  }

  # ---------- what the modelled totals represent ----------
  eel <- s$reinsuranceStructureEEL
  al <- s$reinsuranceStructureAL
  structure_kind <- function(x) {
    if (is_blank(x) || identical(x, "No Reinsurance Structure")) return("none")
    if (x %in% c("Unlimited Layer", "Limited Layer")) return("layer")
    if (identical(x, "Exclude Layer")) return("exclude")
    "other"
  }
  kinds <- c(structure_kind(eel), structure_kind(al))
  #layers give the losses ceded to them; exclusions leave the retained (net) losses
  role <- if (all(kinds == "none")) {
    "gross"
  } else if (all(kinds %in% c("none", "layer"))) {
    "ceded"
  } else if (all(kinds %in% c("none", "exclude"))) {
    "net"
  } else {
    "mixed"
  }
  modelled_label <- switch(role, gross = "Total claims", ceded = "Ceded", net = "Net", mixed = "After structures")

  # ---------- statistics of the modelled totals ----------
  sorted <- sort(totals)
  q <- function(p) sorted_quantile(sorted, p)
  totals_mean <- mean(totals)
  totals_sd <- sd_or_na(totals)
  var995 <- q(0.995)

  #accuracy: standard error of the mean, and a distribution-free 95% range for VaR 99.5%
  #from the order statistics around the 99.5th percentile
  se <- if (n > 1) totals_sd / sqrt(n) else NA_real_
  p_tail <- 0.995
  spread <- 1.96 * sqrt(n * p_tail * (1 - p_tail))
  var995_ci <- c(
    sorted[max(1, floor(n * p_tail - spread))],
    sorted[min(n, ceiling(n * p_tail + spread))]
  )

  stats <- list(
    mean = totals_mean,
    median = q(0.5),
    sd = totals_sd,
    cv = if (isTRUE(totals_mean != 0) && !is.na(totals_sd)) totals_sd / totals_mean else NA_real_,
    min = sorted[1],
    max = sorted[n],
    var99 = q(0.99),
    var995 = var995,
    tvar995 = sorted_tvar(sorted, 0.995),
    se = se,
    mean_ci = c(totals_mean - 1.96 * se, totals_mean + 1.96 * se),
    var995_ci = var995_ci,
    beyond_var995 = floor(round(n * (1 - p_tail), 8)),
    zero_share = mean(totals == 0)
  )

  # ---------- percentiles, VaR and TVaR ----------
  #a percentile is only listed when at least 10 simulations lie beyond it
  all_probs <- c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.995, 0.996, 0.998, 0.999)
  probs <- all_probs[round(n * (1 - all_probs), 8) >= 10]
  if (length(probs) == 0) probs <- 0.5
  percentiles <- data.frame(
    prob = probs,
    return_period = 1 / (1 - probs),
    var = q(probs),
    tvar = vapply(probs, function(p) sorted_tvar(sorted, p), numeric(1))
  )

  # ---------- gross, ceded and net ----------
  gross_summary <- NULL
  if (!is.null(gross) && role != "gross") {
    difference <- gross - totals
    #when the gross and the modelled total are both infinite, the difference is unknown
    #(Inf - Inf is NaN); it is kept as NA, and that series gets no statistics
    unknown <- is.infinite(gross) & is.infinite(totals)
    difference[unknown] <- NA_real_
    series <- switch(
      role,
      ceded = list(Gross = gross, Ceded = totals, Net = difference),
      net = list(Gross = gross, Ceded = difference, Net = totals),
      mixed = list(Gross = gross, `After structures` = totals, Difference = difference)
    )
    gross_mean <- mean(gross)
    columns <- lapply(series, function(x) {
      #sort() would drop the unknown values and give statistics of the others only
      if (anyNA(x)) return(rep(NA_real_, 7))
      x_sorted <- sort(x)
      x_mean <- mean(x)
      share <- if (gross_mean > 0) x_mean / gross_mean else NA_real_
      c(
        x_mean,
        #Inf / Inf, when both means are infinite
        if (is.nan(share)) NA_real_ else share,
        sd_or_na(x),
        sorted_quantile(x_sorted, c(0.5, 0.99, 0.995)),
        sorted_tvar(x_sorted, 0.995)
      )
    })
    table <- data.frame(
      metric = c("Mean", "Share of gross mean", "Standard deviation", "Median", "VaR 99%", "VaR 99.5%", "TVaR 99.5%"),
      columns,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    gross_summary <- list(table = table, series = series, unknown = sum(unknown))
  }

  # ---------- layer metrics ----------
  layer <- NULL
  if (role == "ceded") {
    eel_limit <- s$reinsurance_structure_eel_limit_amount
    al_limit <- s$reinsurance_structure_al_limit_amount
    reinstatements_limited <- identical(eel, "Limited Layer") && isTRUE(s$reinsuranceStructureLimitedReinstatements) &&
      is_number(s$reinsuranceStructureReinstatementLimit)

    #the most the layers can pay in one period: the reinstatement capacity and the aggregate
    #limit; the aggregate deductible comes off the recoveries before either caps them
    capacity <- Inf
    if (reinstatements_limited && is_number(eel_limit)) {
      capacity <- (s$reinsuranceStructureReinstatementLimit + 1) * eel_limit
    }
    if (identical(al, "Limited Layer") && is_number(al_limit)) capacity <- min(capacity, al_limit)

    #loss on line uses the aggregate limit when there is one, otherwise the each-and-every-loss limit
    line_limit <- NA_real_
    line_limit_name <- NA_character_
    if (identical(al, "Limited Layer") && is_number(al_limit)) {
      line_limit <- al_limit
      line_limit_name <- "the aggregate limit"
    } else if (identical(eel, "Limited Layer") && is_number(eel_limit)) {
      line_limit <- eel_limit
      line_limit_name <- "the each-and-every-loss limit"
    }

    hit <- totals > 0
    exhaust_prob <- NA_real_
    #totals are unrounded; the relative tolerance only absorbs floating-point error in the sums
    if (is.finite(capacity)) {
      exhaust_prob <- mean(totals >= capacity * (1 - 1e-9))
    }

    #reinstatement figures need the limit and the column that simulate_function adds for it
    reinstatements_avg <- NA_real_
    reinstatements_all_used_prob <- NA_real_
    reinstatement_limit <- NA_real_
    if (reinstatements_limited && !is.null(reinstatements_used)) {
      reinstatement_limit <- s$reinsuranceStructureReinstatementLimit
      reinstatements_avg <- mean(reinstatements_used)
      #every reinstatement is used once the recoveries reach reinstatements * limit
      reinstatements_all_used_prob <- mean(reinstatements_used >= reinstatement_limit * (1 - 1e-9))
    }

    layer <- list(
      hit_prob = mean(hit),
      avg_loss_when_hit = if (any(hit)) mean(totals[hit]) else NA_real_,
      expected_loss = totals_mean,
      loss_on_line = if (!is.na(line_limit) && line_limit > 0) totals_mean / line_limit else NA_real_,
      line_limit = line_limit,
      line_limit_name = line_limit_name,
      capacity = capacity,
      exhaust_prob = exhaust_prob,
      reinstatements_avg = reinstatements_avg,
      reinstatements_all_used_prob = reinstatements_all_used_prob,
      reinstatement_limit = reinstatement_limit
    )
  }

  # ---------- claim frequency ----------
  frequency <- NULL
  if (!is.null(counts)) {
    counts_sorted <- sort(counts)
    frequency <- list(
      mean = mean(counts),
      sd = sd_or_na(counts),
      p_zero = mean(counts == 0),
      p99 = sorted_quantile(counts_sorted, 0.99),
      max = counts_sorted[n]
    )
  }

  list(
    n = n,
    role = role,
    modelled_label = modelled_label,
    totals = totals,
    stats = stats,
    percentiles = percentiles,
    percentiles_dropped = length(probs) < length(all_probs),
    gross = gross_summary,
    layer = layer,
    frequency = frequency
  )
}
