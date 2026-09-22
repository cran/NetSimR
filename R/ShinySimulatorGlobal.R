#help functions

#' Parameter to set the maximum number of Pareto slices
#'
#' The largest number of Pareto slices that the simulator app and
#' \code{\link{simulate_claims}} accept, so that the app can build one row of slice inputs
#' for each.
#'
#' @return The maximum number of Pareto Slices.
#' @keywords internal
max_number_of_pareto_slices <- 6

#' The largest number of simulations a run may have
#'
#' Used by the settings validation and by the number of simulations input of the app.
#'
#' @noRd
max_number_of_simulations <- 10000000L

#' Random Pareto generator with a minimum value
#'
#' Pareto (type I) with shape \code{alpha} and minimum \code{x_m}: P(X > x) = (x_m / x)^alpha
#' for x >= x_m. This is not the Lomax parameterisation some packages call "Pareto".
#'
#' @param n Number of values to generate.
#' @param alpha A positive real number. Alpha parameter of the Pareto distribution.
#' @param x_m A positive real number. The minimum value for the Pareto distribution.
#' @return A vector of \code{n} random Pareto variables with parameters \code{alpha} and \code{x_m}.
#' @noRd
rpareto_xm <- function(n, alpha, x_m) x_m / stats::runif(n)^(1/alpha)

#' Random Normal generator truncated at zero
#'
#' Draws from the Normal distribution conditional on being positive, by inverse-CDF
#' sampling on the upper tail (s ~ U(0, P(X > 0)), x = the upper-tail quantile of s),
#' which stays accurate when only a little of the distribution lies above zero.
#'
#' @param n Number of values to generate.
#' @param mean The mean of the underlying Normal distribution.
#' @param sd The standard deviation of the underlying Normal distribution.
#' @return A vector of \code{n} positive random values.
#' @noRd
rnorm_truncated_at_zero <- function(n, mean, sd) {
  above_zero <- stats::pnorm(0, mean = mean, sd = sd, lower.tail = FALSE)
  stats::qnorm(stats::runif(n, min = 0, max = above_zero), mean = mean, sd = sd, lower.tail = FALSE)
}

#' Apply severity cap function
#'
#' @param claims A vector of Claims.
#' @param severity_cap_boolean A variable that if true, the function will cap the claims, otherwise will just return them.
#' @param severity_cap_amount The claim cap value.
#' @return If \code{severity_cap_boolean} is true, then will return the minimum of \code{severity_cap_amount} or \code{claims} otherwise will return \code{claims}. The operation is vectorised.
#' @noRd
apply_severity_cap <- function(claims, severity_cap_boolean, severity_cap_amount){
  if (!severity_cap_boolean) return(claims)
  pmin(claims, severity_cap_amount)
}

#' A vector with the reinsurance structure options
#'
#' The names of the reinsurance structures that \code{\link{apply_deductible_limit}} and
#' \code{\link{simulate_function}} accept, used by the simulator app as the choices of its
#' structure inputs and to validate saved settings.
#'
#' @return The reinsurance structure options
#' @keywords internal
reinsurance_structures_options <- c('No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer')

#' Apply a deductible and limit to claims
#'
#' Works out what a reinsurance structure cedes (or, for an excluded layer, leaves) of each
#' claim, for pricing a layer or checking the simulator's figures by hand.
#'
#' @param gross_claims_data A vector of Claims.
#' @param reinsurance_structure The chosen reinsurance structure, a single string. Options are: 'No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer'; anything else is an error.
#' @param deductible The deductible of the reinsurance structure, zero or more. Not used by 'No Reinsurance Structure'.
#' @param limit The limit of the reinsurance structure, zero or more. Used only by 'Limited Layer' and 'Exclude Layer'.
#' @return A vector with one value per claim: for 'Unlimited Layer' and 'Limited Layer', the
#' amount ceded to the layer; for 'Exclude Layer', the claims with the layer taken out (the
#' claims less what a 'Limited Layer' with the same deductible and limit would cede); for
#' 'No Reinsurance Structure', the claims unchanged. Stops with an error when a deductible
#' or limit the structure uses is negative.
#' @export
#' @examples
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 40, 20)
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 10, 30)
#' apply_deductible_limit(c(100, 50, 20), 'Exclude Layer', 40, 20)
apply_deductible_limit <- function(gross_claims_data, reinsurance_structure, deductible, limit){
  #NA or several structures would otherwise give "missing value where TRUE/FALSE needed" or
  #"the condition has length > 1" below
  if (!(is.character(reinsurance_structure) && length(reinsurance_structure) == 1 &&
        reinsurance_structure %in% reinsurance_structures_options)) {
    given <- if (is.character(reinsurance_structure) && length(reinsurance_structure) == 1) {
      paste0(" (got '", reinsurance_structure, "')")
    } else {
      ""
    }
    stop("Unknown reinsurance structure", given, ": reinsurance_structure must be one of ",
         paste0("'", reinsurance_structures_options, "'", collapse = ", "), ".", call. = FALSE)
  }
  if (reinsurance_structure == 'No Reinsurance Structure') {return(gross_claims_data)}

  #a negative amount would cede more than the claims (or less than nothing)
  if (isTRUE(any(deductible < 0))) stop("The deductible must not be negative.", call. = FALSE)
  layer_claims <- pmax(gross_claims_data - deductible, 0)

  if (reinsurance_structure == 'Unlimited Layer') {return(layer_claims)}

  if (isTRUE(any(limit < 0))) stop("The limit must not be negative.", call. = FALSE)
  limited_layer_claims <- pmin(layer_claims, limit)

  if (reinsurance_structure == 'Limited Layer') {return(limited_layer_claims)}

  # 'Exclude Layer', the only option left
  gross_claims_data - limited_layer_claims
}

#' A function slot that may be empty
#'
#' @noRd
setClassUnion("functionOrNULL", c("function", "NULL"))

#' The class of the distribution objects
#'
#' Each object describes one frequency or severity distribution of the simulator:
#' its parameters (ids used as app input ids, labels, allowed ranges and whether they
#' must be whole numbers) and the functions the simulator needs. \code{simulate_func}
#' draws values; \code{moments_func} gives the mean and standard deviation. Severity
#' distributions also have \code{survival_func} and \code{tail_quantile_func} (the
#' upper-tail probability and its inverse), used to splice Pareto tails and to draw only
#' the claims that reach a layer, and optionally \code{sum_func}, which draws the total
#' of a given number of claims in one step. Frequency distributions may have
#' \code{split_func}, which draws the number of claims and how many of them are large.
#' Allowed ranges are \code{param_min_values} and \code{param_max_values} (NA for no
#' bound), with \code{param_min_strict} and \code{param_max_strict} marking bounds the
#' value may not equal.
#' @keywords internal
distributionClass <- setClass(
  "distributionClass",
  slots = c(
    distrID = "character"
    ,distr_label = "character"
    ,paramIDs = "character"
    ,param_labels = "character"
    ,param_min_values = "numeric"
    ,param_max_values = "numeric"
    ,param_min_strict = "logical"
    ,param_max_strict = "logical"
    ,param_whole_numbers = "logical"
    ,simulate_func = "function"
    ,moments_func = "function"
    ,survival_func = "functionOrNULL"
    ,tail_quantile_func = "functionOrNULL"
    ,sum_func = "functionOrNULL"
    ,split_func = "functionOrNULL"
  ),
  prototype = list(
    param_min_values = numeric(0)
    ,param_max_values = numeric(0)
    ,param_min_strict = logical(0)
    ,param_max_strict = logical(0)
    ,param_whole_numbers = logical(0)
    ,survival_func = NULL
    ,tail_quantile_func = NULL
    ,sum_func = NULL
    ,split_func = NULL
  ),
  validity = function(object) {
    problems <- character(0)
    n <- length(object@paramIDs)
    if (length(object@distrID) != 1 || length(object@distr_label) != 1) {
      problems <- c(problems, "distrID and distr_label must be single strings")
    }
    if (length(object@param_labels) != n) problems <- c(problems, "param_labels must have one label per parameter")
    for (slot_name in c("param_min_values", "param_max_values", "param_min_strict", "param_max_strict", "param_whole_numbers")) {
      if (!(length(methods::slot(object, slot_name)) %in% c(0, n))) {
        problems <- c(problems, paste(slot_name, "must be empty or have one value per parameter"))
      }
    }
    if (is.null(object@survival_func) != is.null(object@tail_quantile_func)) {
      problems <- c(problems, "survival_func and tail_quantile_func must be given together")
    }
    if (length(problems)) problems else TRUE
  }
)

#' A vector with the frequency distribution objects
#'
#' A named list of \code{distributionClass} objects, one for each claim count distribution
#' of the simulator: \code{Poisson}, \code{Negative_Binomial}, \code{Binomial} and
#' \code{Fixed_number_of_Counts}. The names are the values accepted by the \code{freqDistr} argument of
#' \code{simulate_function()}, and the objects hold the parameter ids, labels and ranges
#' of the app's inputs and the functions that draw the claim counts.
#'
#' @return The frequency distribution objects.
#' @keywords internal
freq_dist_options <- c(
  Poisson=distributionClass(
    distrID='Poisson'
    ,distr_label='Poisson'
    ,paramIDs=c("lamda")
    ,param_labels=c("lambda (mean claims)")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rpois(n = number_of_simulations, lambda = parameters[1])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1], sqrt(p[1]))
    #a Poisson count splits into independent Poisson counts of large and small claims
    ,split_func = function(n, p, large_prob) {
      large <- stats::rpois(n, p[1] * large_prob)
      list(total = large + stats::rpois(n, p[1] * (1 - large_prob)), large = large)
    }
  )
  ,Negative_Binomial=distributionClass(
    distrID='Negative_Binomial'
    ,distr_label='Negative Binomial'
    ,paramIDs=c("r", "beta")
    ,param_labels=c("r (shape)", "beta (scale)")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    #Poisson-Gamma mixture with Gamma shape r and scale beta
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rpois(n = number_of_simulations, lambda = stats::rgamma(n = number_of_simulations, shape = parameters[1], scale = parameters[2]))
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1] * p[2], sqrt(p[1] * p[2] * (1 + p[2])))
    #given the Gamma rate, large and small claims are independent Poisson counts
    ,split_func = function(n, p, large_prob) {
      rate <- stats::rgamma(n, shape = p[1], scale = p[2])
      large <- stats::rpois(n, rate * large_prob)
      list(total = large + stats::rpois(n, rate * (1 - large_prob)), large = large)
    }
  )
  ,Binomial=distributionClass(
    distrID='Binomial'
    ,distr_label='Binomial'
    ,paramIDs=c("n", "p")
    ,param_labels=c("n (number of trials)", "p (probability)")
    ,param_min_values=c(0, 0)
    ,param_max_values=c(NA, 1)
    ,param_whole_numbers=c(TRUE, FALSE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rbinom(n = number_of_simulations, size = parameters[1], prob = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1] * p[2], sqrt(p[1] * p[2] * (1 - p[2])))
    #each trial gives a large claim, a small claim or none (a multinomial split)
    ,split_func = function(n, p, large_prob) {
      large_trial_prob <- p[2] * large_prob
      large <- stats::rbinom(n, size = p[1], prob = large_trial_prob)
      small_prob <- if (large_trial_prob < 1) p[2] * (1 - large_prob) / (1 - large_trial_prob) else 0
      list(total = large + stats::rbinom(n, size = p[1] - large, prob = small_prob), large = large)
    }
  )
  ,Fixed_number_of_Counts=distributionClass(
    distrID='Fixed_number_of_Counts'
    ,distr_label='Fixed number of Counts'
    ,paramIDs=c("FixedNumberOfCounts")
    ,param_labels=c("Number of claims")
    ,param_min_values=c(0)
    ,param_whole_numbers=c(TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      rep(parameters[1], number_of_simulations)
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1], 0)
    ,split_func = function(n, p, large_prob) {
      list(total = rep(p[1], n), large = stats::rbinom(n, size = p[1], prob = large_prob))
    }
  )
)

#' A data frame with the frequency distribution parameter placeholders
#'
#' One row per parameter of the frequency distribution with the most parameters, giving
#' the number and output id of the placeholder in which the simulator app renders that
#' parameter's input for the chosen distribution.
#'
#' @return The frequency distribution parameter placeholders.
#' @keywords internal
freq_dist_parameter_placeholders <- data.frame(
  param_number = seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1))))
  ,param_id = paste0("freq_param_", seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1)))))
)

#' A vector with the severity distribution objects
#'
#' A named list of \code{distributionClass} objects, one for each severity distribution
#' of the simulator: Normal, LogNormal, Gamma, Exponential, Pareto and Fixed_Severity.
#' The names are the values accepted by the \code{sevDistr} argument of
#' \code{simulate_function()}. The Normal distribution has its own parameter ids
#' (normal_mean, normal_sd), so that switching between the Normal and the Log-Normal in
#' the app does not carry values across.
#'
#' @return The severity distribution objects.
#' @keywords internal
sev_dist_options <- c(
  Normal=distributionClass(
    distrID='Normal'
    ,distr_label='Normal'
    ,paramIDs=c("normal_mean", "normal_sd")
    ,param_labels=c("Mean", "Standard deviation")
    ,param_min_values=c(NA, 0)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rnorm(n = number_of_simulations, mean = parameters[1], sd = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) {
      if (isTRUE(truncate_at_zero)) truncated_normal_moments(p[1], p[2]) else c(p[1], p[2])
    }
    ,survival_func = function(x, p) stats::pnorm(x, mean = p[1], sd = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qnorm(s, mean = p[1], sd = p[2], lower.tail = FALSE)
    #the sum of n Normal claims is Normal with mean n * mean and variance n * sd^2; where
    #n * mean or sqrt(n) * sd overflows, rnorm() would give NaN, so those simulations sum
    #their claims one by one instead, which gives the infinite totals the claims add up to
    ,sum_func = function(counts, p) {
      totals_mean <- counts * p[1]
      totals_sd <- sqrt(counts) * p[2]
      in_range <- is.finite(totals_mean) & is.finite(totals_sd)
      totals <- numeric(length(counts))
      totals[in_range] <- stats::rnorm(sum(in_range), mean = totals_mean[in_range], sd = totals_sd[in_range])
      overflow <- which(!in_range & counts > 0)
      if (length(overflow) > 0) {
        claims <- stats::rnorm(sum(counts[overflow]), mean = p[1], sd = p[2])
        totals[overflow] <- rowsum(claims, rep.int(seq_along(overflow), counts[overflow]), reorder = FALSE)[, 1]
      }
      totals
    }
  )
  ,LogNormal=distributionClass(
    distrID='LogNormal'
    ,distr_label='Log-Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu (mean of log)", "sigma (sd of log)")
    ,param_min_values=c(NA, 0)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rlnorm(n = number_of_simulations, meanlog = parameters[1], sdlog = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) {
      c(exp(p[1] + p[2]^2 / 2), sqrt(exp(2 * p[1] + p[2]^2) * (exp(p[2]^2) - 1)))
    }
    ,survival_func = function(x, p) stats::plnorm(x, meanlog = p[1], sdlog = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qlnorm(s, meanlog = p[1], sdlog = p[2], lower.tail = FALSE)
  )
  ,Gamma=distributionClass(
    distrID='Gamma'
    ,distr_label='Gamma'
    ,paramIDs=c("shape", "scale")
    ,param_labels=c("Shape", "Scale")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rgamma(n = number_of_simulations, shape = parameters[1], scale = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1] * p[2], sqrt(p[1]) * p[2])
    ,survival_func = function(x, p) stats::pgamma(x, shape = p[1], scale = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qgamma(s, shape = p[1], scale = p[2], lower.tail = FALSE)
    #the sum of n Gamma(shape, scale) claims is Gamma(n * shape, scale)
    ,sum_func = function(counts, p) {
      totals <- numeric(length(counts))
      any_claims <- counts > 0
      totals[any_claims] <- stats::rgamma(sum(any_claims), shape = counts[any_claims] * p[1], scale = p[2])
      totals
    }
  )
  ,Exponential=distributionClass(
    distrID='Exponential'
    ,distr_label='Exponential'
    ,paramIDs=c("rate")
    ,param_labels=c("Rate")
    ,param_min_values=c(0)
    ,param_min_strict=c(TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rexp(n = number_of_simulations, rate = parameters[1])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(1 / p[1], 1 / p[1])
    ,survival_func = function(x, p) stats::pexp(x, rate = p[1], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qexp(s, rate = p[1], lower.tail = FALSE)
    #the sum of n Exponential(rate) claims is Gamma(n, 1 / rate)
    ,sum_func = function(counts, p) {
      totals <- numeric(length(counts))
      any_claims <- counts > 0
      totals[any_claims] <- stats::rgamma(sum(any_claims), shape = counts[any_claims], scale = 1 / p[1])
      totals
    }
  )
  ,Pareto=distributionClass(
    distrID='Pareto'
    ,distr_label='Pareto'
    ,paramIDs=c("alpha", "x_m")
    ,param_labels=c("alpha (shape)", "x_m (minimum)")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      rpareto_xm(n = number_of_simulations, alpha = parameters[1], x_m = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) pareto_moments(p[1], p[2])
    ,survival_func = function(x, p) (p[2] / pmax(x, p[2]))^p[1]
    ,tail_quantile_func = function(s, p) p[2] * s^(-1 / p[1])
  )
  ,Fixed_Severity=distributionClass(
    distrID='Fixed_Severity'
    ,distr_label='Fixed Severity'
    ,paramIDs=c("Fixed_sev_amount")
    ,param_labels=c("Claim amount")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){
      rep(parameters[1], number_of_simulations)
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1], 0)
    ,survival_func = function(x, p) as.numeric(x < p[1])
    ,tail_quantile_func = function(s, p) rep(p[1], length(s))
    ,sum_func = function(counts, p) counts * p[1]
  )
)

#' A data frame with the severity distribution parameter placeholders
#'
#' One row per parameter of the severity distribution with the most parameters, giving
#' the number and output id of the placeholder in which the simulator app renders that
#' parameter's input for the chosen distribution.
#'
#' @return The severity distribution parameter placeholders.
#' @keywords internal
sev_dist_parameter_placeholders <- data.frame(
  param_number = seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1))))
  ,param_id = paste0("sev_param_", seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1)))))
)

#' Problems with a distribution's parameter values
#'
#' Checks that each parameter is a number, a whole number where required, and within its
#' allowed range.
#'
#' @param object A distribution object.
#' @param values A numeric vector (or list) of parameters in \code{paramIDs} order.
#' @return A character vector of problems, each starting with "parameter '<label>'";
#' empty when the values are valid.
#' @noRd
distribution_param_problems <- function(object, values) {
  pick <- function(x, i, default) if (i <= length(x)) x[[i]] else default
  labels <- object@param_labels
  problems <- character(0)
  for (i in seq_along(labels)) {
    value <- pick(values, i, NULL)
    label <- paste0("parameter '", labels[i], "'")
    if (!(is.numeric(value) && length(value) == 1 && !is.na(value))) {
      problems <- c(problems, label)
      next
    }
    if (!is.finite(value)) {
      #an infinite parameter makes the draws fail or gives infinite or NaN totals
      problems <- c(problems, paste(label, "must be finite"))
      next
    }
    if (isTRUE(pick(object@param_whole_numbers, i, FALSE)) && value != round(value)) {
      #claim counts and Binomial n cannot be fractional
      problems <- c(problems, paste(label, "must be a whole number"))
    }
    low <- pick(object@param_min_values, i, NA)
    low_strict <- isTRUE(pick(object@param_min_strict, i, FALSE))
    if (!is.na(low) && (value < low || (low_strict && value == low))) {
      problems <- c(problems, paste(label, if (low_strict) "must be greater than" else "must be at least", format(low)))
    }
    high <- pick(object@param_max_values, i, NA)
    high_strict <- isTRUE(pick(object@param_max_strict, i, FALSE))
    if (!is.na(high) && (value > high || (high_strict && value == high))) {
      problems <- c(problems, paste(label, if (high_strict) "must be less than" else "must be at most", format(high)))
    }
  }
  problems
}

#' Mean and standard deviation implied by a distribution's parameters
#'
#' @param options \code{freq_dist_options} or \code{sev_dist_options}.
#' @param id The distribution's \code{distrID}.
#' @param params A numeric vector (or list) of parameters in \code{paramIDs} order.
#' @param truncate_at_zero If TRUE and the distribution is the Normal, the moments of the
#' Normal truncated at zero are returned. Ignored for other distributions.
#' @return A named numeric vector \code{c(mean = , sd = )}. Both are NA when the parameters
#' are missing or invalid, and Inf when the moment does not exist (e.g. Pareto alpha <= 1).
#' @noRd
distribution_moments <- function(options, id, params, truncate_at_zero = FALSE) {
  unavailable <- c(mean = NA_real_, sd = NA_real_)
  if (!(is.character(id) && length(id) == 1 && id %in% names(options))) return(unavailable)
  object <- options[[id]]
  n_params <- length(object@paramIDs)

  #inputs that have not been filled in arrive as NULL entries in a list
  as_number <- function(v) if (is.numeric(v) && length(v) == 1) as.numeric(v) else NA_real_
  p <- if (is.list(params)) vapply(params, as_number, numeric(1)) else suppressWarnings(as.numeric(params))
  if (length(p) < n_params) return(unavailable)
  p <- p[seq_len(n_params)]
  if (length(distribution_param_problems(object, p)) > 0) return(unavailable)

  moments <- as.numeric(object@moments_func(p, truncate_at_zero))
  moments[is.nan(moments)] <- NA_real_
  c(mean = moments[1], sd = moments[2])
}

#' Mean and standard deviation of the Normal distribution truncated at zero
#'
#' @param mu Mean of the underlying Normal distribution.
#' @param sigma Standard deviation of the underlying Normal distribution.
#' @return A numeric vector of length two: the mean and the standard deviation. NA when
#' (almost) no probability mass lies above zero.
#' @noRd
truncated_normal_moments <- function(mu, sigma) {
  if (sigma == 0) return(if (mu > 0) c(mu, 0) else c(NA_real_, NA_real_))
  alpha <- -mu / sigma
  tail <- stats::pnorm(alpha, lower.tail = FALSE)
  if (tail < 1e-9) return(c(NA_real_, NA_real_))
  lambda <- stats::dnorm(alpha) / tail
  variance <- sigma^2 * (1 + alpha * lambda - lambda^2)
  c(mu + sigma * lambda, sqrt(max(variance, 0)))
}

#' Mean and standard deviation of the Pareto distribution
#'
#' @param alpha The shape parameter.
#' @param x_m The minimum value.
#' @return A numeric vector of length two: the mean (Inf when alpha <= 1) and the
#' standard deviation (Inf when alpha <= 2). NA when the parameters are not positive.
#' @noRd
pareto_moments <- function(alpha, x_m) {
  if (alpha <= 0 || x_m <= 0) return(c(NA_real_, NA_real_))
  mean_value <- if (alpha > 1) alpha * x_m / (alpha - 1) else Inf
  sd_value <- if (alpha > 2) x_m / (alpha - 1) * sqrt(alpha / (alpha - 2)) else Inf
  c(mean_value, sd_value)
}

#' The severity distribution used for a run
#'
#' Wraps a severity distribution object and its parameters, applying the truncation of
#' the Normal at zero when asked, into three functions: \code{draw(n)}, \code{survival(x)}
#' (P(X > x)) and \code{tail_quantile(s)} (the x with P(X > x) = s).
#'
#' @param object A severity distribution object.
#' @param params Numeric parameters in \code{paramIDs} order.
#' @param truncate_at_zero TRUE to truncate the Normal at zero.
#' @return A list with the functions \code{draw}, \code{survival} and \code{tail_quantile}.
#' @noRd
severity_model <- function(object, params, truncate_at_zero = FALSE) {
  if (isTRUE(truncate_at_zero) && identical(object@distrID, "Normal")) {
    mu <- params[1]
    sd <- params[2]
    above_zero <- stats::pnorm(0, mean = mu, sd = sd, lower.tail = FALSE)
    return(list(
      draw = function(n) rnorm_truncated_at_zero(n, mean = mu, sd = sd)
      ,survival = function(x) pmin(1, stats::pnorm(pmax(x, 0), mean = mu, sd = sd, lower.tail = FALSE) / above_zero)
      ,tail_quantile = function(s) stats::qnorm(s * above_zero, mean = mu, sd = sd, lower.tail = FALSE)
    ))
  }
  list(
    draw = function(n) object@simulate_func(n, params)
    ,survival = function(x) object@survival_func(x, params)
    ,tail_quantile = function(s) object@tail_quantile_func(s, params)
  )
}

#' Splice piecewise Pareto tails onto a severity distribution
#'
#' Above the first threshold the severity follows a Pareto with the first alpha, above the
#' second threshold a Pareto with the second alpha, and so on. This is the distribution the
#' simulator has always produced by redrawing claims above each threshold in turn; here the
#' claims above the first threshold are drawn from it in one step.
#'
#' @param model The base severity model from \code{severity_model()}.
#' @param thresholds Increasing slice thresholds (x_m values).
#' @param alphas The slice alphas.
#' @return A list with \code{first_threshold}, \code{draw_above_first(n)} (draws from the
#' spliced distribution conditional on exceeding the first threshold), and
#' \code{survival(x)} and \code{tail_quantile(s)} of the spliced distribution.
#' @noRd
pareto_splice <- function(model, thresholds, alphas) {
  k <- length(thresholds)
  #probability of exceeding each threshold
  survival_at <- numeric(k)
  survival_at[1] <- model$survival(thresholds[1])
  if (k > 1) {
    for (j in 2:k) survival_at[j] <- survival_at[j - 1] * (thresholds[j - 1] / thresholds[j])^alphas[j - 1]
  }

  #upper-tail quantile for probabilities at or below the first threshold's exceedance
  pareto_part_quantile <- function(s) {
    j <- k - findInterval(s, rev(survival_at), left.open = TRUE)
    thresholds[j] * (survival_at[j] / s)^(1 / alphas[j])
  }

  list(
    first_threshold = thresholds[1]
    ,draw_above_first = function(n) pareto_part_quantile(stats::runif(n, min = 0, max = survival_at[1]))
    ,survival = function(x) {
      out <- model$survival(x)
      j <- findInterval(x, thresholds, left.open = TRUE)
      above <- j > 0
      out[above] <- survival_at[j[above]] * (thresholds[j[above]] / x[above])^alphas[j[above]]
      out
    }
    ,tail_quantile = function(s) {
      out <- numeric(length(s))
      in_base <- s >= survival_at[1]
      out[in_base] <- model$tail_quantile(s[in_base])
      out[!in_base] <- pareto_part_quantile(s[!in_base])
      out
    }
  )
}

#' Find missing or invalid simulation settings
#'
#' Checks every setting that the chosen options require: that it is a number, and that
#' it lies in its allowed range. Distribution parameters, slice alphas, counts (the number
#' of simulations, the chunk size, the number of reinstatements) and the seed must be
#' finite; the severity cap, deductibles, limits and slice thresholds may be Inf.
#'
#' @param settings A named list of \code{simulate_function} arguments.
#' @return A character vector naming each setting that is missing or invalid.
#' An empty vector means the settings are complete.
#' @noRd
find_missing_simulation_settings <- function(settings) {
  s <- settings
  problems <- character(0)
  is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  nth <- function(x, i) if (i <= length(x)) x[[i]] else NULL
  #a number that must be present and at least (or, when strict, above) a minimum
  check_amount <- function(value, label, minimum = 0, strict = FALSE, whole = FALSE, maximum = NA, finite = FALSE) {
    if (!is_number(value)) {
      problems <<- c(problems, label)
      return(invisible(NULL))
    }
    if (finite && !is.finite(value)) {
      problems <<- c(problems, paste(label, "must be finite"))
      return(invisible(NULL))
    }
    if (whole && value != round(value)) problems <<- c(problems, paste(label, "must be a whole number"))
    if (value < minimum || (strict && value == minimum)) {
      problems <<- c(problems, paste(label, if (strict) "must be greater than" else "must be at least", format(minimum)))
    }
    if (!is.na(maximum) && value > maximum) problems <<- c(problems, paste(label, "must be at most", format(maximum)))
    invisible(NULL)
  }
  check_params <- function(values, distr, options, group) {
    if (!(is.character(distr) && length(distr) == 1 && distr %in% names(options))) {
      problems <<- c(problems, paste(group, "distribution"))
      return(invisible(NULL))
    }
    found <- distribution_param_problems(options[[distr]], values)
    if (length(found)) problems <<- c(problems, paste(group, found))
    invisible(NULL)
  }

  if (!is_number(s$numOfSimulations)) {
    problems <- c(problems, "Number of simulations")
  } else if (!is.finite(s$numOfSimulations) || s$numOfSimulations != round(s$numOfSimulations) ||
             s$numOfSimulations < 1 || s$numOfSimulations > max_number_of_simulations) {
    problems <- c(problems, paste(
      "Number of simulations must be a whole number between 1 and"
      ,format(max_number_of_simulations, big.mark = ",")
    ))
  }
  if (!is.null(s$chunk_size)) check_amount(s$chunk_size, "Chunk size", minimum = 1, whole = TRUE, finite = TRUE)
  check_params(s$freq_params, s$freqDistr, freq_dist_options, "Frequency")
  check_params(s$sev_params, s$sevDistr, sev_dist_options, "Severity")
  if (isTRUE(s$seedSetBinary)) {
    if (!is_number(s$seedValue)) {
      problems <- c(problems, "Seed value")
    } else if (!is.finite(s$seedValue) || s$seedValue != round(s$seedValue)) {
      problems <- c(problems, "Seed value must be a whole number")
    } else if (abs(s$seedValue) > .Machine$integer.max) {
      #set.seed() only takes integers
      largest <- format(.Machine$integer.max, big.mark = ",")
      problems <- c(problems, paste0("Seed value must be between -", largest, " and ", largest))
    }
  }

  #a Normal truncated at zero needs some probability mass above zero to sample from
  if (isTRUE(s$sevTruncateAtZero) && identical(s$sevDistr, "Normal")) {
    sev <- suppressWarnings(as.numeric(unlist(s$sev_params)))
    if (length(sev) == 2 && all(is.finite(sev)) && sev[2] >= 0 &&
        stats::pnorm(0, sev[1], sev[2], lower.tail = FALSE) < 1e-9) {
      problems <- c(problems, paste(
        "Severity Normal truncated at zero has almost no probability above zero",
        "(increase the mean or reduce the standard deviation)"
      ))
    }
  }

  if (isTRUE(s$paretoSlice)) {
    if (!is_number(s$pareto_slice_times)) {
      problems <- c(problems, "Number of Pareto Slices")
    } else if (s$pareto_slice_times != round(s$pareto_slice_times) ||
               s$pareto_slice_times < 1 || s$pareto_slice_times > max_number_of_pareto_slices) {
      problems <- c(problems, paste("Number of Pareto Slices must be a whole number from 1 to", max_number_of_pareto_slices))
    } else {
      for (j in seq_len(s$pareto_slice_times)) {
        check_amount(nth(s$slice_pareto_alphas, j), paste("Slice", j, "alpha"), minimum = 0, strict = TRUE, finite = TRUE)
        check_amount(nth(s$slice_pareto_x_ms, j), paste("Slice", j, "threshold (x_m)"), minimum = 0, strict = TRUE)
      }
      #each slice replaces the tail above its threshold, so thresholds must increase
      #(compared pairwise rather than with diff(), which gives NaN for two Inf thresholds)
      x_ms <- utils::head(suppressWarnings(as.numeric(unlist(s$slice_pareto_x_ms))), s$pareto_slice_times)
      if (length(x_ms) == s$pareto_slice_times && length(x_ms) > 1 &&
          !anyNA(x_ms) && !all(x_ms[-1] > x_ms[-length(x_ms)])) {
        problems <- c(problems, "Slice thresholds must increase from one slice to the next")
      }
    }
  }

  if (isTRUE(s$sevCapBinary)) check_amount(s$sev_cap_amount, "Severity Cap Amount")

  layers_with_deductible <- c('Unlimited Layer', 'Limited Layer', 'Exclude Layer')
  layers_with_limit <- c('Limited Layer', 'Exclude Layer')

  is_structure <- function(x) is.character(x) && length(x) == 1 && x %in% reinsurance_structures_options
  if (!is_structure(s$reinsuranceStructureEEL)) problems <- c(problems, "EEL reinsurance structure")
  if (!is_structure(s$reinsuranceStructureAL)) problems <- c(problems, "AL reinsurance structure")

  if (isTRUE(s$reinsuranceStructureEEL %in% layers_with_deductible)) {
    check_amount(s$reinsurance_structure_eel_dedctible_amount, "EEL Deductible Amount")
  }
  if (isTRUE(s$reinsuranceStructureEEL %in% layers_with_limit)) {
    check_amount(s$reinsurance_structure_eel_limit_amount, "EEL Limit Amount", strict = TRUE)
  }
  if (isTRUE(s$reinsuranceStructureEEL == 'Limited Layer') &&
      isTRUE(s$reinsuranceStructureLimitedReinstatements)) {
    check_amount(s$reinsuranceStructureReinstatementLimit, "Number of Reinstatements", whole = TRUE, finite = TRUE)
  }

  if (isTRUE(s$reinsuranceStructureAL %in% layers_with_deductible)) {
    check_amount(s$reinsurance_structure_al_dedctible_amount, "AL Deductible Amount")
  }
  if (isTRUE(s$reinsuranceStructureAL %in% layers_with_limit)) {
    check_amount(s$reinsurance_structure_al_limit_amount, "AL Limit Amount", strict = TRUE)
  }

  problems
}

#' Simulate insurance claims with reinsurance structures
#'
#' A function to simulate frequency - severity of insurance claims using chunked vectorisation.
#' The function applies severity cap, reinsurance structure for each and every loss claim,
#' reinsurance structure for aggregate claims, and allows for piecewise Pareto slices
#'
#' Order of the calculations, for each simulation (a period, e.g. a year): claims are drawn
#' from the severity distribution (with its Pareto slices) and capped at the severity cap;
#' the each-and-every-loss (EEL) structure applies to each claim; the results are summed
#' over the period; then the aggregate deductible comes off that sum, and finally the
#' aggregate limit and the reinstatement capacity cap what is left. In short: cap -> EEL
#' layer per claim -> annual sum -> aggregate deductible -> aggregate limit and
#' reinstatement capacity.
#'
#' The reinstatement capacity applies to a 'Limited Layer' EEL structure with limited
#' reinstatements, which pays at most \code{(reinstatements + 1) * limit} in a period. With
#' an aggregate 'Unlimited Layer' or 'Limited Layer', the ceded total is
#' \code{min(max(S - aggregate deductible, 0), aggregate limit, (reinstatements + 1) * limit)},
#' where S is the sum of the period's EEL recoveries before any capacity (the market
#' convention for an annual aggregate deductible). For example, three claims of 100 through
#' a layer of 100 excess of 0 with no reinstatements and an aggregate deductible of 50 cede
#' \code{min(300 - 50, 100) = 100}. Without an aggregate structure, the capacity caps S.
#' With an aggregate 'Exclude Layer', the capacity caps S first and the aggregate layer is
#' then taken out of the capped amount. An unlimited EEL layer, or limited reinstatements
#' switched off, has no capacity cap. (Before version 0.2.0 the capacity was applied before
#' the aggregate deductible, which gave smaller ceded totals when the two were combined.)
#'
#' Totals are returned at full precision; round them only for display.
#'
#' Random numbers: each chunk of simulations uses its own L'Ecuyer-CMRG random stream,
#' derived from one seed, so a run gives the same results whether or not it runs in
#' parallel. With \code{seedSetBinary = TRUE} (the default when a \code{seedValue} is given)
#' the run is reproducible from \code{seedValue} and the caller's random number stream is
#' left unchanged; otherwise the seed is drawn from the caller's stream, so
#' \code{set.seed()} before the call also makes it reproducible.
#' The streams always use Inversion for normal draws and Rejection sampling, so a seed
#' gives the same results whatever the caller's \code{RNGkind()}, which is restored afterwards.
#' Results depend on the chunk size, which by default adapts to the expected number of
#' claims per simulation.
#'
#' @param numOfSimulations The number of simulations to run.
#' @param freq_params A vector of the frequency distribution parameters.
#' @param sev_params A vector of the severity distribution parameters.
#' @param seedSetBinary True if there is a fixed seed (\code{seedValue}), otherwise false. Defaults to TRUE when a \code{seedValue} is given and FALSE otherwise, so a \code{seedValue} on its own makes the run reproducible; an explicit FALSE ignores \code{seedValue}.
#' @param seedValue The seed value, a whole number between \code{-.Machine$integer.max} and \code{.Machine$integer.max}, or NULL (the default) for no fixed seed.
#' @param freqDistr The frequency distribution: \code{"Poisson"}, \code{"Negative_Binomial"}, \code{"Binomial"} or \code{"Fixed_number_of_Counts"}. The parameters of each are listed in \code{\link{simulate_claims}}.
#' @param sevDistr The severity distribution: \code{"Normal"}, \code{"LogNormal"}, \code{"Gamma"}, \code{"Exponential"}, \code{"Pareto"} or \code{"Fixed_Severity"}. The parameters of each are listed in \code{\link{simulate_claims}}.
#' @param paretoSlice True if there is Pareto slicing.
#' @param pareto_slice_times The number of Pareto slices.
#' @param slice_pareto_alphas A vector of Pareto slices' alpha parameters.
#' @param slice_pareto_x_ms A vector of Pareto slices' x_m parameters.
#' @param sevCapBinary True if there is a severity cap.
#' @param sev_cap_amount The severity cap amount.
#' @param reinsuranceStructureEEL The chosen reinsurance structure for each and every loss claim.
#' @param reinsurance_structure_eel_dedctible_amount The deductible for each and every loss reinsurance structure.
#' @param reinsurance_structure_eel_limit_amount The limit for each and every loss reinsurance structure.
#' @param reinsuranceStructureAL The chosen reinsurance structure for aggregate claims.
#' @param reinsurance_structure_al_dedctible_amount The deductible for aggregate reinsurance structure.
#' @param reinsurance_structure_al_limit_amount The limit for aggregate reinsurance structure.
#' @param reinsuranceStructureLimitedReinstatements True if there is a limit in reinstatements, otherwise false.
#' @param reinsuranceStructureReinstatementLimit The reinstatement limit.
#' @param multiprocessing True to run the chunks in parallel with the future package, otherwise false. A future plan with more than one worker that the caller has already set is reused and left running. Otherwise the call starts a multisession plan with one worker per available core (\code{parallelly::availableCores()}), shuts those workers down when it finishes and restores the caller's plan, so every such call pays the start-up cost again. To choose the number of workers and reuse them across calls, set a plan first, e.g. \code{future::plan(future::multisession, workers = 4)}.
#' @param sevTruncateAtZero True to draw Normal severities from the Normal distribution truncated at zero, so that no claim is negative. Ignored for other severity distributions. Defaults to FALSE.
#' @param chunk_size The number of simulations processed per vectorised batch. By default (NULL) it is chosen from the expected number of claims per simulation, so that a batch holds about a million claims (between 100 and 10,000 simulations). Because of the floor of 100 simulations, a batch holds more than a million claims when the mean frequency exceeds 10,000 claims per simulation (about 100 million at a mean of a million), and memory use grows with it; give a smaller \code{chunk_size} to keep batches small. Results with a fixed seed depend on the chunk size.
#' @param gross True (the default) to return the gross total claims before reinsurance. Set it to FALSE when only the totals after the structures are needed: with an each-and-every-loss layer this allows drawing only the claims that reach the layer, which is much faster.
#' @param shortcuts True (the default) to use exact shortcuts where the settings allow: when no layer, cap, Pareto slice or truncation acts on individual claims, each simulation's total is drawn in one step for the Normal, Gamma, Exponential and fixed severities; with \code{gross = FALSE} and a layer, only the claims above the deductible are drawn. The results follow the same distribution as without shortcuts. Set it to FALSE to simulate every claim.
#' @param progress An optional function called after each chunk of a sequential run with the fraction done and a short description, e.g. to update a progress bar.
#' @return A data frame with one row per simulation, at full precision: \code{claim_counts},
#' the claim count; \code{total_claims}, the total claims after the reinsurance structures;
#' \code{gross_claims}, the gross total claims before them (after Pareto slices and the
#' severity cap; unless \code{gross = FALSE}); and, when reinstatements are limited,
#' \code{number_of_reinstatements_used}: the EEL layer's recoveries in the period divided by
#' the EEL limit, capped at the number of reinstatements, so reinstatements are counted pro
#' rata to the amount recovered. The recoveries are taken after the aggregate deductible and
#' limit of an aggregate 'Unlimited Layer' or 'Limited Layer', but before an aggregate
#' 'Exclude Layer' is taken out: with an exclusion they are the EEL recoveries after the
#' reinstatement capacity. For example, three claims of 100 through a layer of 60 excess of
#' 30 with two reinstatements and an aggregate exclusion of 150 excess of 50 give a total of
#' 50 but 2 reinstatements used (180 / 60, capped at 2).
#' Stops with an error that names any required setting that is missing or invalid.
#' @seealso \code{\link{simulate_claims}}, a simpler interface with short argument names,
#'   and \code{\link{run_shiny_simulator}} for the same model in an app.
#' @export
#' @examples
#' # 1,000 simulated years of Poisson claim counts with Normal claim sizes, no reinsurance
#' results <- simulate_function(
#'   numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
#'   seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal"
#' )
#' summary(results$total_claims)
#'
#' # the same claims ceded to a layer of 1,500 excess of 800 on each claim
#' layer <- simulate_function(
#'   numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
#'   seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal",
#'   reinsuranceStructureEEL = "Limited Layer",
#'   reinsurance_structure_eel_dedctible_amount = 800,
#'   reinsurance_structure_eel_limit_amount = 1500
#' )
#' mean(layer$total_claims)
simulate_function <- function(
    numOfSimulations,
    freq_params,
    sev_params,
    seedSetBinary = !is.null(seedValue),
    seedValue = NULL,
    freqDistr,
    sevDistr,
    paretoSlice = FALSE,
    pareto_slice_times = NULL,
    slice_pareto_alphas = NULL,
    slice_pareto_x_ms = NULL,
    sevCapBinary = FALSE,
    sev_cap_amount = NULL,
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsurance_structure_eel_dedctible_amount = NULL,
    reinsurance_structure_eel_limit_amount = NULL,
    reinsuranceStructureAL = "No Reinsurance Structure",
    reinsurance_structure_al_dedctible_amount = NULL,
    reinsurance_structure_al_limit_amount = NULL,
    reinsuranceStructureLimitedReinstatements = FALSE,
    reinsuranceStructureReinstatementLimit = NULL,
    multiprocessing = FALSE,
    sevTruncateAtZero = FALSE,
    chunk_size = NULL,
    gross = TRUE,
    shortcuts = TRUE,
    progress = NULL
){
  #collect the settings; an omitted argument without a default is treated as not set (NULL)
  arg_env <- environment()
  arg_defaults <- formals(sys.function())
  has_no_default <- vapply(arg_defaults, function(d) is.symbol(d) && as.character(d) == "", logical(1))
  settings <- list()
  for (arg in names(arg_defaults)) {
    if (has_no_default[[arg]] && do.call(missing, list(as.name(arg)), envir = arg_env)) {
      assign(arg, NULL, envir = arg_env)
    }
    settings[arg] <- list(get(arg, envir = arg_env))
  }

  #stop early with a clear message if a required setting is missing or invalid
  missing_settings <- find_missing_simulation_settings(settings)
  if (length(missing_settings) > 0) {
    stop("Missing or invalid settings: ", paste(missing_settings, collapse = ", "), call. = FALSE)
  }

  #parameters may be supplied as lists; use plain numeric vectors from here on
  freq_params <- as.numeric(unlist(freq_params))
  sev_params <- as.numeric(unlist(sev_params))
  freq_object <- freq_dist_options[[freqDistr]]
  sev_object <- sev_dist_options[[sevDistr]]

  #the severity for this run: the chosen distribution, truncated and sliced as asked
  truncate <- isTRUE(sevTruncateAtZero) && identical(sevDistr, "Normal")
  model <- severity_model(sev_object, sev_params, truncate)
  slices <- NULL
  if (isTRUE(paretoSlice)) {
    k <- pareto_slice_times
    slices <- pareto_splice(
      model,
      thresholds = as.numeric(unlist(slice_pareto_x_ms))[seq_len(k)],
      alphas = as.numeric(unlist(slice_pareto_alphas))[seq_len(k)]
    )
  }
  cap <- if (isTRUE(sevCapBinary)) sev_cap_amount else Inf

  #exact shortcuts: one draw per simulation when nothing acts on individual claims, or
  #only the claims above the deductible when the gross totals are not needed
  sum_in_one_step <- isTRUE(shortcuts) && is.null(slices) && !isTRUE(sevCapBinary) && !truncate &&
    identical(reinsuranceStructureEEL, "No Reinsurance Structure") && !is.null(sev_object@sum_func)
  draw_large_claims_only <- isTRUE(shortcuts) && !isTRUE(gross) &&
    reinsuranceStructureEEL %in% c("Unlimited Layer", "Limited Layer") && !is.null(freq_object@split_func)
  if (draw_large_claims_only) {
    eel_deductible <- reinsurance_structure_eel_dedctible_amount
    final_survival <- if (is.null(slices)) model$survival else slices$survival
    final_tail_quantile <- if (is.null(slices)) model$tail_quantile else slices$tail_quantile
    #a claim cedes something only above the deductible; a cap at or below it cedes nothing
    large_prob <- if (cap <= eel_deductible) 0 else final_survival(eel_deductible)
  }

  #chunks: by default about a million claims each, to keep memory use flat
  if (is.null(chunk_size)) {
    expected_claims <- distribution_moments(freq_dist_options, freqDistr, freq_params)[["mean"]]
    chunk_size <- if (is.finite(expected_claims) && expected_claims > 0) {
      min(10000, max(100, floor(1e6 / expected_claims)))
    } else {
      10000
    }
  }
  n_chunks <- ceiling(numOfSimulations / chunk_size)
  chunk_sizes <- rep(chunk_size, n_chunks)
  chunk_sizes[n_chunks] <- numOfSimulations - chunk_size * (n_chunks - 1)

  #random numbers: one L'Ecuyer-CMRG stream per chunk, derived from a single seed, so that
  #sequential and parallel runs agree; the caller's generator is restored afterwards
  global_env <- globalenv()
  if (isTRUE(seedSetBinary)) {
    saved_seed <- if (exists(".Random.seed", envir = global_env, inherits = FALSE)) get(".Random.seed", envir = global_env) else NULL
    base_seed <- seedValue
  } else {
    #draw the seed from the caller's stream, so set.seed() before the call makes the run reproducible
    base_seed <- sample.int(.Machine$integer.max, 1L)
    saved_seed <- get(".Random.seed", envir = global_env)
  }
  saved_kind <- RNGkind()
  on.exit({
    #restoring a "Rounding" sample kind warns; the caller chose it, so the warning is not ours
    suppressWarnings(RNGkind(saved_kind[1], saved_kind[2], saved_kind[3]))
    if (is.null(saved_seed)) {
      if (exists(".Random.seed", envir = global_env, inherits = FALSE)) rm(".Random.seed", envir = global_env)
    } else {
      assign(".Random.seed", saved_seed, envir = global_env)
    }
  }, add = TRUE)
  #the normal and sample kinds are pinned too (the streams carry them), so that a seed gives
  #the same results whatever the caller's RNGkind()
  set.seed(base_seed, kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  chunk_seeds <- vector("list", n_chunks)
  stream <- get(".Random.seed", envir = global_env)
  for (i in seq_len(n_chunks)) {
    chunk_seeds[[i]] <- stream
    stream <- parallel::nextRNGStream(stream)
  }

  #simulations for one chunk
  simulate_chunk <- function(this_n){

    if (draw_large_claims_only) {
      #split the claim counts into large claims (above the deductible) and the rest,
      #and draw only the large claims, from the tail of the severity
      counts <- freq_object@split_func(this_n, freq_params, large_prob)
      totals <- numeric(this_n)
      n_large <- sum(counts$large)
      if (n_large > 0) {
        large_claims <- pmin(final_tail_quantile(stats::runif(n_large, min = 0, max = large_prob)), cap)
        ceded <- apply_deductible_limit(
          large_claims
          ,reinsurance_structure = reinsuranceStructureEEL
          ,deductible = eel_deductible
          ,limit = reinsurance_structure_eel_limit_amount
        )
        sums <- rowsum(ceded, rep.int(seq_len(this_n), counts$large))
        totals[as.integer(rownames(sums))] <- sums[, 1]
      }
      return(list(claim_counts = counts$total, total_claims = totals, gross_claims = NULL))
    }

    #simulate claim counts for this chunk
    counts <- freq_object@simulate_func(this_n, freq_params)

    if (sum_in_one_step) {
      totals <- sev_object@sum_func(counts, sev_params)
      return(list(claim_counts = counts, total_claims = totals, gross_claims = totals))
    }

    total_claims_needed <- sum(counts)

    #edge case: chunk has zero claims across all simulations
    if(total_claims_needed == 0){
      return(list(claim_counts = counts, total_claims = rep(0, this_n), gross_claims = rep(0, this_n)))
    }

    sim_id <- rep.int(seq_len(this_n), counts)

    #simulate all individual severities for the chunk in one vectorised call
    claims <- model$draw(total_claims_needed)

    #pareto slices: claims above the first threshold are redrawn from the spliced tail
    if (!is.null(slices)) {
      above <- claims > slices$first_threshold
      if (any(above)) claims[above] <- slices$draw_above_first(sum(above))
    }

    #apply severity cap
    claims <- apply_severity_cap(
      claims
      ,severity_cap_boolean = isTRUE(sevCapBinary)
      ,severity_cap_amount = sev_cap_amount
    )

    #keep the gross claims (after tail adjustments and the cap, before any reinsurance)
    gross_claims <- claims

    #apply EEL deductible/limit per individual claim
    claims <- apply_deductible_limit(
      claims
      ,reinsurance_structure = reinsuranceStructureEEL
      ,deductible = reinsurance_structure_eel_dedctible_amount
      ,limit = reinsurance_structure_eel_limit_amount
    )

    #sum individual claims back to simulation-level totals, gross and after the EEL structure;
    #simulations with no claims keep a total of zero
    sums <- rowsum(cbind(gross = gross_claims, total = claims), sim_id)
    ids <- as.integer(rownames(sums))
    totals <- numeric(this_n)
    gross_totals <- numeric(this_n)
    totals[ids] <- sums[, "total"]
    gross_totals[ids] <- sums[, "gross"]

    return(list(claim_counts = counts, total_claims = totals, gross_claims = gross_totals))
  }
  run_chunk <- function(i) simulate_chunk(chunk_sizes[i])

  #run chunks, optionally in parallel across chunks (not per-simulation)
  if (isTRUE(multiprocessing)) {
    #reuse workers that are already running (e.g. the app keeps a warm multisession plan);
    #otherwise start a plan for this call and restore the caller's plan afterwards
    #keep objects the workers do not need (e.g. a Shiny progress callback) out of the chunk closure
    progress <- NULL
    settings <- NULL
    if (future::nbrOfWorkers() <= 1) {
      old_plan <- future::plan(future::multisession)
      on.exit(future::plan(old_plan), add = TRUE)
    }
    chunk_results <- run_chunks_in_futures(run_chunk, chunk_seeds)
  } else {
    chunk_results <- vector("list", n_chunks)
    for (i in seq_len(n_chunks)) {
      assign(".Random.seed", chunk_seeds[[i]], envir = global_env)
      chunk_results[[i]] <- run_chunk(i)
      if (is.function(progress)) progress(i / n_chunks, paste("Chunk", i, "of", n_chunks))
    }
  }

  data <- data.frame(
    claim_counts = unlist(lapply(chunk_results, `[[`, "claim_counts"), use.names = FALSE)
    ,total_claims = unlist(lapply(chunk_results, `[[`, "total_claims"), use.names = FALSE)
  )
  if (isTRUE(gross)) {
    data$gross_claims <- unlist(lapply(chunk_results, `[[`, "gross_claims"), use.names = FALSE)
  }
  rm(chunk_results)

  #the aggregate step, on each simulation's total after the EEL structure (see Details);
  #a limited EEL layer with limited reinstatements pays at most (reinstatements + 1) limits
  reinstatements_limited <- identical(reinsuranceStructureEEL, 'Limited Layer') &&
    isTRUE(reinsuranceStructureLimitedReinstatements)
  capacity <- if (reinstatements_limited) {
    (reinsuranceStructureReinstatementLimit + 1) * reinsurance_structure_eel_limit_amount
  } else {
    Inf
  }
  apply_al <- function(x) apply_deductible_limit(
    x
    ,reinsuranceStructureAL
    ,reinsurance_structure_al_dedctible_amount
    ,reinsurance_structure_al_limit_amount
  )
  if (reinsuranceStructureAL %in% c('Unlimited Layer', 'Limited Layer')) {
    #aggregate layer: its deductible comes off the period's EEL recoveries first, then the
    #aggregate limit and the reinstatement capacity cap what is left
    data$total_claims <- pmin(apply_al(data$total_claims), capacity)
    layer_recoveries <- data$total_claims
  } else {
    #no aggregate layer, or an aggregate exclusion taken out of the capped recoveries; the
    #reinstatements used are counted on the recoveries before the exclusion
    data$total_claims <- pmin(data$total_claims, capacity)
    layer_recoveries <- data$total_claims
    data$total_claims <- apply_al(data$total_claims)
  }
  if (reinstatements_limited) {
    #reinstatements are used pro rata to the amount the layer recovers
    data$number_of_reinstatements_used <- pmin(
      layer_recoveries / reinsurance_structure_eel_limit_amount
      ,reinsuranceStructureReinstatementLimit
    )
  }
  return(data)
}

#' Run the chunks of a simulation on the workers of the current future plan
#'
#' Splits the chunks into one contiguous group per worker and runs each group in one
#' future, so the setup cost is paid once per worker rather than once per chunk. Each
#' chunk starts from its own random stream, set in the worker just before it runs, so the
#' results are identical to a sequential run. An error in a chunk is raised again here
#' with its message.
#' @param run_chunk Function of the chunk index that returns the chunk's results.
#' @param chunk_seeds List of L'Ecuyer-CMRG seeds (\code{.Random.seed} values), one per chunk.
#' @return List of the chunk results, in chunk order.
#' @noRd
run_chunks_in_futures <- function(run_chunk, chunk_seeds) {
  n_chunks <- length(chunk_seeds)
  #min() also handles backends that report an infinite number of workers
  groups <- parallel::splitIndices(n_chunks, min(n_chunks, future::nbrOfWorkers()))
  futures <- lapply(groups, function(chunks) {
    seeds <- chunk_seeds[chunks]
    #the future's own seed is the first chunk's stream; giving one tells future that the
    #expression uses random numbers, and draws nothing from the caller's stream
    future::future({
      lapply(seq_along(chunks), function(k) {
        assign(".Random.seed", seeds[[k]], envir = globalenv())
        run_chunk(chunks[[k]])
      })
    }, seed = seeds[[1]])
  })
  #wait for every group before collecting the values: value() stops at the first error, and a
  #group still running would leave its result unread on its worker; future then finds that
  #worker broken and relaunches it, and the relaunched worker's connection is not closed when
  #the plan is shut down (the garbage collector closes it later, with a warning)
  future::resolve(futures)
  unlist(future::value(futures), recursive = FALSE, use.names = FALSE)
}

#' A function to run the shiny simulator application
#'
#' Opens the claims simulator, a Shiny app for running the frequency-severity model of
#' \code{\link{simulate_function}} from a form, with charts, a report and saved settings,
#' without writing any code.
#'
#' @return A shiny app object (class \code{shiny.appobj}). Printing it, as happens when
#'   \code{run_shiny_simulator()} is called at the console, opens the app; pass it to
#'   \code{shiny::runApp()} to choose options such as the port.
#' @seealso \code{\link{simulate_claims}} and \code{\link{simulate_function}}, which run
#'   the same model from R code.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_simulator()
#' }
run_shiny_simulator <- function() {
  shinyApp(ui = shiny_simulator_ui, server = shiny_simulator_server)
}
