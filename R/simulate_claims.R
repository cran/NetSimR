#' Simulate claims with a frequency-severity model
#'
#' A simpler interface to \code{\link{simulate_function}}, with short argument names and
#' defaults for everything optional. Leaving an option out (NULL) switches the feature off:
#' no seed, no Pareto slices, no cap, no reinstatement limit.
#'
#' Distributions are chosen by name; case, spaces and underscores are ignored, so
#' "Negative Binomial", "negative_binomial" and "Negative_Binomial" are the same. Their
#' parameters are given in the order below, or named with these names:
#' \tabular{lll}{
#'   \strong{Distribution} \tab \strong{Type} \tab \strong{Parameters} \cr
#'   Poisson \tab frequency \tab lambda \cr
#'   Negative_Binomial \tab frequency \tab r, beta (Gamma shape and scale of the Poisson mean) \cr
#'   Binomial \tab frequency \tab n, p \cr
#'   Fixed_number_of_Counts \tab frequency \tab count \cr
#'   Normal \tab severity \tab mean, sd \cr
#'   LogNormal \tab severity \tab meanlog, sdlog \cr
#'   Gamma \tab severity \tab shape, scale \cr
#'   Exponential \tab severity \tab rate \cr
#'   Pareto \tab severity \tab alpha, x_m (minimum) \cr
#'   Fixed_Severity \tab severity \tab amount
#' }
#'
#' Layers are "none", "unlimited" (everything above the deductible), "limited" (the
#' limit excess of the deductible) or "exclude" (the claims with that layer removed).
#' The each-and-every-loss layer applies to every claim; the aggregate layer applies to
#' each simulation's total after the each-and-every-loss layer. The order is: severity cap
#' -> each-and-every-loss layer per claim -> sum over the simulation -> aggregate
#' deductible -> aggregate limit and reinstatement capacity. So with a "limited"
#' each-and-every-loss layer and an "unlimited" or "limited" aggregate layer, the ceded
#' total is \code{min(max(S - agg_deductible, 0), agg_limit, (eel_reinstatements + 1) * eel_limit)},
#' where S is the sum of the layer's recoveries; three claims of 100 through a layer of
#' 100 excess of 0 with no reinstatements and an aggregate deductible of 50 cede 100. An
#' "exclude" aggregate layer is taken out of the recoveries after the reinstatement
#' capacity has capped them. See \code{\link{simulate_function}} for the details.
#'
#' @param n_sims Number of simulations (e.g. years).
#' @param frequency Name of the claim count distribution; see Details.
#' @param frequency_params Parameters of the claim count distribution, either all unnamed in the order of Details or all named.
#' @param severity Name of the claim size distribution; see Details.
#' @param severity_params Parameters of the claim size distribution, either all unnamed in the order of Details or all named.
#' @param seed A whole number for a reproducible run. NULL (the default) uses the current random number stream, so \code{set.seed()} before the call also makes the run reproducible.
#' @param truncate_at_zero TRUE to draw Normal claim sizes from the Normal distribution truncated at zero, so no claim is negative. Only used with the Normal severity.
#' @param pareto_thresholds Increasing claim sizes above which the severity tail is replaced by Pareto slices, one per slice (at most six). NULL (the default) for no slices.
#' @param pareto_alphas The Pareto alpha of each slice, one per threshold.
#' @param severity_cap The largest amount a single claim can reach, or NULL (the default) for no cap.
#' @param eel_layer The each-and-every-loss layer: "none" (the default), "unlimited", "limited" or "exclude".
#' @param eel_deductible The deductible of the each-and-every-loss layer.
#' @param eel_limit The limit of a "limited" or "exclude" each-and-every-loss layer.
#' @param eel_reinstatements The number of reinstatements of a "limited" each-and-every-loss layer, so it pays at most \code{(eel_reinstatements + 1) * eel_limit} per simulation. NULL (the default) for unlimited reinstatements.
#' @param agg_layer The aggregate layer: "none" (the default), "unlimited", "limited" or "exclude".
#' @param agg_deductible The deductible of the aggregate layer.
#' @param agg_limit The limit of a "limited" or "exclude" aggregate layer.
#' @param parallel TRUE to run the chunks of simulations on parallel workers. Results are the same as a sequential run.
#' @param chunk_size The number of simulations per vectorised batch; NULL (the default) chooses it from the expected number of claims, aiming at about a million claims per batch but never fewer than 100 simulations, so batches are larger when the mean frequency exceeds 10,000. Results with a fixed seed depend on it.
#' @param gross TRUE (the default) to return the gross totals before the layers. FALSE allows a much faster run with an "unlimited" or "limited" each-and-every-loss layer, by drawing only the claims that reach it.
#' @param shortcuts TRUE (the default) to use exact shortcuts where the settings allow; see \code{\link{simulate_function}}.
#' @param progress An optional function called after each chunk of a sequential run with the fraction done and a short description.
#' @return A data frame with one row per simulation, at full precision: \code{claim_counts}, \code{total_claims} (after the layers), \code{gross_claims} (before them, unless \code{gross = FALSE}) and, with limited reinstatements, \code{number_of_reinstatements_used}: the layer's recoveries in the simulation (after the aggregate deductible and limit, if any) divided by \code{eel_limit}, capped at \code{eel_reinstatements}.
#' @seealso \code{\link{simulate_function}}, which this calls, and \code{\link{run_shiny_simulator}} for the same model in an app.
#' @export
#' @examples
#' # 10,000 years of Poisson claim counts with Log-Normal claim sizes
#' claims <- simulate_claims(
#'   10000, frequency = "Poisson", frequency_params = 3,
#'   severity = "LogNormal", severity_params = c(meanlog = 8, sdlog = 1.5), seed = 1
#' )
#' summary(claims$total_claims)
#'
#' # a Pareto tail above 100,000, and a layer of 50,000 excess of 20,000 on each
#' # claim with two reinstatements
#' ceded <- simulate_claims(
#'   10000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
#'   pareto_thresholds = 100000, pareto_alphas = 1.5,
#'   eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
#'   eel_reinstatements = 2
#' )
#' mean(ceded$total_claims)
simulate_claims <- function(
    n_sims,
    frequency,
    frequency_params,
    severity,
    severity_params,
    seed = NULL,
    truncate_at_zero = FALSE,
    pareto_thresholds = NULL,
    pareto_alphas = NULL,
    severity_cap = NULL,
    eel_layer = "none",
    eel_deductible = NULL,
    eel_limit = NULL,
    eel_reinstatements = NULL,
    agg_layer = "none",
    agg_deductible = NULL,
    agg_limit = NULL,
    parallel = FALSE,
    chunk_size = NULL,
    gross = TRUE,
    shortcuts = TRUE,
    progress = NULL
) {
  claims_check_flag(truncate_at_zero, "truncate_at_zero")
  claims_check_flag(parallel, "parallel")
  claims_check_flag(gross, "gross")
  claims_check_flag(shortcuts, "shortcuts")

  frequency_id <- claims_distribution_id(frequency, freq_dist_options, "frequency")
  severity_id <- claims_distribution_id(severity, sev_dist_options, "severity")
  freq_params <- claims_distribution_params(frequency_params, freq_dist_options[[frequency_id]], "frequency_params")
  sev_params <- claims_distribution_params(severity_params, sev_dist_options[[severity_id]], "severity_params")

  eel <- claims_layer(eel_layer, "eel_layer")
  agg <- claims_layer(agg_layer, "agg_layer")
  claims_check_layer(eel, eel_deductible, eel_limit, "eel")
  claims_check_layer(agg, agg_deductible, agg_limit, "agg")
  if (!is.null(eel_reinstatements) && eel != "limited") {
    stop("eel_reinstatements applies only to a \"limited\" eel_layer.", call. = FALSE)
  }

  n_slices <- length(pareto_thresholds)
  if (length(pareto_alphas) != n_slices) {
    stop("pareto_alphas and pareto_thresholds must have the same length: one alpha per threshold.", call. = FALSE)
  }
  if (n_slices > max_number_of_pareto_slices) {
    stop("At most ", max_number_of_pareto_slices, " Pareto slices are allowed.", call. = FALSE)
  }
  if (isTRUE(truncate_at_zero) && severity_id != "Normal") {
    warning("truncate_at_zero is ignored: it applies only to the Normal severity.", call. = FALSE)
  }

  simulate_function(
    numOfSimulations = n_sims,
    freq_params = freq_params,
    sev_params = sev_params,
    seedSetBinary = !is.null(seed),
    seedValue = seed,
    freqDistr = frequency_id,
    sevDistr = severity_id,
    paretoSlice = n_slices > 0,
    pareto_slice_times = if (n_slices > 0) n_slices,
    slice_pareto_alphas = pareto_alphas,
    slice_pareto_x_ms = pareto_thresholds,
    sevCapBinary = !is.null(severity_cap),
    sev_cap_amount = severity_cap,
    reinsuranceStructureEEL = claims_layer_names[[eel]],
    reinsurance_structure_eel_dedctible_amount = eel_deductible,
    reinsurance_structure_eel_limit_amount = eel_limit,
    reinsuranceStructureAL = claims_layer_names[[agg]],
    reinsurance_structure_al_dedctible_amount = agg_deductible,
    reinsurance_structure_al_limit_amount = agg_limit,
    reinsuranceStructureLimitedReinstatements = !is.null(eel_reinstatements),
    reinsuranceStructureReinstatementLimit = eel_reinstatements,
    multiprocessing = isTRUE(parallel),
    sevTruncateAtZero = isTRUE(truncate_at_zero),
    chunk_size = chunk_size,
    gross = gross,
    shortcuts = shortcuts,
    progress = progress
  )
}

#' Short layer names of simulate_claims() and the structures they stand for
#'
#' @noRd
claims_layer_names <- c(
  none = "No Reinsurance Structure",
  unlimited = "Unlimited Layer",
  limited = "Limited Layer",
  exclude = "Exclude Layer"
)

#' Parameter names of each distribution in simulate_claims()
#'
#' @noRd
claims_param_names <- list(
  Poisson = "lambda",
  Negative_Binomial = c("r", "beta"),
  Binomial = c("n", "p"),
  Fixed_number_of_Counts = "count",
  Normal = c("mean", "sd"),
  LogNormal = c("meanlog", "sdlog"),
  Gamma = c("shape", "scale"),
  Exponential = "rate",
  Pareto = c("alpha", "x_m"),
  Fixed_Severity = "amount"
)

#' Find a distribution by name, ignoring case, spaces and underscores
#'
#' @param x The name given by the user.
#' @param options \code{freq_dist_options} or \code{sev_dist_options}.
#' @param what "frequency" or "severity", for the error message.
#' @return The distribution's id.
#' @noRd
claims_distribution_id <- function(x, options, what) {
  ids <- names(options)
  if (!(is.character(x) && length(x) == 1 && !is.na(x))) {
    stop(what, " must be one distribution name: ", paste0("\"", ids, "\"", collapse = ", "), call. = FALSE)
  }
  squash <- function(s) gsub("[^a-z0-9]", "", tolower(s))
  labels <- vapply(options, function(o) o@distr_label, character(1))
  hit <- ids[squash(x) == squash(ids) | squash(x) == squash(labels)]
  if (length(hit) != 1) {
    stop("Unknown ", what, " distribution \"", x, "\". Choose one of: ",
         paste0("\"", ids, "\"", collapse = ", "), call. = FALSE)
  }
  hit
}

#' Put a distribution's parameters in order
#'
#' Unnamed parameters are taken in order; named ones are matched to the names of
#' \code{claims_param_names} (or the app's parameter ids) and reordered.
#'
#' @param params The parameters given by the user.
#' @param object The distribution object.
#' @param arg The argument name, for the error message.
#' @return An unnamed numeric vector in the order the simulator expects.
#' @noRd
claims_distribution_params <- function(params, object, arg) {
  friendly <- claims_param_names[[object@distrID]]
  expected <- paste0(length(friendly), " number", if (length(friendly) > 1) "s", " for the ",
                     object@distr_label, " (", paste(friendly, collapse = ", "), ")")
  if (is.list(params)) params <- unlist(params)
  if (!is.numeric(params) || length(params) != length(friendly)) {
    stop(arg, " must be ", expected, ".", call. = FALSE)
  }
  given <- names(params)
  if (!is.null(given) && any(given != "")) {
    if (any(is.na(given) | given == "")) {
      stop("The parameters in ", arg, " must be all named or all unnamed: name every one (",
           paste(friendly, collapse = ", "), ") or none.", call. = FALSE)
    }
    position <- match(given, friendly)
    by_id <- match(given, object@paramIDs)
    position[is.na(position)] <- by_id[is.na(position)]
    if (anyNA(position) || anyDuplicated(position)) {
      stop(arg, " must be ", expected, "; names that do not match are not allowed.", call. = FALSE)
    }
    params <- params[order(position)]
  }
  unname(as.numeric(params))
}

#' Check that an option is a single TRUE or FALSE
#'
#' @param x The value given by the user.
#' @param arg The argument name, for the error message.
#' @noRd
claims_check_flag <- function(x, arg) {
  if (!(is.logical(x) && length(x) == 1 && !is.na(x))) {
    stop(arg, " must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}

#' Turn a layer name into its short name
#'
#' @param x "none", "unlimited", "limited" or "exclude" (any case), or a full structure name.
#' @param arg The argument name, for the error message.
#' @return The short layer name.
#' @noRd
claims_layer <- function(x, arg) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) {
    if (x %in% claims_layer_names) return(names(claims_layer_names)[claims_layer_names == x])
    if (tolower(x) %in% names(claims_layer_names)) return(tolower(x))
  }
  stop(arg, " must be one of \"none\", \"unlimited\", \"limited\" or \"exclude\".", call. = FALSE)
}

#' Check that a layer has the amounts it needs
#'
#' @param layer The short layer name.
#' @param deductible,limit The amounts given.
#' @param prefix "eel" or "agg", the start of the argument names.
#' @noRd
claims_check_layer <- function(layer, deductible, limit, prefix) {
  arg <- function(name) paste0(prefix, "_", name)
  if (layer != "none" && is.null(deductible)) {
    stop(arg("deductible"), " is needed for a \"", layer, "\" ", arg("layer"), ".", call. = FALSE)
  }
  if (layer %in% c("limited", "exclude") && is.null(limit)) {
    stop(arg("limit"), " is needed for a \"", layer, "\" ", arg("layer"), ".", call. = FALSE)
  }
  if (layer == "none" && (!is.null(deductible) || !is.null(limit))) {
    warning(arg("deductible"), " and ", arg("limit"), " are ignored because ", arg("layer"), " is \"none\".", call. = FALSE)
  }
  if (layer == "unlimited" && !is.null(limit)) {
    warning(arg("limit"), " is ignored for an \"unlimited\" ", arg("layer"), ".", call. = FALSE)
  }
  invisible(NULL)
}
