#' A function to run the distribution fitting tool application
#'
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   shiny::runApp().
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_distribution_fitting_tool()
#' }
run_shiny_distribution_fitting_tool <- function(){
  shinyApp(ui = distribution_fitting_tool_UI, server = distribution_fitting_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the distribution fitting tool.

# Chart colours for the light or dark app theme. The charts have a transparent
# background, so they sit on the card colour of either theme.
dft_plot_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(font = "#cbd5e1", muted = "#94a3b8", grid = "rgba(148, 163, 184, 0.18)",
         line = "rgba(148, 163, 184, 0.35)", empirical = "#f1f5f9", bar = "#8b5cf6",
         bar_line = "#111a2b", hover_bg = "#16213a", hover_border = "#334155")
  } else {
    list(font = "#334155", muted = "#64748b", grid = "rgba(148, 163, 184, 0.30)",
         line = "rgba(100, 116, 139, 0.45)", empirical = "#0f172a", bar = "#7c3aed",
         bar_line = "#ffffff", hover_bg = "#ffffff", hover_border = "#cbd5e1")
  }
}

# Colours of the fitted models, which read well on both backgrounds.
dft_model_palette <- c("#3b82f6", "#f97316", "#10b981", "#ec4899", "#06b6d4", "#eab308")

# Applies the tool's chart styling: transparent background, theme colours,
# optional logarithmic x axis and a horizontal legend above the plot.
dft_plot_layout <- function(p, x_title, y_title, dark = FALSE, x_log = FALSE,
                            hovermode = "x unified", y_range = NULL) {
  colours <- dft_plot_colours(dark)
  axis <- function(title) {
    list(
      title = list(text = title, font = list(color = colours$muted)),
      gridcolor = colours$grid, linecolor = colours$line, tickcolor = colours$line,
      zerolinecolor = colours$grid, showline = TRUE, automargin = TRUE
    )
  }
  xaxis <- axis(x_title)
  if (x_log) xaxis$type <- "log"
  yaxis <- axis(y_title)
  if (!is.null(y_range)) yaxis$range <- y_range
  p <- plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = colours$font, family = "Inter, system-ui, sans-serif"),
    margin = list(l = 60, r = 16, t = 16, b = 50),
    hovermode = hovermode,
    hoverlabel = list(bgcolor = colours$hover_bg, bordercolor = colours$hover_border,
                      font = list(color = colours$font)),
    xaxis = xaxis,
    yaxis = yaxis,
    legend = list(orientation = "h", x = 0, y = 1.02, xanchor = "left", yanchor = "bottom",
                  font = list(color = colours$font), bgcolor = "rgba(0,0,0,0)")
  )
  plotly::config(p, displaylogo = FALSE,
                 modeBarButtonsToRemove = list("select2d", "lasso2d", "autoScale2d"))
}

# Dashed vertical lines at xs (slicing points, thresholds) for dft_plot_layout().
# Shapes on a log axis take log10 coordinates.
dft_vlines <- function(xs, log_scale = FALSE, dark = FALSE) {
  colour <- dft_plot_colours(dark)$muted
  lapply(xs, function(x) {
    position <- if (log_scale) log10(x) else x
    list(type = "line", xref = "x", yref = "paper", x0 = position, x1 = position, y0 = 0, y1 = 1,
         line = list(color = colour, width = 1.2, dash = "dash"))
  })
}

# Theme for the data preview table. It uses the page's CSS variables, so the
# table follows the light / dark theme without being redrawn.
dft_reactable_theme <- function() {
  reactable::reactableTheme(
    color = "var(--bs-body-color)",
    backgroundColor = "transparent",
    borderColor = "var(--sim-border)",
    highlightColor = "var(--dft-row-hover)",
    cellPadding = "0.45rem 0.6rem",
    headerStyle = list(
      color = "var(--sim-muted)", fontSize = "0.72rem", fontWeight = 800,
      textTransform = "uppercase", letterSpacing = "0.06em",
      borderBottomColor = "var(--sim-border-strong)"
    ),
    inputStyle = list(
      backgroundColor = "var(--sim-input-bg)", borderColor = "var(--sim-border-strong)",
      color = "var(--bs-body-color)", borderRadius = "10px"
    ),
    searchInputStyle = list(width = "100%", maxWidth = "280px"),
    selectStyle = list(
      backgroundColor = "var(--sim-input-bg)", borderColor = "var(--sim-border-strong)",
      color = "var(--bs-body-color)"
    ),
    paginationStyle = list(color = "var(--sim-muted)"),
    pageButtonHoverStyle = list(backgroundColor = "var(--sim-accent-soft)"),
    pageButtonActiveStyle = list(backgroundColor = "var(--sim-accent-soft)"),
    pageButtonCurrentStyle = list(backgroundColor = "var(--dft-accent)", color = "#ffffff")
  )
}

# Rounds a slider bound to three significant digits, down or up, so that the
# bound still includes the data.
dft_nice_bound <- function(x, up = FALSE) {
  if (!is.finite(x) || x == 0) return(x)
  step <- 10^(floor(log10(abs(x))) - 2)
  if (up) ceiling(x / step) * step else floor(x / step) * step
}

# Formats numbers for the result tables and tiles: `digits` significant digits
# with thousands separators, and a dash for missing or infinite values.
dft_dash <- intToUtf8(8212)

dft_fmt <- function(x, digits = 4) {
  out <- rep(dft_dash, length(x))
  ok <- is.finite(x)
  out[ok] <- formatC(signif(x[ok], digits), digits = digits, format = "fg", big.mark = ",")
  trimws(out)
}

# Builds an HTML table for the tool's results. `rows` is a list of lists of
# cells (strings or tags), `header` a character vector, `best` the index of a
# row to mark with a badge.
dft_html_table <- function(header, rows, best = NULL, best_label = "Best fit", note = NULL) {
  body <- lapply(seq_along(rows), function(i) {
    cells <- rows[[i]]
    first <- if (!is.null(best) && length(best) == 1 && !is.na(best) && i == best) {
      tagList(cells[[1]], tags$span(class = "dft-badge", best_label))
    } else {
      cells[[1]]
    }
    tags$tr(tags$td(first), lapply(cells[-1], tags$td))
  })
  tagList(
    div(
      class = "dft-table-wrap",
      tags$table(
        class = "dft-table",
        tags$thead(tags$tr(lapply(header, tags$th))),
        tags$tbody(body)
      )
    ),
    if (!is.null(note)) p(class = "dft-table-note", note)
  )
}

# Parameter cell: a muted parameter name followed by its value.
dft_param_cell <- function(name, value, digits = 4) {
  if (is.null(name) || identical(name, "")) return(tags$span(class = "dft-na", dft_dash))
  tagList(tags$span(class = "dft-param-name", name), dft_fmt(value, digits))
}

# Summary tile for the results area.
dft_stat_tile <- function(label, value, note = NULL, icon_name = NULL, accent = FALSE) {
  div(
    class = paste("dft-stat", if (accent) "dft-stat-accent"),
    div(class = "dft-stat-label", if (!is.null(icon_name)) icon(icon_name), label),
    div(class = "dft-stat-value", value),
    if (!is.null(note)) div(class = "dft-stat-note", note)
  )
}

# Maximum likelihood Gamma fit used by the severity analysis. This is the same
# optimisation MASS::fitdistr(x, "gamma", method = "L-BFGS-B", lower = c(0, 0),
# start = list(scale = 1, shape = 1)) runs, written out so that MASS is not a
# dependency; it returns the estimates in the same order (scale, shape).
fit_gamma_mle <- function(x) {
  fit <- function(y) {
    negative_loglik <- function(p) -sum(dgamma(y, shape = p[2], scale = p[1], log = TRUE))
    result <- optim(par = c(scale = 1, shape = 1), fn = negative_loglik, method = "L-BFGS-B", lower = c(0, 0))
    if (result$convergence > 0L) stop("optimization failed")
    result$par
  }
  estimate <- tryCatch(fit(x), error = function(e) NULL)
  # the optimiser can step onto its zero bounds (claims far from 1, or small
  # whole numbers) and stop; the profile likelihood still gives the estimate
  if (is.null(estimate)) return(fit_gamma_profile(x))
  list(estimate = estimate)
}

# Maximum likelihood Gamma fit from the profile likelihood: the shape k solves
# log(k) - digamma(k) = log(mean(x)) - mean(log(x)), whose left side falls from
# +Inf to 0, and the scale is mean(x) / k. Same return value as fit_gamma_mle().
fit_gamma_profile <- function(x) {
  target <- log(mean(x)) - mean(log(x))
  if (!is.finite(target) || target <= 0) stop("the claims must be positive and not all equal")
  shape <- stats::uniroot(function(k) log(k) - digamma(k) - target,
                          lower = 1e-8, upper = max(10, 10 / target), tol = 1e-12)$root
  list(estimate = c(scale = mean(x) / shape, shape = shape))
}

# Empirical cdf of claims evaluated at points: share of claims <= each point.
# With weights, each claim counts `weight` times.
empirical_cdf_at <- function(points, claims, weights = NULL) {
  if (is.null(weights)) return(findInterval(points, sort(claims)) / length(claims))
  o <- order(claims)
  cumulative <- c(0, cumsum(weights[o]))
  cumulative[findInterval(points, claims[o]) + 1] / sum(weights)
}

# Kolmogorov-Smirnov distance between the empirical cdf of claims and the
# cdf function cdf_fun: the largest gap on either side of each step of the
# empirical cdf, as in stats::ks.test().
ks_distance <- function(claims, cdf_fun) {
  x <- sort(claims)
  n <- length(x)
  if (n == 0) return(NA_real_)
  fitted <- cdf_fun(x)
  if (anyNA(fitted)) return(NA_real_)
  max(seq_len(n) / n - fitted, fitted - (seq_len(n) - 1) / n)
}

# Converts a data column to numbers. Text is trimmed; with a decimal comma the
# comma becomes a point, and with a decimal point thousands separators such as
# "1,234.5" are removed. Anything else that is not a number becomes NA.
dft_as_numeric <- function(x, dec = ".") {
  if (is.numeric(x) || is.logical(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  if (identical(dec, ",")) {
    x <- sub(",", ".", x, fixed = TRUE)
  } else {
    grouped <- grepl("^[-+]?[0-9]{1,3}(,[0-9]{3})+([.][0-9]*)?$", x)
    x[grouped] <- gsub(",", "", x[grouped], fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

# Names of the columns of df that are mostly numbers (at least 90% of the
# non-blank values convert), in their original order.
dft_numeric_columns <- function(df, dec = ".") {
  is_numeric <- vapply(df, function(column) {
    values <- dft_as_numeric(column, dec)
    present <- !is.na(column) & trimws(as.character(column)) != ""
    any(present) && mean(!is.na(values[present])) >= 0.9
  }, logical(1))
  names(df)[is_numeric]
}

# Reads an uploaded delimited text file. A UTF-8 byte order mark is skipped, so
# it does not end up in the first column name; column names are kept as they
# are (spaces allowed), with blanks and duplicates made unique.
dft_read_data <- function(path, header = TRUE, sep = ",", quote = "\"", dec = ".") {
  has_bom <- identical(readBin(path, "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))
  df <- utils::read.csv(
    path, header = header, sep = sep, quote = quote, dec = dec,
    check.names = FALSE, stringsAsFactors = FALSE, strip.white = TRUE,
    fileEncoding = if (has_bom) "UTF-8-BOM" else ""
  )
  column_names <- names(df)
  blank <- is.na(column_names) | column_names == ""
  column_names[blank] <- paste0("V", which(blank))
  names(df) <- make.unique(column_names, sep = "_")
  df
}

# Grid of claim sizes for drawing fitted cdfs: evenly spaced from zero, or on
# a log scale from the smallest claim, up to the largest claim.
dft_severity_grid <- function(claims, log_scale = FALSE, n = 500) {
  top <- max(claims)
  if (log_scale) {
    bottom <- min(claims[claims > 0])
    if (bottom >= top) return(top)
    exp(seq(log(bottom), log(top), length.out = n))
  } else {
    seq(0, top, length.out = n)
  }
}

# Points at which to draw the empirical cdf as a step line: every distinct
# claim, or evenly spaced quantiles when there are more than max_points.
dft_ecdf_points <- function(claims, max_points = 2000) {
  points <- sort(unique(claims))
  if (length(points) > max_points) {
    points <- unique(stats::quantile(claims, seq(0, 1, length.out = max_points), type = 1, names = FALSE))
  }
  points
}

# Empirical mean excess function of claims evaluated at points:
# mean(claims[claims > point]) - point, NaN where no claim exceeds the point.
# Uses cumulative sums of the sorted claims instead of one subset per point.
mean_excess_at <- function(points, claims) {
  sorted <- sort(claims)
  # number of claims <= each point
  below <- findInterval(points, sorted)
  sum_below <- c(0, cumsum(sorted))[below + 1]
  count_above <- length(sorted) - below
  (sum(sorted) - sum_below) / count_above - points
}

# Piecewise Pareto helpers. They cover the case the tool uses (no truncation,
# no reporting thresholds, no censoring, unit weights) with the same arithmetic
# as Pareto::PiecewisePareto_ML_Estimator_Alpha() and Pareto::pPiecewisePareto(),
# so the Pareto package is not needed.

# Maximum likelihood estimate of the Pareto alphas of a piecewise Pareto
# distribution with strictly increasing thresholds t, fitted to losses.
piecewise_pareto_alpha <- function(losses, t) {
  k <- length(t)
  if (!is.numeric(t) || k < 1 || anyNA(t) || any(t <= 0) || any(is.infinite(t))) {
    warning("t must be positive.")
    return(NaN)
  }
  if (!is.numeric(losses) || length(losses) < 1 || anyNA(losses) || any(losses < 0) || any(is.infinite(losses))) {
    warning("losses must be non-negative.")
    return(rep(NaN, k))
  }
  if (k > 1 && min(diff(t)) <= 0) {
    warning("t must be strictly ascending.")
    return(rep(NaN, k))
  }
  if (max(losses) <= max(t)) {
    warning("Number of losses > max(t) must be positive.")
    return(rep(NaN, k))
  }
  losses <- losses[losses > t[1]]
  if (length(losses) == 0) {
    warning("No losses larger than t[1].")
    return(rep(NaN, k))
  }
  upper <- c(t[-1], Inf)
  alpha <- numeric(k)
  for (i in seq_len(k)) {
    in_layer <- losses[losses >= t[i]]
    alpha[i] <- (length(in_layer) - sum(losses >= upper[i])) /
      sum(log(pmin(in_layer, upper[i]) / t[i]))
  }
  alpha
}

# Cumulative distribution function of a piecewise Pareto distribution with
# thresholds t and alphas alpha, evaluated at the vector x.
piecewise_pareto_cdf <- function(x, t, alpha) {
  if (is.null(x) || length(x) == 0) return(numeric())
  k <- length(t)
  valid <- is.numeric(t) && is.numeric(alpha) && k >= 1 && length(alpha) == k &&
    !anyNA(t) && !anyNA(alpha) && all(t > 0) && all(is.finite(t)) &&
    all(alpha >= 0) && all(is.finite(alpha)) && alpha[k] > 0 &&
    (k == 1 || min(diff(t)) > 0)
  if (!valid) {
    warning("t must be positive and increasing, alpha non-negative with a positive last value.")
    return(rep(NaN, length(x)))
  }
  # survival probability at each threshold: S(t[1]) = 1, S(t[j+1]) = S(t[j]) * (t[j] / t[j+1])^alpha[j]
  survival <- c(1, cumprod((t[-k] / t[-1])^alpha[-k]))
  j <- findInterval(x, t, left.open = TRUE)
  p <- numeric(length(x))
  above <- which(j > 0)
  p[above] <- 1 - survival[j[above]] * (t[j[above]] / x[above])^alpha[j[above]]
  p[is.na(x)] <- NaN
  p
}
