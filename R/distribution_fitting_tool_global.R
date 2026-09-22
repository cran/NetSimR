#' Run the distribution fitting application
#'
#' @description Opens NetDisFit, a shiny application that fits frequency and
#'   severity distributions to an uploaded claims file, compares them with the
#'   data, and splices Pareto tails onto a LogNormal body or fits a piecewise
#'   Pareto at chosen slicing points, ready for the NetSimR simulator.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{run_shiny_simulator}} and
#'   \code{\link{run_shiny_glm_fitting_tool}}, the other two applications;
#'   \code{\link{SlicedLNormParetoMean}}, \code{\link{SlicedLNormParetoCappedMean}},
#'   \code{\link{SlicedGammaParetoMean}} and \code{\link{SlicedGammaParetoCappedMean}}
#'   for the moments of the sliced distributions the tool fits.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_distribution_fitting_tool()
#' }
run_shiny_distribution_fitting_tool <- function(){
  shinyApp(ui = distribution_fitting_tool_UI, server = distribution_fitting_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the distribution fitting tool.

# Charts of the distribution fitting and GLM fitting tools. They are drawn with
# base graphics as PNG images (shiny::renderPlot()) on a transparent
# background, so they sit on the card colour of either theme (#ffffff in the
# light theme, #111a2b in the dark one), and are redrawn when the theme changes.

# Chart colours for the light or dark app theme. Eight hex digits are a
# translucent colour.
dft_plot_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(font = "#cbd5e1", muted = "#94a3b8", grid = "#94a3b82e", axis = "#94a3b873",
         empirical = "#f1f5f9", bar = "#8b5cf6", bar_border = "#111a2b", band_bar = "#14b8a64d")
  } else {
    list(font = "#334155", muted = "#64748b", grid = "#94a3b84d", axis = "#64748b8c",
         empirical = "#0f172a", bar = "#7c3aed", bar_border = "#ffffff", band_bar = "#0d948838")
  }
}

# Colours of the fitted models, which read well on both backgrounds.
dft_model_palette <- c("#3b82f6", "#f97316", "#10b981", "#ec4899", "#06b6d4", "#eab308")

# The colour of a series: a colour as it is, or the name of a theme colour
# ("empirical", "bar", "band_bar"), so that the data of a chart does not depend
# on the theme.
dft_series_colour <- function(colour, colours) {
  if (!is.null(colours[[colour]])) colours[[colour]] else colour
}

# Axis labels with thousands separators ("25,000", "0.25"); from 1e12 up, or
# below 1e-6 (but not zero), in scientific notation. Shares as "25%".
dft_axis_labels <- function(x, percent = FALSE) {
  if (percent) x <- 100 * x
  scientific <- x != 0 & (abs(x) >= 1e12 | abs(x) < 1e-6)
  out <- formatC(signif(x, 6), format = "fg", digits = 6, big.mark = ",")
  out[scientific] <- formatC(x[scientific], format = "g", digits = 3)
  out <- trimws(out)
  if (percent) paste0(out, "%") else out
}

# Ticks of a logarithmic axis over the range r: the powers of ten, with 2 and 5
# times each power when there are fewer than three of them, or ordinary ticks
# when the range is too short for either.
dft_log_ticks <- function(r) {
  powers <- 10^seq(floor(log10(r[1])), ceiling(log10(r[2])))
  ticks <- powers[powers >= r[1] & powers <= r[2]]
  if (length(ticks) < 3) {
    fine <- sort(c(powers, 2 * powers, 5 * powers))
    ticks <- fine[fine >= r[1] & fine <= r[2]]
  }
  if (length(ticks) < 2) {
    ticks <- pretty(r)
    ticks <- ticks[ticks >= r[1] & ticks <= r[2]]
  }
  ticks
}

# A range from zero (or below, for negative values) to a little above the
# values, for bars and averages.
dft_zero_range <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(c(0, 1))
  low <- min(0, values)
  high <- max(0, values)
  if (high == low) high <- low + 1
  c(if (low < 0) low - 0.06 * (high - low) else 0, high + 0.08 * (high - low))
}

# A range for lines that need not start at zero: from zero while zero is near
# the values (they cross it, or the smallest is within half of the largest from
# it, as for rates and frequencies), otherwise the values with 8% either side,
# so that a response around 1,000 does not draw as a flat line at the top.
dft_line_range <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(c(0, 1))
  r <- range(values)
  near_zero <- (r[1] <= 0 && r[2] >= 0) || (r[1] > 0 && r[1] <= 0.5 * r[2]) || (r[2] < 0 && r[2] >= 0.5 * r[1])
  if (near_zero) return(dft_zero_range(values))
  span <- if (diff(r) > 0) diff(r) else 0.1 * abs(r[1])
  c(r[1] - 0.08 * span, r[2] + 0.08 * span)
}

# Maps values on the secondary axis (range y2lim) onto the primary one (ylim).
dft_to_y2 <- function(values, ylim, y2lim) {
  ylim[1] + (values - y2lim[1]) / diff(y2lim) * diff(ylim)
}

# Category labels shortened to fit under a chart.
dft_short_label <- function(labels, max_chars = 24) {
  labels <- as.character(labels)
  long <- nchar(labels) > max_chars
  labels[long] <- paste0(substr(labels[long], 1, max_chars - 1), intToUtf8(8230))
  labels
}

# Legend entries packed into rows no wider than `width` inches: the row of each
# entry and its offset from the left of the row.
dft_legend_layout <- function(labels, width, cex) {
  key <- 0.3
  gap <- 0.07
  spacing <- 0.22
  item <- key + gap + graphics::strwidth(labels, units = "inches", cex = cex) + spacing
  row <- integer(length(labels))
  offset <- numeric(length(labels))
  current <- 1
  x <- 0
  for (i in seq_along(labels)) {
    if (x > 0 && x + item[i] - spacing > width) {
      current <- current + 1
      x <- 0
    }
    row[i] <- current
    offset[i] <- x
    x <- x + item[i]
  }
  list(row = row, offset = offset, rows = current, key = key, gap = gap)
}

# Draws the legend in the top margin, from `left` inches, one row every 1.4
# lines. Entries are lists with a label, a colour and a type: "line", "point"
# (a line with a marker) or "bar".
dft_draw_legend <- function(entries, layout, left, colours, cex) {
  inch <- graphics::par("csi")
  device_height <- graphics::par("din")[2]
  old <- graphics::par(xpd = NA)
  on.exit(graphics::par(old))
  to_x <- function(x) graphics::grconvertX(x, "inches", "user")
  to_y <- function(y) graphics::grconvertY(y, "inches", "user")
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    colour <- dft_series_colour(entry$colour, colours)
    x0 <- left + layout$offset[i]
    y <- device_height - ((layout$row[i] - 0.5) * 1.4 + 0.15) * inch
    if (identical(entry$type, "bar")) {
      graphics::rect(to_x(x0 + 0.04), to_y(y - 0.065), to_x(x0 + layout$key - 0.04), to_y(y + 0.065),
                     col = colour, border = NA)
    } else {
      graphics::segments(to_x(x0), to_y(y), to_x(x0 + layout$key), to_y(y), col = colour, lwd = 2.5)
      if (identical(entry$type, "point")) graphics::points(to_x(x0 + layout$key / 2), to_y(y), pch = 19, cex = 0.8, col = colour)
    }
    graphics::text(to_x(x0 + layout$key + layout$gap), to_y(y), entry$label, adj = c(0, 0.5), cex = cex, col = colours$font)
  }
}

# Opens a chart on the current device: theme colours, grid lines, axes with
# thousands separators, axis titles, and the legend above the plot, on as many
# rows as the width needs. The margins follow the widest tick labels, so that
# nothing is cut off at narrow widths. x_labels puts one category label at each
# of 1, 2, ..., slanted and thinned out when they do not fit side by side; y2
# (a list with lim and title) adds a secondary axis on the right, onto which
# values are mapped with dft_to_y2(). Returns the theme colours.
dft_plot_frame <- function(xlim, ylim, x_title, y_title, dark = FALSE, x_log = FALSE, legend = NULL,
                           x_labels = NULL, y_percent = FALSE, y2 = NULL) {
  colours <- dft_plot_colours(dark)
  cex_axis <- 0.8
  cex_title <- 0.85
  # the background is the device's: transparent in the apps (renderPlot(bg = "transparent"))
  graphics::par(fg = colours$axis, col.axis = colours$font, family = "sans",
                las = 1, tcl = -0.25, mgp = c(3, 0.4, 0), cex.axis = cex_axis, xpd = FALSE)
  # the height of a line of text and the size of the device, in inches
  inch <- graphics::par("csi")
  device <- graphics::par("din")
  lines_wide <- function(labels) max(graphics::strwidth(labels, units = "inches", cex = cex_axis), 0) / inch
  within <- function(ticks, r) ticks[ticks >= r[1] - 1e-9 * diff(r) & ticks <= r[2] + 1e-9 * diff(r)]
  y_ticks <- within(pretty(ylim, n = 6), ylim)
  y_labels <- dft_axis_labels(y_ticks, y_percent)
  left <- lines_wide(y_labels) + 2.1
  right <- 1
  if (!is.null(y2)) {
    y2_ticks <- within(pretty(y2$lim, n = 5), y2$lim)
    right <- lines_wide(dft_axis_labels(y2_ticks)) + 2.1
  }
  # category labels side by side when they fit, otherwise slanted, every few when crowded
  bottom <- 3.1
  slanted <- FALSE
  every <- 1
  if (!is.null(x_labels)) {
    x_labels <- dft_short_label(x_labels)
    slot <- (device[1] - (left + right) * inch) / length(x_labels)
    label_width <- graphics::strwidth(x_labels, units = "inches", cex = cex_axis)
    if (max(label_width) + 0.1 > slot) {
      slanted <- TRUE
      every <- max(1, ceiling(1.5 * inch * cex_axis / slot))
      bottom <- min(max(label_width) * sin(pi / 4) / inch + 2.6, 0.4 * device[2] / inch)
    }
  }
  cex_legend <- 0.82
  top <- 0.8
  if (length(legend) > 0) {
    layout <- dft_legend_layout(vapply(legend, function(entry) entry$label, character(1)),
                                device[1] - left * inch - 0.1, cex_legend)
    top <- layout$rows * 1.4 + 0.5
  }
  graphics::par(mar = c(bottom, left, top, right))
  graphics::plot.new()
  if (length(legend) > 0) {
    graphics::plot.window(c(0, 1), c(0, 1))
    dft_draw_legend(legend, layout, left * inch, colours, cex_legend)
  }
  graphics::plot.window(xlim, ylim, log = if (x_log) "x" else "", xaxs = if (is.null(x_labels)) "r" else "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::abline(h = y_ticks, col = colours$grid, lwd = 1)
  if (is.null(x_labels)) {
    x_range <- if (x_log) 10^usr[1:2] else usr[1:2]
    x_ticks <- within(if (x_log) dft_log_ticks(x_range) else pretty(x_range, n = 6), x_range)
    graphics::abline(v = x_ticks, col = colours$grid, lwd = 1)
    graphics::axis(1, at = x_ticks, labels = dft_axis_labels(x_ticks), lwd = 0, lwd.ticks = 1, col.ticks = colours$axis)
  } else {
    shown <- seq(1, length(x_labels), by = every)
    graphics::axis(1, at = seq_along(x_labels), labels = FALSE, lwd = 0, lwd.ticks = 1, col.ticks = colours$axis)
    if (slanted) {
      y <- graphics::grconvertY(graphics::grconvertY(usr[3], "user", "inches") - 0.4 * inch, "inches", "user")
      graphics::text(shown, y, x_labels[shown], srt = 45, adj = c(1, 1), xpd = NA, cex = cex_axis, col = colours$font)
    } else {
      graphics::axis(1, at = shown, labels = x_labels[shown], lwd = 0, lwd.ticks = 0)
    }
  }
  graphics::axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 1, col.ticks = colours$axis)
  if (!is.null(y2)) {
    graphics::axis(4, at = dft_to_y2(y2_ticks, ylim, y2$lim), labels = dft_axis_labels(y2_ticks), lwd = 0,
                   lwd.ticks = 1, col.ticks = colours$axis, col.axis = colours$muted)
    graphics::mtext(y2$title, side = 4, line = right - 1.2, col = colours$muted, cex = cex_title, las = 0)
  }
  graphics::box(bty = if (is.null(y2)) "l" else "u", col = colours$axis)
  graphics::mtext(x_title, side = 1, line = bottom - 1.2, col = colours$muted, cex = cex_title)
  graphics::mtext(y_title, side = 2, line = left - 1.2, col = colours$muted, cex = cex_title, las = 0)
  invisible(colours)
}

# Line chart of series, lists with a name, x, y, a colour and optionally step
# (a step line, as for an empirical cdf) and lwd; vlines are dashed markers
# (slicing points, thresholds). The legend is left out for a single series.
dft_line_chart <- function(series, x_title, y_title, dark = FALSE, x_log = FALSE, ylim = NULL, vlines = NULL) {
  usable <- function(x) is.finite(x) & (!x_log | x > 0)
  xs <- c(unlist(lapply(series, function(s) s$x)), vlines)
  xlim <- range(xs[usable(xs)])
  if (is.null(ylim)) ylim <- dft_zero_range(unlist(lapply(series, function(s) s$y)))
  legend <- if (length(series) > 1) lapply(series, function(s) list(label = s$name, colour = s$colour, type = "line"))
  colours <- dft_plot_frame(xlim, ylim, x_title, y_title, dark, x_log, legend)
  if (length(vlines) > 0) graphics::abline(v = vlines[usable(vlines)], col = colours$muted, lty = 2, lwd = 1.2)
  for (s in series) {
    ok <- usable(s$x) & is.finite(s$y)
    graphics::lines(s$x[ok], s$y[ok], type = if (isTRUE(s$step)) "s" else "l",
                    col = dft_series_colour(s$colour, colours), lwd = if (is.null(s$lwd)) 2 else s$lwd)
  }
  invisible(NULL)
}

# Chart by category: bars (a list with name, values, colour, and optionally a
# border colour and y2, the title of a secondary axis on the right for them)
# and lines with markers (lists with name, values and colour). With the bars on
# the right axis, the left axis follows the lines (dft_line_range()).
dft_category_chart <- function(labels, bars = NULL, lines = list(), x_title, y_title, dark = FALSE, y_percent = FALSE) {
  n <- length(labels)
  on_y2 <- !is.null(bars) && !is.null(bars$y2)
  line_values <- unlist(lapply(lines, function(l) l$values))
  ylim <- if (on_y2) dft_line_range(line_values) else dft_zero_range(c(line_values, if (!is.null(bars)) bars$values))
  y2 <- if (on_y2) list(lim = dft_zero_range(bars$values), title = bars$y2)
  legend <- c(if (!is.null(bars)) list(list(label = bars$name, colour = bars$colour, type = "bar")),
              lapply(lines, function(l) list(label = l$name, colour = l$colour, type = "point")))
  colours <- dft_plot_frame(c(0.4, n + 0.6), ylim, x_title, y_title, dark, legend = legend, x_labels = labels,
                            y_percent = y_percent, y2 = y2)
  if (!is.null(bars)) {
    top <- if (on_y2) dft_to_y2(bars$values, ylim, y2$lim) else bars$values
    base <- if (on_y2) dft_to_y2(0, ylim, y2$lim) else 0
    # a border separates the bars while they are wide enough
    border <- if (!is.null(bars$border) && graphics::par("pin")[1] / n > 0.06) dft_series_colour(bars$border, colours) else NA
    graphics::rect(seq_len(n) - 0.42, base, seq_len(n) + 0.42, top, col = dft_series_colour(bars$colour, colours),
                   border = border, lwd = 1)
  }
  point_size <- if (n > 40) 0.55 else if (n > 20) 0.75 else 0.95
  for (l in lines) {
    colour <- dft_series_colour(l$colour, colours)
    graphics::lines(seq_len(n), l$values, col = colour, lwd = 2.2)
    graphics::points(seq_len(n), l$values, pch = 19, cex = point_size, col = colour)
  }
  invisible(NULL)
}

# Histogram of counts in the bins between breaks.
dft_histogram_chart <- function(breaks, counts, x_title, y_title, dark = FALSE) {
  colours <- dft_plot_frame(range(breaks), dft_zero_range(counts), x_title, y_title, dark)
  border <- if (graphics::par("pin")[1] / length(counts) > 0.06) colours$bar_border else NA
  graphics::rect(breaks[-length(breaks)], 0, breaks[-1], counts, col = colours$bar, border = border, lwd = 1)
  invisible(NULL)
}

# Rounds a slider bound to three significant digits, down or up, so that the
# bound still includes the data.
dft_nice_bound <- function(x, up = FALSE) {
  if (!is.finite(x) || x == 0) return(x)
  step <- 10^(floor(log10(abs(x))) - 2)
  if (up) ceiling(x / step) * step else floor(x / step) * step
}

# The top of a slider that places a slicing point or threshold among the
# claims: the largest claim once the top half a percent are left out, rounded
# up, rather than the largest claim itself. Claims are heavy tailed, and with
# the largest claim as the top the body of the distribution took less than a
# pixel of the bar (3,000 claims around 1,000 and one of 5,000,000: the first
# user click snapped to 380,000 or to the bottom). Leaving out whole claims
# rather than taking the 99.5th percentile keeps the largest claim as the top
# of fewer than two hundred claims, where half a percent is not a claim at
# all; a point above the top can still be typed.
dft_slider_top <- function(x) {
  sorted <- sort(x)
  if (!length(sorted)) return(NA_real_)
  dft_nice_bound(sorted[length(sorted) - floor(length(sorted) * 0.005)], up = TRUE)
}

# Formats numbers for the result tables and tiles: `digits` significant digits
# with thousands separators, and a dash for missing or infinite values. Values
# from 1e15 up, or below 1e-6 (but not zero), are written as 1.234e+20: in full
# they would be long runs of digits, some of them spurious.
dft_dash <- intToUtf8(8212)

dft_fmt <- function(x, digits = 4) {
  out <- rep(dft_dash, length(x))
  ok <- is.finite(x)
  scientific <- ok & x != 0 & (abs(x) >= 1e15 | abs(x) < 1e-6)
  plain <- ok & !scientific
  out[plain] <- formatC(signif(x[plain], digits), digits = digits, format = "fg", big.mark = ",")
  out[scientific] <- formatC(x[scientific], digits = digits, format = "g")
  trimws(out)
}

# Formats a count or total weight in full with thousands separators (format()
# switches to 1.61e+09 for large doubles); very large values as dft_fmt().
dft_fmt_count <- function(x) {
  if (is.finite(x) && abs(x) < 1e15) formatC(x, format = "f", digits = 0, big.mark = ",") else dft_fmt(x)
}

# Builds an HTML table for the tool's results. `rows` is a list of lists of
# cells (strings or tags), `header` a character vector, `best` the index of a
# row to mark with a badge. `align` gives "left" for the columns to align left
# (text); the others are aligned right, except the first. `class` is added to
# the scrolling container.
dft_html_table <- function(header, rows, best = NULL, best_label = "Best fit", note = NULL,
                           align = NULL, class = NULL) {
  cell_class <- function(j) if (isTRUE(unname(align[j]) == "left")) "dft-left"
  body <- lapply(seq_along(rows), function(i) {
    cells <- rows[[i]]
    if (!is.null(best) && length(best) == 1 && !is.na(best) && i == best) {
      cells[[1]] <- tagList(cells[[1]], tags$span(class = "dft-badge", best_label))
    }
    tags$tr(lapply(seq_along(cells), function(j) tags$td(class = cell_class(j), cells[[j]])))
  })
  tagList(
    div(
      class = paste(c("dft-table-wrap", class), collapse = " "),
      tags$table(
        class = "dft-table",
        tags$thead(tags$tr(lapply(seq_along(header), function(j) tags$th(class = cell_class(j), header[[j]])))),
        tags$tbody(body)
      )
    ),
    if (!is.null(note)) p(class = "dft-table-note", note)
  )
}

# Escapes text for HTML (htmltools::htmlEscape() is not re-exported by shiny).
dft_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

# Preview of a data frame: its first max_rows rows and max_cols columns,
# numbered, in a container that scrolls both ways, so that long and wide files
# stay usable. Numbers are aligned right and missing values shown as a dash.
# The table is the markup of dft_html_table(), written as one string: built as
# tags, a file of 1,000 columns took 20 seconds.
dft_data_preview <- function(df, max_rows = 100, max_cols = 50) {
  rows <- min(nrow(df), max_rows)
  cols <- min(ncol(df), max_cols)
  shown <- df[seq_len(rows), seq_len(cols), drop = FALSE]
  left <- c(TRUE, !vapply(shown, function(column) is.numeric(column) || is.logical(column), logical(1)))
  opening <- function(tag) ifelse(left, paste0("<", tag, " class=\"dft-left\">"), paste0("<", tag, ">"))
  td <- opening("td")
  missing <- paste0("<span class=\"dft-na\">", dft_dash, "</span>")
  cells <- lapply(seq_len(cols), function(j) {
    column <- shown[[j]]
    text <- dft_escape(as.character(column))
    text[is.na(column)] <- missing
    paste0(td[j + 1], text, "</td>")
  })
  numbers <- paste0(td[1], formatC(seq_len(rows), format = "d", big.mark = ","), "</td>")
  body <- if (rows > 0) paste0(do.call(paste0, c(list("<tr>", numbers), cells, list("</tr>"))), collapse = "") else ""
  header <- paste0(opening("th"), dft_escape(c("#", names(shown))), "</th>", collapse = "")
  count <- function(x) format(x, big.mark = ",")
  columns_note <- if (ncol(df) > max_cols) paste(count(max_cols), "of", count(ncol(df)), "columns")
  note <- if (nrow(df) > max_rows) {
    paste0("Showing the first ", count(max_rows), " of ", count(nrow(df)), " rows",
           if (!is.null(columns_note)) paste(" and", columns_note), ".")
  } else {
    paste0(if (nrow(df) == 1) "1 row" else paste("All", count(nrow(df)), "rows"),
           if (!is.null(columns_note)) paste(" and the first", columns_note), ".")
  }
  HTML(paste0(
    "<div class=\"dft-table-wrap dft-table-preview\"><table class=\"dft-table\"><thead><tr>", header,
    "</tr></thead><tbody>", body, "</tbody></table></div><p class=\"dft-table-note\">", dft_escape(note), "</p>"
  ))
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

# Maximum likelihood Gamma fit used by the severity analysis, with the
# estimates in the order MASS::fitdistr(x, "gamma") gives them (scale, shape).
# It solves the likelihood equations (fit_gamma_profile()) rather than running
# an optimiser from scale = 1, shape = 1 as fitdistr() does: that optimiser
# stops short of the maximum, and for claims of 1e9 or more it reports success
# far from it (a shape of 141 instead of 2 at 1e12).
fit_gamma_mle <- function(x) {
  fit_gamma_profile(x)
}

# Maximum likelihood Gamma fit from the profile likelihood: the shape k solves
# log(k) - digamma(k) = log(mean(x)) - mean(log(x)), whose left side falls from
# +Inf to 0, and the scale is mean(x) / k. The right side is computed on
# x / mean(x), so that it does not depend on the scale of the claims.
fit_gamma_profile <- function(x) {
  target <- -mean(log(x / mean(x)))
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
# empirical cdf, as in stats::ks.test(). sorted = TRUE says the claims are
# already sorted, when several cdfs are compared with the same claims.
ks_distance <- function(claims, cdf_fun, sorted = FALSE) {
  x <- if (sorted) claims else sort(claims)
  n <- length(x)
  if (n == 0) return(NA_real_)
  fitted <- cdf_fun(x)
  if (anyNA(fitted)) return(NA_real_)
  max(seq_len(n) / n - fitted, fitted - (seq_len(n) - 1) / n)
}

# Converts a data column to numbers. Text is trimmed; thousands separators are
# removed ("1,234.5", or "1.234,5" with a decimal comma) and a decimal comma
# becomes a point. Anything else that is not a number becomes NA.
dft_as_numeric <- function(x, dec = ".") {
  if (is.numeric(x) || is.logical(x)) return(as.numeric(x))
  x <- as.character(x)
  # text that is not valid in the session's encoding is not a number, and would
  # stop the text functions below (an error in an observer ends the session)
  x[!validUTF8(x)] <- NA
  x <- trimws(x)
  if (identical(dec, ",")) {
    grouped <- grepl("^[-+]?[0-9]{1,3}([.][0-9]{3})+(,[0-9]*)?$", x)
    x[grouped] <- gsub(".", "", x[grouped], fixed = TRUE)
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
    # (bytes, not characters: a value need not be valid text)
    present <- !is.na(column) & !grepl("^[[:space:]]*$", as.character(column), useBytes = TRUE)
    any(present) && mean(!is.na(values[present])) >= 0.9
  }, logical(1))
  names(df)[is_numeric]
}

# Reads an uploaded delimited text file. A UTF-8 byte order mark is skipped, so
# it does not end up in the first column name, and a file that is not UTF-8 is
# read as Latin-1 (Excel on a German or French Windows writes CSV files in the
# Windows-1252 code page): read as UTF-8, an accented character makes a string
# that is not valid text, on which the text functions stop. Column names are
# kept as they are (spaces allowed), with blanks and duplicates made unique.
# The header line is read on its own: given one, read.csv() takes the first
# column as row names when the data rows have one more field than the header
# (a separator at the end of each row), which shifts every column onto the
# next one's values. The columns are sized by the widest line, not by the first
# five as read.table() sizes them without a header: a later row with an extra
# field then keeps it in its own column, where its surplus started a new row
# in the first column, a claim that was never in the file.
dft_read_data <- function(path, header = TRUE, sep = ",", quote = "\"", dec = ".") {
  has_bom <- identical(readBin(path, "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))
  lines <- readLines(path, warn = FALSE)
  encoding <- if (has_bom) "UTF-8-BOM" else if (all(validUTF8(lines))) "" else "latin1"
  # the number of fields in the widest line from `skip` lines on (NA for a line
  # that ends inside a quoted field)
  widest <- function(skip) {
    fields <- utils::count.fields(path, sep = sep, quote = quote, skip = skip, comment.char = "")
    max(0L, fields, na.rm = TRUE)
  }
  read <- function(skip = 0L, columns, ...) {
    utils::read.csv(
      path, header = FALSE, sep = sep, quote = quote, dec = dec, skip = skip,
      col.names = paste0("V", seq_len(columns)), fill = TRUE,
      check.names = FALSE, stringsAsFactors = FALSE, strip.white = TRUE,
      fileEncoding = encoding, ...
    )
  }
  if (header) {
    # blank lines before the header are skipped, as read.csv() skips them
    # (bytes, not characters: the file need not be UTF-8)
    not_blank <- grepl("[^[:space:]]", lines, useBytes = TRUE)
    blank_lines <- match(TRUE, not_blank, nomatch = 1L) - 1L
    # a header shorter than the data rows leaves blank names, which get a V
    # name below; a header longer than them gives empty columns
    columns <- widest(blank_lines)
    column_names <- unlist(read(blank_lines, columns, nrows = 1, colClasses = "character", na.strings = character(0)),
                           use.names = FALSE)
    df <- tryCatch(read(blank_lines + 1L, columns), error = function(e) {
      # a file with only the header has no data rows, which the tools report
      if (any(not_blank[-seq_len(blank_lines + 1L)])) stop(e)
      as.data.frame(matrix(logical(0), 0, columns))
    })
  } else {
    df <- read(columns = widest(0L))
    column_names <- names(df)
  }
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
# so the Pareto package is not needed, except that a loss equal to the first
# threshold counts in the first layer.

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
  # the first layer includes a claim at t[1], so that with t[1] the smallest
  # claim it is the Severity tab's Pareto (the Pareto package leaves it out)
  losses <- losses[losses >= t[1]]
  if (length(losses) == 0) {
    warning("No losses at or above t[1].")
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

# Maximum likelihood fits of the Frequency tab, with the fields of
# fitdistrplus::fitdist() that the tool uses: estimate, sd, cor, loglik, aic,
# bic and n (the number of rows, as fitdist() counts it for the BIC). Weights
# are case weights, as in fitdist(): a row of weight 3 counts as three
# observations, so the log-likelihood is the weighted sum of the log densities
# and the standard errors come from the information of the total weight.

# Poisson: the MLE of lambda is the (weighted) mean, and its standard error
# sqrt(lambda / total weight). fitdist() finds it with an optimiser, which
# stops within about 1e-8 of it.
fit_poisson_mle <- function(x, weights = NULL) {
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  total <- sum(w)
  lambda <- sum(w * x) / total
  loglik <- sum(w * stats::dpois(x, lambda, log = TRUE))
  list(estimate = c(lambda = lambda), sd = c(lambda = sqrt(lambda / total)), cor = NULL,
       loglik = loglik, aic = -2 * loglik + 2, bic = -2 * loglik + log(length(x)),
       n = length(x), distname = "pois", weighted = !is.null(weights), capped = FALSE)
}

# The Negative Binomial size at which the fit stops: when the variance of the
# counts is not above their mean, the likelihood rises without end as the size
# grows, towards the Poisson, and has no maximum. At this size the Negative
# Binomial is the Poisson for every practical purpose.
dft_nbinom_max_size <- 1e8

# t - log(1 + t) for t >= 0, without the cancellation of the direct formula
# for small t (its series t^2/2 - t^3/3 + ... converges fast below 0.1).
dft_log1p_excess <- function(t) {
  if (t >= 0.1) return(t - log1p(t))
  m <- 2:30
  sum((-1)^m * t^m / m)
}

# Sums over the rows of w * sum(f(j) for j = 0, ..., x - 1), for the integer
# counts x, as sums over j of f(j) times the weight of the counts above j.
# Counts above `limit` use the closed forms of the two sums the fit needs,
# from j = limit up: sum(j / (size + j)) = (x - limit) - size * (digamma(size
# + x) - digamma(size + limit)) and sum(1 / (size + j)^2) = trigamma(size +
# limit) - trigamma(size + x).
dft_nbinom_count_sums <- function(x, w, limit = 1e6) {
  top <- min(max(x), limit)
  # weight of the counts above j, for j = 0, ..., top - 1
  by_value <- numeric(top + 1)
  summed <- rowsum(w, pmin(x, top))
  by_value[as.numeric(rownames(summed)) + 1] <- summed
  above <- rev(cumsum(rev(by_value)))[-1]
  j <- seq_len(top) - 1
  big <- x > limit
  list(
    # sum over the rows of w * sum(j / (size + j), j < x)
    ratio = function(size) {
      s <- sum(above * j / (size + j))
      if (any(big)) s <- s + sum(w[big] * ((x[big] - limit) - size * (digamma(size + x[big]) - digamma(size + limit))))
      s
    },
    # sum over the rows of w * sum(1 / (size + j)^2, j < x)
    square = function(size) {
      s <- sum(above / (size + j)^2)
      if (any(big)) s <- s + sum(w[big] * (trigamma(size + limit) - trigamma(size + x[big])))
      s
    }
  )
}

# Negative Binomial in fitdist()'s parameters, size and mu. The MLE of mu is
# the (weighted) mean whatever the size, so the size solves the likelihood
# equation for it with mu at the mean. That equation has a root, and only one,
# exactly when the variance of the counts (divided by the total weight) is
# above the mean (Levin and Reeds 1977); otherwise the likelihood rises
# towards the Poisson as the size grows. The standard errors come from the
# observed information, whose off-diagonal term is zero at the maximum.
# fitdist() optimises both parameters with Nelder-Mead, which stops about 1e-4
# (relative) from the maximum.
fit_nbinom_mle <- function(x, weights = NULL) {
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  total <- sum(w)
  mu <- sum(w * x) / total
  if (!(mu > 0)) stop("the claim counts are all zero")
  spread <- sum(w * (x - mu)^2) / total
  sums <- dft_nbinom_count_sums(x, w)
  # the derivative of the log-likelihood in the size, times the size, with mu at the mean:
  # positive below the MLE and negative above it. As digamma(x + size) - digamma(size)
  # - log(1 + mu / size) it is a difference of nearly equal numbers when the size is
  # large (its sign at 1e8 was rounding error); since sum(w * x) = total * mu it is
  # also the difference of these two positive sums, which keep their precision
  score <- function(log_size) {
    size <- exp(log_size)
    total * size * dft_log1p_excess(mu / size) - sums$ratio(size)
  }
  capped <- !(spread > mu)
  if (!capped) {
    # a bracket around the root, from the moment estimate mu^2 / (variance - mu): the
    # score is positive as the size tends to 0 and negative for large sizes
    start <- min(2 * log(mu) - log(spread - mu), 600)
    lower <- start
    while (isTRUE(score(lower) <= 0) && lower > start - 60) lower <- lower - 1
    upper <- lower + 1
    while (isTRUE(score(upper) >= 0) && upper < start + 60) upper <- upper + 1
    # the variance above the mean only by rounding: no sign change within e^60 of the start
    capped <- !isTRUE(score(lower) > 0 && score(upper) < 0)
  }
  size <- if (capped) {
    dft_nbinom_max_size
  } else {
    exp(stats::uniroot(score, c(lower, upper), tol = 1e-12)$root)
  }
  loglik <- sum(w * stats::dnbinom(x, size = size, mu = mu, log = TRUE))
  # observed information, minus the second derivatives of the log-likelihood; in the
  # size, with mu at the mean, it is sum(w * sum(1 / (size + j)^2, j < x)) - total * mu /
  # (size * (size + mu)) (trigamma(x + size) - trigamma(size) = -sum(1 / (size + j)^2, j < x))
  info <- matrix(c(
    sums$square(size) - total * mu / (size * (size + mu)),
    -sum(w * (x - mu) / (size + mu)^2),
    -sum(w * (x - mu) / (size + mu)^2),
    sum(w * (x / mu^2 - (x + size) / (size + mu)^2))
  ), 2, 2, dimnames = list(c("size", "mu"), c("size", "mu")))
  if (capped) {
    # the likelihood is flat in the size: it has no standard error
    sd <- c(size = NA_real_, mu = 1 / sqrt(info[2, 2]))
    correlation <- NULL
  } else {
    vcov <- tryCatch(solve(info), error = function(e) matrix(NA_real_, 2, 2, dimnames = dimnames(info)))
    variances <- diag(vcov)
    sd <- ifelse(is.finite(variances) & variances > 0, sqrt(variances), NA_real_)
    names(sd) <- c("size", "mu")
    correlation <- if (all(is.finite(sd))) stats::cov2cor(vcov)
  }
  list(estimate = c(size = size, mu = mu), sd = sd, cor = correlation,
       loglik = loglik, aic = -2 * loglik + 4, bic = -2 * loglik + 2 * log(length(x)),
       n = length(x), distname = "nbinom", weighted = !is.null(weights), capped = capped)
}

# Prints a count fit as summary() of a fitdistrplus::fitdist() fit did.
dft_print_count_fit <- function(fit) {
  name <- if (identical(fit$distname, "pois")) "Poisson" else "Negative Binomial"
  cat(name, "distribution fitted by maximum likelihood\n")
  cat("Parameters:\n")
  print(data.frame(estimate = fit$estimate, `Std. Error` = fit$sd, check.names = FALSE))
  cat("Loglikelihood: ", fit$loglik, "  AIC: ", fit$aic, "  BIC: ", fit$bic, "\n")
  if (!is.null(fit$cor)) {
    cat("Correlation matrix:\n")
    print(fit$cor)
  }
  if (isTRUE(fit$capped)) {
    cat("\nThe variance of the counts is not above their mean, so the likelihood keeps rising as the\n",
        "size grows and the Negative Binomial tends to the Poisson. The size is set to ",
        dft_fmt_count(dft_nbinom_max_size), ".\n", sep = "")
  }
  if (isTRUE(fit$weighted)) cat("\nEach row counts as many observations as its weight.\n")
  invisible(fit)
}

# Data of the charts. Each function returns what its chart draws, so that it
# can be checked without drawing; `cdfs` is a named list of the fitted cdfs.

# Frequency histogram: the share of the observations (of the weight, with
# weights) in about `bins` bins of consecutive counts, and the probability of
# each bin under each fitted model. The last bin ends at the largest count.
# When the smallest count is above zero, a first bin holds the counts below
# it: the data has none there, but a fitted model may put some of its
# probability there (zero-truncated counts), which is the lack of fit the
# chart is there to show.
dft_count_hist_data <- function(counts, weights = NULL, bins = 20, cdfs = list()) {
  w <- if (is.null(weights)) rep(1, length(counts)) else weights
  lowest <- min(counts)
  width <- max(1, ceiling((max(counts) - lowest + 1) / max(1, bins)))
  starts <- seq(lowest, max(counts), by = width)
  ends <- starts + width - 1
  ends[length(ends)] <- max(counts)
  bin <- factor((counts - lowest) %/% width + 1, levels = seq_along(starts))
  observed <- as.numeric(tapply(w, bin, sum))
  observed[is.na(observed)] <- 0
  if (lowest > 0) {
    starts <- c(0, starts)
    ends <- c(lowest - 1, ends)
    observed <- c(0, observed)
  }
  # (format = "d" gives "NA" from 2^31 up)
  label <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  labels <- ifelse(starts == ends, label(starts), paste0(label(starts), "-", label(ends)))
  list(labels = labels, observed = observed / sum(w),
       fitted = lapply(cdfs, function(cdf) cdf(ends) - cdf(starts - 1)))
}

# Frequency cdf chart: the (weighted) empirical cdf and the fitted cdfs at
# every count from 0 to the largest, or at 5,000 of them.
dft_count_cdf_data <- function(counts, weights = NULL, cdfs = list()) {
  top <- max(counts)
  points <- if (top <= 5000) seq(0, top) else unique(round(seq(0, top, length.out = 5000)))
  list(x = points, empirical = empirical_cdf_at(points, counts, weights),
       fitted = lapply(cdfs, function(cdf) cdf(points)))
}

# Claim size cdf chart: the empirical cdf at its steps and the fitted cdfs on a
# grid (evenly or log spaced) that includes the extra points, the slicing
# points or thresholds where spliced cdfs change shape.
dft_severity_cdf_data <- function(claims, cdfs = list(), log_scale = FALSE, extra_points = NULL) {
  points <- dft_ecdf_points(claims)
  grid <- sort(unique(c(dft_severity_grid(claims, log_scale), extra_points)))
  list(x = points, empirical = empirical_cdf_at(points, claims), grid = grid,
       fitted = lapply(cdfs, function(cdf) cdf(grid)))
}

# Claim size histogram: about `bins` bins with round breaks, as hist() makes them.
dft_severity_hist_data <- function(claims, bins = 20) {
  h <- graphics::hist(claims, breaks = max(1, bins), plot = FALSE)
  list(breaks = h$breaks, counts = h$counts)
}

# Mean excess chart: the empirical mean excess at the steps of the empirical
# cdf, leaving out the largest claim, which no claim exceeds.
dft_mean_excess_data <- function(claims) {
  points <- dft_ecdf_points(claims)
  mean_excess <- mean_excess_at(points, claims)
  defined <- is.finite(mean_excess)
  list(x = points[defined], y = mean_excess[defined])
}

# Series of a cdf chart for dft_line_chart(): the empirical cdf as a step
# line, then each fitted cdf in its colour; step draws the fitted cdfs as
# steps too (counts).
dft_cdf_series <- function(data, colours = dft_model_palette[seq_along(data$fitted)], step = FALSE) {
  x <- if (is.null(data$grid)) data$x else data$grid
  fitted <- Map(function(name, y, colour) list(name = name, x = x, y = y, colour = colour, step = step),
                names(data$fitted), data$fitted, colours)
  c(list(list(name = "Empirical", x = data$x, y = data$empirical, colour = "empirical", step = TRUE, lwd = 2.5)),
    unname(fitted))
}
