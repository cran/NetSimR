#shiny modules for the Report and Compare tabs of the claims simulator
#
#both modules read the latest run from a reactive (last_run) that holds NULL until the
#first successful run and then list(id, settings, data, finished)

# ---------------------------------------------------------------- helpers

#' Empty-state card used by the Report and Compare tabs
#'
#' @param icon_name Font Awesome icon name.
#' @param title Short title.
#' @param text Explanation shown under the title.
#' @param ... Optional buttons shown under the text.
#' @noRd
sim_tab_empty_state <- function(icon_name, title, text, ...) {
  bslib::card(
    class = "sim-empty-card",
    bslib::card_body(
      div(class = "sim-empty-icon", icon(icon_name)),
      div(class = "sim-empty-title", title),
      p(class = "sim-empty-text", text),
      ...
    )
  )
}

#' Button that opens the Claims Simulator tab
#'
#' Clicks the navbar link in the browser, so it works from any module.
#' @noRd
sim_tab_open_simulator_button <- function() {
  tags$button(
    type = "button",
    class = "btn btn-primary",
    onclick = "var l = document.querySelector('.navbar a[data-value=\"simulator\"]'); if (l) l.click();",
    icon("play"), " Open the simulator"
  )
}

#' Format an amount with thousands separators
#'
#' @param x A number.
#' @param digits Decimal places; chosen from the size of \code{x} when NULL.
#' @return A string, or a dash when \code{x} is missing.
#' @noRd
sim_tab_fmt_amount <- function(x, digits = NULL) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !is.numeric(x)) return(intToUtf8(8212))
  x <- as.numeric(x[1])
  if (is.null(digits)) digits <- if (abs(x) >= 1000) 0 else 2
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

#' Format a whole number with thousands separators
#' @noRd
sim_tab_fmt_int <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(intToUtf8(8212))
  formatC(round(as.numeric(x[1])), format = "d", big.mark = ",")
}

#' Format a probability as a percentage
#' @noRd
sim_tab_fmt_pct <- function(p, digits = 1) {
  if (is.null(p) || length(p) == 0 || is.na(p[1])) return(intToUtf8(8212))
  p <- as.numeric(p[1])
  #small probabilities get an extra decimal so they do not round to zero
  if (p > 0 && p < 0.01 && digits < 2) digits <- 2
  paste0(formatC(100 * p, format = "f", digits = digits), "%")
}

#' Short description of a run's settings, e.g. for the compare tab
#'
#' @param s The list of \code{simulate_function} arguments.
#' @return A single string such as
#'   "Poisson (lamda = 5) / Log-Normal (mu = 8, sigma = 1.5), EEL Limited Layer 200,000 xs 50,000, AL none".
#' @noRd
sim_tab_describe_settings <- function(s) {
  is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  fmt <- function(x) {
    if (!is_number(x)) return("?")
    formatC(x, format = "f", digits = if (x == round(x)) 0 else 2, big.mark = ",")
  }
  fmt_param <- function(x) sub("\\.?0+$", "", formatC(x, format = "f", digits = 4))

  distr_text <- function(options, id, values) {
    known <- is.character(id) && length(id) == 1 && id %in% names(options)
    label <- if (known) options[[id]]@distr_label else if (is.character(id) && length(id) == 1) gsub("_", " ", id) else "?"
    values <- suppressWarnings(as.numeric(unlist(values)))
    if (length(values) == 0 || anyNA(values)) return(label)
    #labels such as "lambda (mean claims)" are shortened to their first word
    labels <- if (known) sub("\\s*\\(.*$", "", options[[id]]@param_labels) else NULL
    shown <- if (length(labels) == length(values)) paste(labels, "=", fmt_param(values)) else fmt_param(values)
    paste0(label, " (", paste(shown, collapse = ", "), ")")
  }

  structure_text <- function(kind, deductible, limit) {
    if (!is.character(kind) || length(kind) != 1 || kind == "No Reinsurance Structure") return("none")
    switch(
      kind,
      "Unlimited Layer" = paste("Unlimited Layer xs", fmt(deductible)),
      "Limited Layer" = paste("Limited Layer", fmt(limit), "xs", fmt(deductible)),
      "Exclude Layer" = paste("Exclude Layer", fmt(limit), "xs", fmt(deductible)),
      kind
    )
  }

  eel <- structure_text(s$reinsuranceStructureEEL, s$reinsurance_structure_eel_dedctible_amount,
                        s$reinsurance_structure_eel_limit_amount)
  if (identical(s$reinsuranceStructureEEL, "Limited Layer") &&
      isTRUE(s$reinsuranceStructureLimitedReinstatements) &&
      is_number(s$reinsuranceStructureReinstatementLimit)) {
    eel <- paste0(eel, " (", fmt(s$reinsuranceStructureReinstatementLimit), " reinst.)")
  }
  al <- structure_text(s$reinsuranceStructureAL, s$reinsurance_structure_al_dedctible_amount,
                       s$reinsurance_structure_al_limit_amount)

  slices <- if (isTRUE(s$paretoSlice) && is_number(s$pareto_slice_times)) {
    paste(fmt(s$pareto_slice_times), if (s$pareto_slice_times == 1) "Pareto slice" else "Pareto slices")
  }
  cap <- if (isTRUE(s$sevCapBinary)) paste("cap", fmt(s$sev_cap_amount))
  seed <- if (isTRUE(s$seedSetBinary) && is_number(s$seedValue)) paste("seed", fmt(s$seedValue))

  parts <- c(
    paste(distr_text(freq_dist_options, s$freqDistr, s$freq_params), "/",
          distr_text(sev_dist_options, s$sevDistr, s$sev_params)),
    slices, cap, paste("EEL", eel), paste("AL", al), seed
  )
  paste(parts, collapse = ", ")
}

#' Return periods at which the compare chart evaluates every run
#'
#' A fixed log-spaced grid, so curves of different runs line up point for point.
#' @noRd
sim_tab_return_period_grid <- exp(seq(log(2), log(1e6), length.out = 700))

#' Losses by return period for one run
#'
#' Uses the same quantile approach as the report: the loss at return period r is the
#' quantile of the totals at probability 1 - 1/r. Return periods stop at n/10, so at
#' least 10 simulations lie beyond each point.
#' @param totals Numeric vector of simulated totals.
#' @return A data frame with columns rp and value (possibly empty).
#' @noRd
sim_tab_return_period_curve <- function(totals) {
  totals <- totals[!is.na(totals)]
  max_rp <- length(totals) / 10
  if (max_rp < 2) return(data.frame(rp = numeric(0), value = numeric(0)))
  rps <- c(sim_tab_return_period_grid[sim_tab_return_period_grid < max_rp], max_rp)
  data.frame(rp = rps, value = stats::quantile(totals, 1 - 1 / rps, names = FALSE))
}

#' Everything the compare tab keeps for one run
#'
#' Metrics come from summarise_simulation; only the return-period curve is kept for the
#' chart, not the simulated totals, so six stored runs stay small.
#' @param run A last_run value: list(id, settings, data, finished).
#' @return A list with id, name, description, modelled_label, digits, metrics and curve.
#' @noRd
sim_tab_compare_entry <- function(run) {
  summary <- summarise_simulation(run$settings, run$data)
  totals <- summary$totals
  if (is.null(totals)) totals <- run$data$total_claims
  totals <- as.numeric(totals)
  totals <- totals[!is.na(totals)]

  pick <- function(x, name) {
    value <- if (is.list(x)) x[[name]] else NULL
    if (is.null(value) || length(value) == 0) NA_real_ else as.numeric(value[1])
  }
  stats <- summary$stats
  layer <- summary$layer
  n <- if (!is.null(summary$n)) as.integer(summary$n[1]) else length(totals)

  list(
    id = run$id,
    name = paste("Run", run$id),
    description = sim_tab_describe_settings(run$settings),
    modelled_label = if (is.character(summary$modelled_label)) summary$modelled_label[1] else "Total claims",
    #headline amounts of a run share one decimal style, set by the scale of its results
    digits = if (length(totals) > 0 && max(abs(totals)) >= 1000) 0 else 2,
    metrics = list(
      n = n,
      mean = pick(stats, "mean"),
      sd = pick(stats, "sd"),
      median = pick(stats, "median"),
      var99 = pick(stats, "var99"),
      var995 = pick(stats, "var995"),
      tvar995 = pick(stats, "tvar995"),
      hit_prob = pick(layer, "hit_prob"),
      loss_on_line = pick(layer, "loss_on_line")
    ),
    curve = sim_tab_return_period_curve(totals),
    finished = run$finished
  )
}

#' Side-by-side metrics table for the compare tab
#'
#' @param entries List of compare entries (see sim_tab_compare_entry) to show as columns.
#' @param names Display names, one per entry.
#' @return An htmltools table.
#' @noRd
sim_tab_metrics_table <- function(entries, names) {
  amount_row <- function(label, key) {
    tags$tr(
      tags$td(label),
      lapply(entries, function(e) tags$td(sim_tab_fmt_amount(e$metrics[[key]], e$digits)))
    )
  }
  has_value <- function(key) any(vapply(entries, function(e) !is.na(e$metrics[[key]]), logical(1)))

  tags$table(
    class = "sim-compare-table",
    tags$thead(tags$tr(
      tags$th("Metric"),
      lapply(seq_along(entries), function(i) tags$th(
        div(class = "sim-run-name", names[i]),
        div(class = "sim-run-desc", entries[[i]]$description)
      ))
    )),
    tags$tbody(
      tags$tr(tags$td("Modelled result"), lapply(entries, function(e) tags$td(e$modelled_label))),
      tags$tr(tags$td("Simulations"), lapply(entries, function(e) tags$td(sim_tab_fmt_int(e$metrics$n)))),
      amount_row("Mean", "mean"),
      amount_row("Standard deviation", "sd"),
      amount_row("Median", "median"),
      amount_row("VaR 99%", "var99"),
      amount_row("VaR 99.5%", "var995"),
      amount_row("TVaR 99.5%", "tvar995"),
      if (has_value("hit_prob")) tags$tr(
        tags$td("Chance the layers are hit"),
        lapply(entries, function(e) tags$td(sim_tab_fmt_pct(e$metrics$hit_prob)))
      ),
      if (has_value("loss_on_line")) tags$tr(
        tags$td("Loss on line"),
        lapply(entries, function(e) tags$td(sim_tab_fmt_pct(e$metrics$loss_on_line, 2)))
      )
    )
  )
}

#' Overlaid return-period chart for the compare tab
#'
#' @param entries List of compare entries to draw.
#' @param names Display names, one per entry.
#' @param dark TRUE to use the dark theme colours.
#' @return A plotly object.
#' @noRd
sim_tab_return_period_plot <- function(entries, names, dark = FALSE) {
  #the chart stops where the smallest run stops
  max_rp <- min(vapply(entries, function(e) e$metrics$n, numeric(1))) / 10
  colours <- if (dark) {
    list(font = "#cbd5e1", muted = "#94a3b8", grid = "rgba(148, 163, 184, 0.18)",
         line = "rgba(148, 163, 184, 0.35)", marker = "#f87171")
  } else {
    list(font = "#334155", muted = "#64748b", grid = "rgba(148, 163, 184, 0.30)",
         line = "rgba(100, 116, 139, 0.45)", marker = "#dc2626")
  }
  #series colours that read well on both backgrounds
  palette <- c("#3b82f6", "#f97316", "#10b981", "#a855f7", "#ef4444", "#eab308")

  p <- plotly::plot_ly()
  for (i in seq_along(entries)) {
    curve <- entries[[i]]$curve
    curve <- curve[curve$rp <= max_rp * (1 + 1e-9), , drop = FALSE]
    if (nrow(curve) == 0) next
    p <- plotly::add_lines(
      p, x = curve$rp, y = curve$value, name = names[i],
      line = list(color = palette[(i - 1) %% length(palette) + 1], width = 2.2),
      text = paste0(names[i], "<br>1 in ", formatC(curve$rp, format = "f", digits = 1, big.mark = ","),
                    ": ", vapply(curve$value, sim_tab_fmt_amount, character(1), digits = entries[[i]]$digits)),
      hoverinfo = "text"
    )
  }

  ticks <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
  ticks <- ticks[ticks <= max_rp]
  shapes <- NULL
  annotations <- NULL
  if (200 <= max_rp) {
    #shapes on a log axis take log10 coordinates
    shapes <- list(list(type = "line", xref = "x", yref = "paper", x0 = log10(200), x1 = log10(200),
                        y0 = 0, y1 = 1, line = list(color = colours$marker, width = 1.5, dash = "dash")))
    annotations <- list(list(x = log10(200), y = 1, xref = "x", yref = "paper", text = "1 in 200",
                             showarrow = FALSE, xanchor = "left", yanchor = "top", xshift = 4,
                             font = list(color = colours$marker, size = 11)))
  }

  p <- plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = colours$font, family = "Inter, system-ui, sans-serif"),
    margin = list(l = 70, r = 20, t = 20, b = 60),
    hovermode = "closest",
    xaxis = list(
      type = "log", title = list(text = "Return period", font = list(color = colours$muted)),
      tickvals = ticks, ticktext = paste("1 in", formatC(ticks, format = "d", big.mark = ",")),
      gridcolor = colours$grid, linecolor = colours$line, tickcolor = colours$line,
      zeroline = FALSE, showline = TRUE, range = log10(c(2, max_rp * 1.02))
    ),
    yaxis = list(
      title = list(text = "Total claims", font = list(color = colours$muted)),
      tickformat = ",", gridcolor = colours$grid, linecolor = colours$line, tickcolor = colours$line,
      zerolinecolor = colours$grid, rangemode = "tozero"
    ),
    legend = list(orientation = "h", x = 0, y = 1.08, xanchor = "left", font = list(color = colours$font)),
    shapes = shapes,
    annotations = annotations
  )
  plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = list("select2d", "lasso2d", "autoScale2d"))
}

# ---------------------------------------------------------------- styles and scripts

#' Styles shared by the Report and Compare tabs
#'
#' The colours come from the --sim-* variables of the app, so both themes match.
#' @noRd
sim_tab_css <- "
.sim-tab-header {
  display: flex;
  flex-wrap: wrap;
  align-items: flex-end;
  justify-content: space-between;
  gap: 0.5rem 1rem;
  margin: 0.25rem 0 1rem 0;
}

.sim-tab-header .shiny-html-output:empty {
  display: none;
}

/* ---------- empty states ---------- */
.sim-empty-card .card-body {
  text-align: center;
  padding: 3rem 1.5rem;
}

.sim-empty-icon {
  width: 56px;
  height: 56px;
  border-radius: 16px;
  margin: 0 auto 1rem auto;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
  font-size: 22px;
}

.sim-empty-title {
  font-weight: 700;
  font-size: 1.1rem;
  color: var(--sim-heading);
}

.sim-empty-text {
  color: var(--sim-muted);
  max-width: 520px;
  margin: 0.35rem auto 1rem auto;
}

.sim-empty-card .sim-run-desc {
  margin: 0 auto 1rem auto;
  max-width: 520px;
}

/* ---------- report frame ---------- */
.sim-report-frame-wrap {
  border: 1px solid var(--sim-border);
  border-radius: var(--sim-radius);
  box-shadow: var(--sim-shadow);
  background: var(--sim-card-bg);
  overflow: hidden;
}

.sim-report-frame {
  display: block;
  width: 100%;
  min-height: 480px;
  border: 0;
  background: transparent;
}

.sim-report-building {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.6rem;
  min-height: 200px;
  color: var(--sim-muted);
  border: 1px dashed var(--sim-border-strong);
  border-radius: var(--sim-radius);
}

/* ---------- compare: stored runs ---------- */
.sim-run-list {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

.sim-run-item {
  border: 1px solid var(--sim-border);
  border-radius: 12px;
  padding: 0.7rem 0.8rem;
  background: var(--sim-card-bg);
}

.sim-run-item-top {
  display: flex;
  align-items: center;
  gap: 0.6rem;
}

.sim-run-item .shiny-input-container,
.sim-run-item .bslib-input-switch {
  margin-bottom: 0;
  width: auto;
}

.sim-run-item .sim-run-name-input {
  flex: 1 1 120px;
  min-width: 0;
}

.sim-run-item .sim-run-name-input .form-control {
  padding: 0.3rem 0.6rem;
  font-weight: 600;
}

.sim-run-item .form-switch .form-check-label {
  font-size: 0.8rem;
}

.sim-run-id {
  flex: 0 0 auto;
  font-size: 0.7rem;
  font-weight: 800;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--sim-accent-text);
  background: var(--sim-accent-soft);
  padding: 0.15rem 0.5rem;
  border-radius: 999px;
}

.sim-run-remove {
  flex: 0 0 auto;
  margin-left: auto;
  color: var(--sim-muted);
  border-color: var(--sim-border-strong);
  padding: 0.2rem 0.5rem;
}

.sim-run-remove:hover {
  color: #ffffff;
  background: #dc2626;
  border-color: #dc2626;
}

.sim-run-desc {
  font-size: 0.8rem;
  font-weight: 400;
  color: var(--sim-muted);
  margin-top: 0.35rem;
  line-height: 1.35;
}

.sim-run-list-actions {
  display: flex;
  justify-content: flex-end;
  margin-top: 0.75rem;
}

/* ---------- compare: table and chart ---------- */
.sim-compare-main {
  display: grid;
  gap: 1rem;
}

.sim-compare-table-wrap {
  overflow-x: auto;
}

.sim-compare-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.9rem;
}

.sim-compare-table th,
.sim-compare-table td {
  padding: 0.55rem 0.75rem;
  border-bottom: 1px solid var(--sim-border);
  text-align: right;
  vertical-align: bottom;
}

.sim-compare-table th {
  color: var(--sim-heading);
  font-weight: 700;
  min-width: 140px;
}

.sim-compare-table th:first-child,
.sim-compare-table td:first-child {
  text-align: left;
  min-width: 0;
}

.sim-compare-table td {
  font-variant-numeric: tabular-nums;
}

.sim-compare-table td:first-child {
  color: var(--sim-label);
  font-weight: 600;
}

.sim-compare-table tbody tr:last-child td {
  border-bottom: 0;
}

.sim-compare-table .sim-run-desc {
  font-size: 0.75rem;
}

.sim-compare-chart .js-plotly-plot .plotly .modebar-btn path {
  fill: var(--sim-muted);
}

.sim-compare-chart .shiny-output-error-validation {
  color: var(--sim-muted);
  padding: 2rem 1rem;
  text-align: center;
}

.sim-compare-note {
  display: flex;
  gap: 0.5rem;
  align-items: flex-start;
  font-size: 0.85rem;
  color: var(--sim-muted);
  padding: 0.65rem 0.8rem;
  border: 1px dashed var(--sim-border-strong);
  border-radius: 10px;
}
"

#' Script that sizes the in-app report frame and keeps its theme in step with the app
#'
#' The report is shown in a srcdoc iframe, which is same-origin, so the frame document
#' can be reached from the page. The load event of frames does not bubble but can be
#' captured at the document level, which also covers frames rendered later by shiny.
#' @noRd
sim_report_frame_js <- "
(function () {
  function appTheme() {
    return document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'dark' : 'light';
  }

  function frameDocument(frame) {
    try {
      return frame.contentDocument || (frame.contentWindow && frame.contentWindow.document) || null;
    } catch (e) {
      return null;
    }
  }

  /* the report has its own switch; inside the app it follows the app theme instead */
  function syncTheme(frame) {
    var doc = frameDocument(frame);
    if (!doc || !doc.documentElement) return;
    var theme = appTheme();
    if (doc.documentElement.getAttribute('data-theme') !== theme) {
      doc.documentElement.setAttribute('data-theme', theme);
    }
  }

  /* the frame takes the height of its content, so the page has a single scrollbar */
  function syncHeight(frame) {
    var doc = frameDocument(frame);
    if (!doc || !doc.documentElement) return;
    var height = Math.max(
      doc.documentElement.scrollHeight || 0,
      doc.body ? doc.body.scrollHeight || 0 : 0
    );
    if (height > 0) frame.style.height = (height + 2) + 'px';
  }

  function setupFrame(frame) {
    var doc = frameDocument(frame);
    if (!doc || !doc.documentElement) return;

    if (!doc.getElementById('sim-report-frame-style')) {
      var style = doc.createElement('style');
      style.id = 'sim-report-frame-style';
      style.textContent = '.theme-switch{display:none!important}';
      (doc.head || doc.documentElement).appendChild(style);
    }

    syncTheme(frame);
    syncHeight(frame);

    if (window.ResizeObserver && !frame.simResizeObserver) {
      var observer = new ResizeObserver(function () { syncHeight(frame); });
      observer.observe(doc.documentElement);
      if (doc.body) observer.observe(doc.body);
      frame.simResizeObserver = observer;
    }

    /* the frame never scrolls, so links to report sections scroll the app page instead */
    if (!frame.simLinksBound) {
      frame.simLinksBound = true;
      doc.addEventListener('click', function (event) {
        var link = event.target && event.target.closest ? event.target.closest('a[href^=\"#\"]') : null;
        if (!link) return;
        var id = link.getAttribute('href').slice(1);
        var target = id ? doc.getElementById(id) : null;
        if (!target || !target.scrollIntoView) return;
        event.preventDefault();
        target.scrollIntoView({ behavior: 'smooth', block: 'start' });
      });
    }
  }

  document.addEventListener('load', function (event) {
    var el = event.target;
    if (el && el.classList && el.classList.contains('sim-report-frame')) setupFrame(el);
  }, true);

  function syncAll() {
    var frames = document.querySelectorAll('iframe.sim-report-frame');
    for (var i = 0; i < frames.length; i++) {
      syncTheme(frames[i]);
      syncHeight(frames[i]);
    }
  }

  if (window.MutationObserver) {
    new MutationObserver(syncAll).observe(document.documentElement, {
      attributes: true,
      attributeFilter: ['data-bs-theme']
    });
  }
  window.addEventListener('resize', syncAll);
})();
"

#' Script that tells the compare module which theme the app is showing
#'
#' The chart is drawn on the server with colours for that theme, on a transparent
#' background, and redrawn when the theme changes.
#' @param input_id The namespaced id of the theme input.
#' @noRd
sim_compare_theme_js <- function(input_id) {
  paste0("
(function () {
  var inputId = '", input_id, "';
  function send() {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    var theme = document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'dark' : 'light';
    window.Shiny.setInputValue(inputId, theme);
  }
  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected shiny:sessioninitialized', send);
  }
  if (window.MutationObserver) {
    new MutationObserver(send).observe(document.documentElement, {
      attributes: true,
      attributeFilter: ['data-bs-theme']
    });
  }
  send();
})();
")
}

#' Prepare a report file for display inside the app
#'
#' Reads the HTML written by write_simulation_report and adds, at the end of its head,
#' a style that hides the report's own theme switch and a script that keeps the
#' report's data-theme equal to the app's data-bs-theme from the first paint.
#' @param file Path of the report file.
#' @return The HTML as a single string.
#' @noRd
sim_tab_report_frame_html <- function(file) {
  html <- paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  inject <- paste0(
    "<style>.theme-switch{display:none!important}</style>\n",
    "<script>(function(){",
    "function parentTheme(){try{var p=window.parent&&window.parent!==window?window.parent.document:null;",
    "return p?p.documentElement.getAttribute('data-bs-theme'):null;}catch(e){return null;}}",
    "function sync(){var t=parentTheme();if(!t)return;",
    "if(document.documentElement.getAttribute('data-theme')!==t)document.documentElement.setAttribute('data-theme',t);}",
    "sync();document.addEventListener('DOMContentLoaded',sync);",
    "if(window.MutationObserver){new MutationObserver(sync).observe(document.documentElement,{attributes:true,attributeFilter:['data-theme']});}",
    "})();</script>\n"
  )
  if (grepl("</head>", html, fixed = TRUE)) {
    sub("</head>", paste0(inject, "</head>"), html, fixed = TRUE)
  } else {
    paste0(inject, html)
  }
}

# ---------------------------------------------------------------- report tab

#' UI of the Report tab
#'
#' @param id Module id.
#' @return A \code{bslib::nav_panel} for the simulator's navbar.
#' @noRd
sim_report_tab_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Report",
    value = "report",
    icon = icon("file-lines"),
    htmltools::singleton(tags$head(tags$style(HTML(sim_tab_css)))),
    tags$script(HTML(sim_report_frame_js)),
    div(
      class = "sim-tab-header",
      div(
        h2(class = "sim-page-title", "Report"),
        p(class = "sim-page-subtitle", "The full report of the latest run, ready to read here or to download.")
      ),
      uiOutput(ns("download_ui"))
    ),
    uiOutput(ns("report_view"))
  )
}

#' Server of the Report tab
#'
#' Builds the report of the latest run with write_simulation_report, but only while the
#' tab is showing and only once per run, and shows it in a frame sized to its content.
#'
#' @param id Module id.
#' @param last_run Reactive holding NULL, or list(id, settings, data, finished) of the latest run.
#' @param navbar_input Reactive returning the selected navbar tab value.
#' @noRd
sim_report_tab_server <- function(id, last_run, navbar_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #the report of one run: list(id, file, html, error)
    built <- reactiveVal(NULL)

    #remove the temporary report file when the session ends
    session$onSessionEnded(function() {
      current <- isolate(built())
      if (!is.null(current$file)) unlink(current$file)
    })

    build_report <- function(run) {
      file <- tempfile("simulation_report_", fileext = ".html")
      shiny::withProgress(message = "Building report", value = 0.3, detail = "Preparing charts and tables", {
        #the report is assembled in R (see write_simulation_report), so no pandoc is needed
        write_simulation_report(file = file, settings = run$settings, results = run$data)
        incProgress(0.7, detail = "Done")
      })
      list(id = run$id, file = file, html = sim_tab_report_frame_html(file), error = NULL)
    }

    #build lazily: only while the tab is showing and only once per run
    observe({
      run <- last_run()
      if (is.null(run) || is.null(run$id)) return(NULL)
      if (!identical(navbar_input(), "report")) return(NULL)
      current <- isolate(built())
      if (!is.null(current) && identical(current$id, run$id)) return(NULL)

      result <- tryCatch(
        build_report(run),
        error = function(cond) {
          showNotification(
            paste("The report could not be created:", conditionMessage(cond)),
            type = "error",
            duration = NULL
          )
          list(id = run$id, file = NULL, html = NULL, error = conditionMessage(cond))
        }
      )
      if (!is.null(current$file)) unlink(current$file)
      built(result)
    })

    output$report_view <- renderUI({
      run <- last_run()
      if (is.null(run)) {
        return(sim_tab_empty_state(
          "file-lines", "No report yet",
          "Run a simulation to see its report here.",
          sim_tab_open_simulator_button()
        ))
      }
      current <- built()
      if (is.null(current) || !identical(current$id, run$id)) {
        return(div(
          class = "sim-report-building",
          tags$span(class = "spinner-border spinner-border-sm", role = "status", `aria-hidden` = "true"),
          "Building the report..."
        ))
      }
      if (!is.null(current$error)) {
        return(sim_tab_empty_state(
          "triangle-exclamation", "The report could not be created", current$error
        ))
      }
      div(
        class = "sim-report-frame-wrap",
        tags$iframe(
          class = "sim-report-frame",
          title = "Simulation report",
          srcdoc = current$html
        )
      )
    })

    output$download_ui <- renderUI({
      req(last_run())
      downloadButton(ns("download_report"), "Download report", icon = icon("download"), class = "btn-outline-primary")
    })
    #the empty container has no size, so Shiny would treat it as hidden and never render the button
    outputOptions(output, "download_ui", suspendWhenHidden = FALSE)

    output$download_report <- downloadHandler(
      filename = "simulation_report.html",
      content = function(file) {
        run <- isolate(last_run())
        req(run)
        showNotification(
          "Preparing report, save dialog will appear shortly...",
          type = "message",
          duration = 3
        )
        #the report shown in the tab is the same file, so reuse it when it is current
        current <- isolate(built())
        if (!is.null(current) && identical(current$id, run$id) &&
            !is.null(current$file) && file.exists(current$file)) {
          file.copy(current$file, file, overwrite = TRUE)
          return(invisible(file))
        }
        tryCatch(
          write_simulation_report(file = file, settings = run$settings, results = run$data),
          error = function(cond) {
            #without this the download just fails, with the reason only in the R console
            showNotification(
              paste("The report could not be created:", conditionMessage(cond)),
              type = "error",
              duration = NULL
            )
            stop(cond)
          }
        )
      }
    )
  })
}

# ---------------------------------------------------------------- compare tab

#' UI of the Compare tab
#'
#' @param id Module id.
#' @return A \code{bslib::nav_panel} for the simulator's navbar.
#' @noRd
sim_compare_tab_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Compare",
    value = "compare",
    icon = icon("scale-balanced"),
    htmltools::singleton(tags$head(tags$style(HTML(sim_tab_css)))),
    tags$script(HTML(sim_compare_theme_js(ns("app_theme")))),
    div(
      class = "sim-tab-header",
      div(
        h2(class = "sim-page-title", "Compare runs"),
        p(class = "sim-page-subtitle", textOutput(ns("stored_note"), inline = TRUE))
      )
    ),
    uiOutput(ns("empty_state")),
    conditionalPanel(
      condition = "output.has_comparison",
      ns = ns,
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8), xxl = c(3, 9)),

        bslib::card(
          sim_card_header("list-check", "Stored runs", "Tick the runs to compare, rename or remove them."),
          bslib::card_body(
            uiOutput(ns("run_list")),
            div(
              class = "sim-run-list-actions",
              actionButton(ns("clear_all"), "Clear all", icon = icon("trash-can"), class = "btn-outline-secondary btn-sm")
            )
          )
        ),

        div(
          class = "sim-compare-main",
          bslib::card(
            sim_card_header("table-list", "Metrics side by side", "One column per included run."),
            bslib::card_body(div(class = "sim-compare-table-wrap", uiOutput(ns("metrics_table"))))
          ),
          bslib::card(
            sim_card_header("chart-line", "Losses by return period",
                            "Loss exceeded once in the given number of periods, up to a tenth of the smallest run."),
            bslib::card_body(
              div(class = "sim-compare-chart", plotly::plotlyOutput(ns("return_period_chart"), height = "400px"))
            )
          ),
          div(
            class = "sim-compare-note",
            icon("circle-info"),
            tags$span(
              "Results differ between runs partly because of simulation noise, ",
              "unless the runs use the same custom seed. Differences at long return periods are the noisiest."
            )
          )
        )
      )
    )
  )
}

#' Server of the Compare tab
#'
#' Stores the latest runs of the session (up to \code{max_runs}) with their metrics,
#' computed once each with summarise_simulation, and compares the included runs.
#'
#' @param id Module id.
#' @param last_run Reactive holding NULL, or list(id, settings, data, finished) of the latest run.
#' @param max_runs Number of runs kept; the oldest is dropped beyond it.
#' @param default_included Number of latest runs included by default.
#' @noRd
sim_compare_tab_server <- function(id, last_run, max_runs = 6, default_included = 3) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #stored runs in the order they were added (see sim_tab_compare_entry)
    runs <- reactiveVal(list())
    #per-run state keyed by run id: display name, included flag, and whether the user
    #chose the included flag (an automatic default may only change untouched runs)
    state <- reactiveValues(name = list(), include = list(), touched = list())
    #observers that copy each run's inputs into the state, destroyed with the run
    handles <- new.env(parent = emptyenv())

    run_ids <- function(entries) vapply(entries, function(e) as.numeric(e$id), numeric(1))

    drop_runs <- function(ids) {
      if (length(ids) == 0) return(invisible(NULL))
      for (key in as.character(ids)) {
        if (!is.null(handles[[key]])) {
          lapply(handles[[key]], function(h) h$destroy())
          rm(list = key, envir = handles)
        }
        state$name[[key]] <- NULL
        state$include[[key]] <- NULL
        state$touched[[key]] <- NULL
      }
      current <- runs()
      runs(current[!(run_ids(current) %in% ids)])
    }

    watch_inputs <- function(id) {
      key <- as.character(id)
      include_id <- paste0("include_", id)
      name_id <- paste0("name_", id)
      handles[[key]] <- list(
        observe({
          value <- input[[include_id]]
          if (is.null(value)) return(NULL)
          isolate({
            #a value that already matches is the browser showing the state, not a choice
            if (!identical(isTRUE(value), isTRUE(state$include[[key]]))) {
              state$include[[key]] <- isTRUE(value)
              state$touched[[key]] <- TRUE
            }
          })
        }),
        observe({
          value <- input[[name_id]]
          if (is.null(value)) return(NULL)
          isolate({
            if (!identical(value, state$name[[key]])) state$name[[key]] <- value
          })
        })
      )
    }

    #store every new run once, with its metrics
    observeEvent(last_run(), {
      run <- last_run()
      if (is.null(run$id) || is.null(run$data)) return(NULL)
      current <- runs()
      if (as.numeric(run$id) %in% run_ids(current)) return(NULL)

      entry <- tryCatch(
        sim_tab_compare_entry(run),
        error = function(cond) {
          showNotification(
            paste("The run could not be added to the comparison:", conditionMessage(cond)),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )
      if (is.null(entry)) return(NULL)

      key <- as.character(run$id)
      state$name[[key]] <- entry$name
      state$include[[key]] <- TRUE
      state$touched[[key]] <- FALSE
      watch_inputs(run$id)

      current <- c(current, list(entry))
      #drop the oldest beyond the limit
      if (length(current) > max_runs) {
        dropped <- run_ids(current)[seq_len(length(current) - max_runs)]
        drop_runs(dropped)
        current <- current[!(run_ids(current) %in% dropped)]
      }
      #by default only the latest few runs are included; runs the user has chosen keep their choice
      ids <- run_ids(current)
      older <- ids[seq_len(max(0, length(ids) - default_included))]
      for (old_key in as.character(older)) {
        if (!isTRUE(state$touched[[old_key]])) state$include[[old_key]] <- FALSE
      }
      runs(current)
    })

    observeEvent(input$remove, {
      id <- suppressWarnings(as.numeric(input$remove))
      if (length(id) == 1 && !is.na(id)) drop_runs(id)
    })

    observeEvent(input$clear_all, {
      drop_runs(run_ids(runs()))
    })

    display_name <- function(entry) {
      name <- state$name[[as.character(entry$id)]]
      if (is.null(name) || !nzchar(trimws(name))) entry$name else trimws(name)
    }

    included <- reactive({
      Filter(function(e) isTRUE(state$include[[as.character(e$id)]]), runs())
    })

    included_names <- reactive({
      vapply(included(), display_name, character(1))
    })

    #renaming re-draws the chart, so wait for typing to pause
    chart_inputs <- debounce(reactive(list(entries = included(), names = included_names())), 300)

    #a plain output function (no render wrapper) sends a bare TRUE/FALSE to the conditionalPanel
    output$has_comparison <- function(shinysession, name, ...) {
      length(runs()) >= 2
    }
    outputOptions(output, "has_comparison", suspendWhenHidden = FALSE)

    output$stored_note <- renderText({
      count <- length(runs())
      if (count == 0) {
        paste0("Every run is stored here (up to ", max_runs, ") so its results can be compared.")
      } else {
        paste0(count, " of up to ", max_runs, " runs stored. Older runs are dropped as new ones are added.")
      }
    })

    output$empty_state <- renderUI({
      current <- runs()
      if (length(current) >= 2) return(NULL)
      if (length(current) == 0) {
        sim_tab_empty_state(
          "scale-balanced", "Nothing to compare yet",
          paste0("Each simulation run is stored in this tab (up to ", max_runs, " runs). ",
                 "Comparisons appear from the second run."),
          sim_tab_open_simulator_button()
        )
      } else {
        sim_tab_empty_state(
          "scale-balanced", "One run stored",
          "The comparison appears once a second run has finished. Change the settings and run again.",
          div(class = "sim-run-desc", paste0(display_name(current[[1]]), ": ", current[[1]]$description)),
          sim_tab_open_simulator_button()
        )
      }
    })

    output$run_list <- renderUI({
      current <- runs()
      if (length(current) == 0) return(NULL)
      div(
        class = "sim-run-list",
        lapply(current, function(entry) {
          key <- as.character(entry$id)
          div(
            class = "sim-run-item",
            div(
              class = "sim-run-item-top",
              tags$span(class = "sim-run-id", paste("Run", entry$id)),
              div(
                class = "sim-run-name-input",
                textInput(ns(paste0("name_", entry$id)), label = NULL,
                          value = isolate(state$name[[key]]), placeholder = entry$name)
              ),
              tags$button(
                type = "button",
                class = "btn btn-sm btn-outline-secondary sim-run-remove",
                title = "Remove this run",
                `aria-label` = paste("Remove run", entry$id),
                onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'});", ns("remove"), entry$id),
                icon("xmark")
              )
            ),
            div(class = "sim-run-desc", entry$description),
            div(
              class = "mt-2",
              bslib::input_switch(ns(paste0("include_", entry$id)), "Include in comparison",
                                  value = isTRUE(isolate(state$include[[key]])))
            )
          )
        })
      )
    })

    output$metrics_table <- renderUI({
      entries <- included()
      if (length(entries) == 0) {
        return(p(class = "sim-muted mb-0", "Include at least one run to see its metrics."))
      }
      sim_tab_metrics_table(entries, included_names())
    })

    output$return_period_chart <- plotly::renderPlotly({
      inputs <- chart_inputs()
      validate(need(length(inputs$entries) > 0, "Include at least one run to draw the chart."))
      max_rp <- min(vapply(inputs$entries, function(e) e$metrics$n, numeric(1))) / 10
      validate(need(max_rp >= 2, "The chart needs runs of at least 20 simulations."))
      sim_tab_return_period_plot(inputs$entries, inputs$names, dark = identical(input$app_theme, "dark"))
    })
  })
}
