#' Styles for the distribution fitting tool
#'
#' Added after the simulator styles (sim_ui_css), whose cards, inputs, theme
#' switch and light / dark tokens the tool shares; this block gives the tool its
#' violet accent and styles the parts only it has.
#' @noRd
dft_ui_css <- "
:root, [data-bs-theme='light'] {
  --dft-accent: #7c3aed;
  --dft-accent-strong: #6d28d9;
  --dft-accent-shadow: rgba(124, 58, 237, 0.28);
  --sim-accent-soft: #f5f3ff;
  --sim-accent-text: #6d28d9;
  --dft-code-bg: #f8fafc;
  --dft-row-hover: #faf8ff;
  --dft-best-bg: #ecfdf5;
  --dft-best-text: #047857;
}

[data-bs-theme='dark'] {
  --dft-accent: #8b5cf6;
  --dft-accent-strong: #7c3aed;
  --dft-accent-shadow: rgba(139, 92, 246, 0.30);
  --sim-accent-soft: rgba(139, 92, 246, 0.18);
  --sim-accent-text: #c4b5fd;
  --dft-code-bg: #0d1526;
  --dft-row-hover: rgba(139, 92, 246, 0.08);
  --dft-best-bg: rgba(16, 185, 129, 0.16);
  --dft-best-text: #6ee7b7;
  --bs-link-color: #c4b5fd;
  --bs-link-color-rgb: 196, 181, 253;
  --bs-link-hover-color: #ddd6fe;
  --bs-link-hover-color-rgb: 221, 214, 254;
}

/* ---------- Accent ---------- */
.navbar {
  background: linear-gradient(90deg, #1e1b4b 0%, #3b1f78 100%) !important;
}

.sim-brand-mark {
  background: linear-gradient(135deg, #a78bfa, #7c3aed);
  box-shadow: 0 6px 18px rgba(124, 58, 237, 0.35);
}

.theme-btn.active {
  color: #1e1b4b;
}

.theme-btn:focus-visible,
.pill-radio .radio-inline:has(input:focus-visible) {
  outline-color: #c4b5fd !important;
}

.form-control:focus, .form-select:focus, .selectize-input.focus {
  border-color: var(--dft-accent) !important;
  box-shadow: 0 0 0 0.2rem rgba(124, 58, 237, 0.18) !important;
}

.pill-radio .radio-inline:hover {
  border-color: var(--dft-accent);
}

@supports selector(:has(*)) {
  .pill-radio .radio-inline:has(input:checked) {
    background: var(--dft-accent);
    border-color: var(--dft-accent);
    box-shadow: 0 4px 12px var(--dft-accent-shadow);
  }
}

.btn-run {
  background: linear-gradient(135deg, #7c3aed, #6d28d9);
  box-shadow: 0 8px 20px var(--dft-accent-shadow);
}

.btn-run:hover, .btn-run:focus {
  background: linear-gradient(135deg, #6d28d9, #5b21b6);
}

.sim-hero {
  background:
    radial-gradient(circle at 85% 20%, rgba(167, 139, 250, 0.38), transparent 45%),
    linear-gradient(135deg, #1e1b4b 0%, #3b1f78 55%, #7c3aed 130%);
}

.sim-hero-eyebrow {
  color: #c4b5fd;
}

.sim-hero .btn-light {
  color: #1e1b4b;
}

.form-check-input:checked {
  background-color: var(--dft-accent);
  border-color: var(--dft-accent);
}

.form-check-input:focus {
  border-color: var(--dft-accent);
  box-shadow: 0 0 0 0.2rem rgba(124, 58, 237, 0.18);
}

/* ---------- Settings panel ---------- */
.dft-help {
  display: flex;
  gap: 0.5rem;
  align-items: flex-start;
  font-size: 0.82rem;
  color: var(--sim-muted);
  margin: 0.6rem 0 0 0;
}

.dft-help .fa, .dft-help svg {
  margin-top: 0.15rem;
  flex: 0 0 auto;
}

.dft-settings .shiny-input-container {
  margin-bottom: 0.9rem;
}

.dft-settings .pill-radio .shiny-input-container {
  margin-bottom: 0.6rem;
}

.dft-settings .sim-section-label {
  margin-bottom: 0.45rem;
}

.dft-settings .shiny-input-container:has(> .input-group) {
  margin-bottom: 0.6rem;
}

.dft-settings .shiny-file-input-progress {
  margin-bottom: 0;
}

.dft-settings .btn-run {
  margin-top: 0.25rem;
}

.dft-sliders:empty {
  display: none;
}

/* file input */
.input-group .btn-file {
  border-radius: 10px 0 0 10px;
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
  border-color: var(--sim-border-strong);
}

.input-group .form-control[readonly] {
  background: var(--sim-input-bg);
}

.shiny-file-input-progress .progress-bar {
  background-color: var(--dft-accent);
}

/* sliders */
.irs--shiny .irs-bar {
  background: var(--dft-accent);
  border-color: var(--dft-accent);
}

.irs--shiny .irs-single,
.irs--shiny .irs-from,
.irs--shiny .irs-to {
  background: var(--dft-accent);
}

.irs--shiny .irs-handle {
  border-color: var(--dft-accent);
  background: var(--sim-card-bg);
}

.irs--shiny .irs-line {
  background: var(--sim-border-strong);
  border-color: var(--sim-border-strong);
}

.irs--shiny .irs-min,
.irs--shiny .irs-max {
  background: var(--bs-secondary-bg, #e9ecef);
  color: var(--sim-muted);
}

.irs--shiny .irs-grid-text {
  color: var(--sim-muted);
}

.irs--shiny .irs-grid-pol {
  background: var(--sim-border-strong);
}

/* ---------- Results ---------- */
.dft-results {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  min-width: 0;
}

.dft-results > .card,
.dft-results > .shiny-html-output > .card {
  margin-bottom: 0;
}

/* the summary tiles take no space until they have content (the server renders
   them while hidden); outputs inside cards are never hidden */
.dft-results > .shiny-html-output:empty {
  display: none;
}

.dft-stats {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  gap: 0.75rem;
}

.dft-stat {
  background: var(--sim-card-bg);
  border: 1px solid var(--sim-border);
  border-radius: 14px;
  box-shadow: var(--sim-shadow);
  padding: 0.85rem 1rem;
  min-width: 0;
}

.dft-stat-label {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.7rem;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: var(--sim-muted);
  margin-bottom: 0.3rem;
}

.dft-stat-value {
  font-size: 1.3rem;
  font-weight: 800;
  letter-spacing: -0.01em;
  color: var(--sim-heading);
  line-height: 1.25;
  font-variant-numeric: tabular-nums;
  overflow-wrap: anywhere;
}

.dft-stat-note {
  font-size: 0.78rem;
  color: var(--sim-muted);
  margin-top: 0.15rem;
}

.dft-stat-accent {
  background: var(--sim-accent-soft);
}

.dft-stat-accent .dft-stat-label {
  color: var(--sim-accent-text);
}

/* tabbed result cards */
.dft-tabs > .card-header {
  background: var(--sim-card-header-bg);
  border-bottom: 1px solid var(--sim-border);
  border-top-left-radius: var(--sim-radius) !important;
  border-top-right-radius: var(--sim-radius) !important;
  padding: 0.35rem 1.1rem 0 1.1rem;
}

.dft-tabs .nav-underline {
  gap: 1.25rem;
  --bs-nav-underline-border-width: 2px;
}

.dft-tabs .nav-underline .nav-link {
  color: var(--sim-muted);
  font-weight: 600;
  font-size: 0.9rem;
  padding: 0.7rem 0;
}

.dft-tabs .nav-underline .nav-link:hover {
  color: var(--sim-heading);
}

.dft-tabs .nav-underline .nav-link.active {
  color: var(--sim-accent-text);
  border-bottom-color: var(--dft-accent);
}

.dft-plot .js-plotly-plot .plotly .modebar-btn path {
  fill: var(--sim-muted);
}

.dft-plot .js-plotly-plot .plotly .modebar {
  background: transparent !important;
}

/* messages shown in place of an output before an analysis has run */
.shiny-output-error-validation {
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  min-height: 7rem;
  color: var(--sim-muted);
  padding: 1.5rem 1rem;
  border: 1px dashed var(--sim-border-strong);
  border-radius: 12px;
  font-size: 0.9rem;
}

.dft-plot .shiny-output-error-validation {
  min-height: 380px;
}

/* tables */
.dft-table-wrap {
  overflow-x: auto;
}

.dft-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.9rem;
}

.dft-table th {
  font-size: 0.7rem;
  font-weight: 800;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--sim-muted);
  text-align: right;
  padding: 0.5rem 0.75rem;
  border-bottom: 1px solid var(--sim-border-strong);
  white-space: nowrap;
}

.dft-table td {
  text-align: right;
  padding: 0.55rem 0.75rem;
  border-bottom: 1px solid var(--sim-border);
  font-variant-numeric: tabular-nums;
  color: var(--bs-body-color);
  white-space: nowrap;
}

.dft-table th:first-child,
.dft-table td:first-child {
  text-align: left;
}

.dft-table td:first-child {
  font-weight: 700;
  color: var(--sim-heading);
}

.dft-table tbody tr:hover td {
  background: var(--dft-row-hover);
}

.dft-table tbody tr:last-child td {
  border-bottom: 0;
}

.dft-table .dft-param-name {
  color: var(--sim-muted);
  font-size: 0.78rem;
  margin-right: 0.3rem;
}

.dft-table .dft-na {
  color: var(--sim-muted);
}

.dft-badge {
  display: inline-block;
  margin-left: 0.5rem;
  padding: 0.1rem 0.5rem;
  border-radius: 999px;
  font-size: 0.68rem;
  font-weight: 800;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  background: var(--dft-best-bg);
  color: var(--dft-best-text);
  vertical-align: 0.1em;
}

.dft-table-note {
  font-size: 0.8rem;
  color: var(--sim-muted);
  margin: 0.75rem 0 0 0;
}

/* printed R output */
.dft-results pre,
.dft-results .shiny-text-output {
  background: var(--dft-code-bg);
  color: var(--bs-body-color);
  border: 1px solid var(--sim-border);
  border-radius: 12px;
  padding: 0.9rem 1rem;
  font-size: 0.8rem;
  margin: 0;
  white-space: pre;
  overflow-x: auto;
}

/* data preview */
.dft-file-name {
  font-size: 1rem;
  overflow-wrap: anywhere;
}

.reactable {
  font-size: 0.88rem;
  border-radius: 10px;
}

/* ---------- Welcome ---------- */
.dft-feature-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.dft-feature-list li {
  display: flex;
  gap: 0.6rem;
  align-items: baseline;
  padding: 0.35rem 0;
  color: var(--sim-label);
}

.dft-feature-list li .fa, .dft-feature-list li svg {
  color: var(--sim-accent-text);
}
"

#' Settings card helper for the analysis tabs of the distribution fitting tool
#'
#' @param icon_name Font Awesome icon name.
#' @param title Card title.
#' @param subtitle Short description shown under the title.
#' @param ... Card body contents.
#' @noRd
dft_settings_card <- function(icon_name, title, subtitle, ...) {
  bslib::card(
    class = "dft-settings",
    sim_card_header(icon_name, title, subtitle),
    bslib::card_body(...)
  )
}

#' Muted help line with an icon
#'
#' @noRd
dft_help <- function(...) {
  div(class = "dft-help", icon("circle-info"), div(...))
}

#' Two-column page layout: settings on the left, results on the right
#'
#' @noRd
dft_layout <- function(settings, ...) {
  bslib::layout_columns(
    class = "sim-layout",
    col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8), xxl = c(3, 9)),
    settings,
    div(class = "dft-results", ...)
  )
}

#' Page title and subtitle
#'
#' @noRd
dft_page_intro <- function(title, subtitle) {
  div(
    class = "sim-page-intro",
    div(
      h2(class = "sim-page-title", title),
      p(class = "sim-page-subtitle", subtitle)
    )
  )
}

#' Plot output with the tool's styling
#'
#' @noRd
dft_plot_output <- function(id, height = "440px") {
  div(class = "dft-plot", plotly::plotlyOutput(id, height = height))
}

#' Card with underlined tabs for the results of an analysis
#'
#' @noRd
dft_tabs <- function(...) {
  tagAppendAttributes(bslib::navset_card_underline(...), class = "dft-tabs")
}

#' Column selector, empty until a file is uploaded
#'
#' @noRd
dft_column_select <- function(id, label) {
  selectizeInput(id, label, choices = NULL,
                 options = list(placeholder = "Upload a data file first"))
}

#' Run button for an analysis tab
#'
#' @noRd
dft_run_button <- function(id) {
  actionButton(id, "Run analysis", icon = icon("play"), class = "btn-run")
}

#' Chart options switch for a logarithmic claim size axis
#'
#' On by default: claim sizes are heavy tailed, and on a linear axis the fitted
#' curves are squeezed against the left edge.
#' @noRd
dft_log_switch <- function(id) {
  bslib::input_switch(id, "Log scale for claim size", value = TRUE)
}

#' User interface of the Shiny distribution fitting tool
#'
#' The page is built when the package is installed;
#' run_shiny_distribution_fitting_tool() pairs it with
#' distribution_fitting_tool_Server.
#' @return The user interface of the application, a bslib navbar page.
distribution_fitting_tool_UI <- bslib::page_navbar(
  title = div(
    class = "sim-brand",
    div(class = "sim-brand-mark", icon("chart-area")),
    div(
      class = "sim-brand-text",
      tags$span(class = "sim-brand-title", "NetDisFit"),
      tags$span(class = "sim-brand-subtitle", "Distribution fitting by NetSimR")
    )
  ),
  window_title = "NetSim Distribution Fitting Tool",
  id = "dft_navbar",
  selected = "welcome",
  fillable = FALSE,

  navbar_options = bslib::navbar_options(
    bg = "#1e1b4b",
    theme = "dark",
    underline = FALSE
  ),

  theme = bslib::bs_theme(
    version = 5,
    primary = "#7c3aed",
    secondary = "#64748b",
    success = "#16a34a",
    info = "#0891b2",
    warning = "#d97706",
    danger = "#dc2626",
    base_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    )
  ),

  header = tagList(
    tags$head(
      tags$script(HTML(netsimr_theme_js("netsimr-distribution-fitting-theme"))),
      tags$style(HTML(sim_ui_css)),
      tags$style(HTML(dft_ui_css))
    ),
    #tells the server which theme is showing, so the charts are drawn in matching colours
    tags$script(HTML(sim_compare_theme_js("app_theme"))),
    useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
    busyIndicatorOptions(
      pulse_background = "linear-gradient(90deg, #a78bfa, #7c3aed, #a78bfa)",
      pulse_height = "4px"
    )
  ),

  # ---------------------------------------------------------------- Welcome
  bslib::nav_panel(
    title = "Welcome",
    value = "welcome",
    icon = icon("house"),

    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),

      div(
        class = "sim-hero",
        div(class = "sim-hero-eyebrow", "NetDisFit"),
        div(class = "sim-hero-title", "Distribution Fitting Tool"),
        p(
          class = "sim-hero-text",
          "Upload claims data, fit frequency and severity distributions, ",
          "compare them against the data and splice Pareto tails onto the ",
          "severity fit, ready to use in the NetSimR simulator."
        ),
        tags$button(
          type = "button",
          class = "btn btn-light",
          onclick = "document.querySelector('.navbar a[data-value=\"data\"]').click();",
          icon("upload"), " Upload data"
        )
      ),

      bslib::card(
        sim_card_header("route", "How it works"),
        bslib::card_body(
          tags$ol(
            class = "sim-steps",
            tags$li(div(div(class = "sim-step-title", "Upload"),
                        div(class = "sim-step-text", "A CSV or text file with one row per claim or period."))),
            tags$li(div(div(class = "sim-step-title", "Choose a column"),
                        div(class = "sim-step-text", "Pick the claim counts or claim sizes to fit."))),
            tags$li(div(div(class = "sim-step-title", "Run the analysis"),
                        div(class = "sim-step-text", "Fitted parameters and goodness of fit appear at once."))),
            tags$li(div(div(class = "sim-step-title", "Compare"),
                        div(class = "sim-step-text", "Check each fit against the empirical distribution.")))
          )
        )
      )
    ),

    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
      div(
        class = "sim-stat",
        div(class = "sim-stat-label", icon("chart-column"), "Frequency models"),
        div(class = "sim-stat-value", paste("Poisson", "Negative Binomial", sep = paste0(" ", intToUtf8(183), " ")))
      ),
      div(
        class = "sim-stat",
        div(class = "sim-stat-label", icon("chart-line"), "Severity models"),
        div(class = "sim-stat-value",
            paste(c("Normal", "LogNormal", "Exponential", "Gamma", "Pareto"), collapse = paste0(" ", intToUtf8(183), " ")))
      ),
      div(
        class = "sim-stat",
        div(class = "sim-stat-label", icon("scissors"), "Tail models"),
        div(class = "sim-stat-value",
            paste(c("Sliced LogNormal-Pareto", "Piecewise Pareto"), collapse = paste0(" ", intToUtf8(183), " ")))
      )
    ),

    bslib::card(
      sim_card_header("user", "About", "Feedback and bug reports are very welcome."),
      bslib::card_body(
        p(
          "Created by Yiannis Parizas. For more information, visit my ",
          a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/",
            target = "_blank", rel = "noopener noreferrer"),
          ". You can also reach me by email at ",
          a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com"),
          "."
        ),
        p(class = "sim-muted mb-0",
          "Please reach out if you have any feedback or encounter any bugs.")
      )
    )
  ),

  # ------------------------------------------------------------------- Data
  bslib::nav_panel(
    title = "Data",
    value = "data",
    icon = icon("table"),

    dft_page_intro("Data", "Upload a CSV or text file. Every analysis tab reads its columns from this file."),

    dft_layout(
      dft_settings_card(
        "file-arrow-up", "Data file", "Comma, semicolon or tab separated.",
        fileInput("file1", "File", accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv", ".txt", ".tsv"
        ), placeholder = "No file selected"),
        bslib::input_switch("data_includes_header", "First row holds column names", value = TRUE),
        div(class = "sim-section-label mt-2", "Separator"),
        div(class = "pill-radio",
            radioButtons("sep", NULL, c(Comma = ",", Semicolon = ";", Tab = "\t"), ",", inline = TRUE)),
        div(class = "sim-section-label mt-2", "Decimal mark"),
        div(class = "pill-radio",
            radioButtons("dec", NULL, c("Point (1.5)" = ".", "Comma (1,5)" = ","), ".", inline = TRUE)),
        div(class = "sim-section-label mt-2", "Quote"),
        div(class = "pill-radio",
            radioButtons("quote", NULL, c("Double" = '"', "Single" = "'", "None" = ""), '"', inline = TRUE))
      ),
      uiOutput("data_overview"),
      bslib::card(
        sim_card_header("table-list", "Preview", "Search, sort and page through the uploaded data."),
        bslib::card_body(
          reactable::reactableOutput("data_table")
        )
      )
    )
  ),

  # -------------------------------------------------------------- Frequency
  bslib::nav_panel(
    title = "Frequency",
    value = "frequency",
    icon = icon("chart-column"),

    dft_page_intro("Frequency analysis", "Fit Poisson and Negative Binomial distributions to claim counts."),

    dft_layout(
      dft_settings_card(
        "sliders", "Settings", "Choose the claim counts and run the fit.",
        dft_column_select("counts_var", "Claim counts column"),
        bslib::input_switch("counts_weighted_var", "Weighted fit", value = FALSE),
        conditionalPanel(
          condition = "input.counts_weighted_var",
          dft_column_select("counts_weights_var", "Weights column")
        ),
        dft_run_button("execute_freq_analysis"),
        dft_help("Rows with a missing, negative or non-numeric count (or weight) are left out."),
        tags$hr(class = "sim-divider"),
        div(class = "sim-section-label", "Chart options"),
        sliderInput("count_hist_bins", "Histogram bins", min = 1, max = 100, value = 20)
      ),
      uiOutput("freq_stats"),
      bslib::card(
        sim_card_header("table-cells", "Fitted distributions",
                        "Maximum likelihood fits; the lower the AIC, the better the fit."),
        bslib::card_body(uiOutput("selected_freq_params"))
      ),
      dft_tabs(
        id = "freq_results_tabs",
        bslib::nav_panel("CDF fit", dft_plot_output("freq_fit_plot")),
        bslib::nav_panel("Histogram", dft_plot_output("count_hist")),
        bslib::nav_panel(
          "Fit details",
          div(class = "pill-radio mb-3",
              radioButtons("FreqDistri", NULL,
                           c("Poisson" = "Poisson", "Negative Binomial" = "NegativeBinomial"),
                           inline = TRUE)),
          verbatimTextOutput("selected_distribution_summary")
        ),
        bslib::nav_panel("Data summary", verbatimTextOutput("freq_summary"))
      )
    )
  ),

  # --------------------------------------------------------------- Severity
  bslib::nav_panel(
    title = "Severity",
    value = "severity",
    icon = icon("chart-line"),

    dft_page_intro("Severity analysis", "Fit Normal, LogNormal, Exponential, Gamma and Pareto distributions to claim sizes."),

    dft_layout(
      dft_settings_card(
        "sliders", "Settings", "Choose the claim sizes and run the fit.",
        dft_column_select("severity_var", "Claim size column"),
        dft_run_button("execute_sev_analysis"),
        dft_help("Missing, negative, zero and non-numeric values are left out."),
        tags$hr(class = "sim-divider"),
        div(class = "sim-section-label", "Chart options"),
        dft_log_switch("sev_fit_log_scale"),
        sliderInput("severity_hist_bins", "Histogram bins", min = 1, max = 200, value = 20)
      ),
      uiOutput("sev_stats"),
      bslib::card(
        sim_card_header("table-cells", "Fitted distributions",
                        "The lower the Kolmogorov-Smirnov distance, the closer the fit."),
        bslib::card_body(uiOutput("sev_param_summary"))
      ),
      dft_tabs(
        id = "sev_results_tabs",
        bslib::nav_panel("CDF fit", dft_plot_output("sev_fit_plot")),
        bslib::nav_panel("Histogram", dft_plot_output("sev_hist")),
        bslib::nav_panel("Data summary", verbatimTextOutput("sev_summary"))
      )
    )
  ),

  # -------------------------------------------------------- Sliced severity
  bslib::nav_panel(
    title = "Sliced severity",
    value = "sliced",
    icon = icon("scissors"),

    dft_page_intro("Sliced severity analysis",
                   "Splice one or two Pareto tails onto a LogNormal body at chosen slicing points."),

    dft_layout(
      dft_settings_card(
        "sliders", "Settings", "Choose the claim sizes and the slicing points.",
        dft_column_select("sliced_sev_var", "Claim size column"),
        dft_run_button("execute_sliced_sev_analysis"),
        dft_help("Missing, negative, zero and non-numeric values are left out."),
        tags$hr(class = "sim-divider"),
        div(class = "sim-section-label", "Slicing points"),
        sliderInput("slicing_point_left", "First slicing point", min = 0, max = 10, value = 5),
        sliderInput("slicing_point_right", "Second slicing point", min = 0, max = 20, value = 10),
        dft_help("The sliders cover the range of the data once the analysis has run. ",
                 "The second point must be above the first."),
        tags$hr(class = "sim-divider"),
        div(class = "sim-section-label", "Chart options"),
        dft_log_switch("sev_cens_fit_log_scale")
      ),
      bslib::card(
        sim_card_header("table-cells", "Fitted distributions",
                        "Body and tail parameters, with the Kolmogorov-Smirnov distance of each model."),
        bslib::card_body(uiOutput("slc_sev_fitted_param_summary"))
      ),
      dft_tabs(
        id = "sliced_results_tabs",
        bslib::nav_panel("CDF fit", dft_plot_output("sliced_sev_cdf_plot")),
        bslib::nav_panel("Mean excess", dft_plot_output("mean_excess_func_plot"))
      )
    )
  ),

  # ------------------------------------------------------- Piecewise Pareto
  bslib::nav_panel(
    title = "Piecewise Pareto",
    value = "piecewise",
    icon = icon("layer-group"),

    dft_page_intro("Piecewise Pareto fit",
                   "Fit a Pareto alpha between each pair of thresholds, from the smallest claim upwards."),

    dft_layout(
      dft_settings_card(
        "sliders", "Settings", "Choose the claim sizes and the thresholds.",
        dft_column_select("piecewise_pareto_var", "Claim size column"),
        dft_run_button("execute_piecewise_sev_analysis"),
        dft_help("Missing, negative, zero and non-numeric values are left out."),
        tags$hr(class = "sim-divider"),
        div(class = "sim-section-label", "Thresholds"),
        sliderInput("num_pareto_slices", "Number of thresholds", min = 1, max = 6, value = 3, step = 1),
        div(class = "dft-sliders", uiOutput("pareto_slider_inputs")),
        dft_help("The first threshold is always the smallest claim. Thresholds must increase."),
        tags$hr(class = "sim-divider"),
        div(class = "sim-section-label", "Chart options"),
        dft_log_switch("piecewise_pareto_fit_log_scale")
      ),
      uiOutput("piecewise_pareto_ks_test"),
      bslib::card(
        sim_card_header("table-cells", "Fitted alphas", "One Pareto alpha for each layer between thresholds."),
        bslib::card_body(uiOutput("fitted_sliced_pareto"))
      ),
      bslib::card(
        sim_card_header("chart-line", "CDF fit", "Empirical against fitted cumulative distribution."),
        bslib::card_body(dft_plot_output("piecewise_pareto_cdf_plot"))
      )
    )
  ),

  bslib::nav_spacer(),
  bslib::nav_item(sim_theme_switch())
)
