#' Styles for the GLM fitting tool
#'
#' Added after the simulator styles (sim_ui_css) and the distribution fitting
#' tool styles (dft_ui_css), whose cards, inputs, theme switch, tiles, tables
#' and light / dark tokens the tool shares; this block gives the tool its teal
#' accent and styles the parts only it has.
#' @noRd
glm_ui_css <- "
:root, [data-bs-theme='light'] {
  --dft-accent: #0d9488;
  --dft-accent-strong: #0f766e;
  --dft-accent-shadow: rgba(13, 148, 136, 0.28);
  --sim-accent-soft: #f0fdfa;
  --sim-accent-text: #0f766e;
  --dft-row-hover: #f5fdfb;
}

[data-bs-theme='dark'] {
  --dft-accent: #14b8a6;
  --dft-accent-strong: #0d9488;
  --dft-accent-shadow: rgba(20, 184, 166, 0.28);
  --sim-accent-soft: rgba(20, 184, 166, 0.16);
  --sim-accent-text: #5eead4;
  --dft-row-hover: rgba(20, 184, 166, 0.08);
  --bs-link-color: #5eead4;
  --bs-link-color-rgb: 94, 234, 212;
  --bs-link-hover-color: #99f6e4;
  --bs-link-hover-color-rgb: 153, 246, 228;
}

/* ---------- Accent ---------- */
.navbar {
  background: linear-gradient(90deg, #042f2e 0%, #115e59 100%) !important;
}

.sim-brand-mark {
  background: linear-gradient(135deg, #2dd4bf, #0d9488);
  box-shadow: 0 6px 18px rgba(13, 148, 136, 0.35);
  font-weight: 800;
  font-size: 13px;
  letter-spacing: 0.02em;
}

.theme-btn.active,
.sim-hero .btn-light {
  color: #042f2e;
}

.theme-btn:focus-visible,
.pill-radio .radio-inline:has(input:focus-visible) {
  outline-color: #5eead4 !important;
}

.form-control:focus, .form-select:focus, .selectize-input.focus,
.form-check-input:focus {
  box-shadow: 0 0 0 0.2rem rgba(13, 148, 136, 0.18) !important;
}

.btn-run {
  background: linear-gradient(135deg, #0d9488, #0f766e);
}

.btn-run:hover, .btn-run:focus {
  background: linear-gradient(135deg, #0f766e, #115e59);
}

.sim-hero {
  background:
    radial-gradient(circle at 85% 20%, rgba(45, 212, 191, 0.32), transparent 45%),
    linear-gradient(135deg, #042f2e 0%, #115e59 55%, #0d9488 130%);
}

.sim-hero-eyebrow {
  color: #5eead4;
}

/* ---------- Formula ---------- */
#formula {
  font-family: 'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  font-size: 0.86rem;
  min-height: 110px;
}

.glm-formula-lhs {
  font-family: 'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  font-size: 0.82rem;
  color: var(--sim-muted);
  margin: -0.35rem 0 0.4rem 0;
  overflow-wrap: anywhere;
}

.glm-chips {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
  margin: 0.25rem 0 0.9rem 0;
}

.glm-chips:empty {
  display: none;
}

.glm-chip {
  border: 1px solid var(--sim-border-strong);
  background: var(--sim-input-bg);
  color: var(--sim-label);
  border-radius: 999px;
  padding: 0.18rem 0.6rem;
  font-size: 0.78rem;
  font-weight: 600;
  cursor: pointer;
  transition: border-color 0.15s ease, background-color 0.15s ease, color 0.15s ease;
}

.glm-chip:hover {
  border-color: var(--dft-accent);
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
}

.glm-chip:focus-visible {
  outline: 2px solid var(--dft-accent);
  outline-offset: 1px;
}

/* ---------- Stored models ---------- */
.glm-slots {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  gap: 0.75rem;
}

.glm-slot {
  border: 1px solid var(--sim-border);
  border-radius: 12px;
  padding: 0.85rem 0.95rem;
  background: var(--sim-card-bg);
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  min-width: 0;
}

.glm-slot-title {
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--sim-muted);
}

.glm-slot-body {
  flex: 1 1 auto;
  font-size: 0.85rem;
  color: var(--sim-label);
  min-width: 0;
}

.glm-slot-formula {
  font-family: 'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  font-size: 0.8rem;
  color: var(--sim-heading);
  overflow-wrap: anywhere;
  margin-bottom: 0.25rem;
}

.glm-slot-aic {
  font-size: 1.1rem;
  font-weight: 800;
  color: var(--sim-heading);
  font-variant-numeric: tabular-nums;
}

.glm-slot-empty {
  color: var(--sim-muted);
  font-style: italic;
}

.glm-slot-buttons {
  display: flex;
  gap: 0.5rem;
}

.glm-slot-buttons .btn {
  flex: 1 1 0;
}

.glm-better {
  color: var(--dft-best-text);
  font-weight: 700;
}

/* ---------- Buttons ---------- */
.btn-outline-glm {
  border: 1px solid var(--sim-border-strong);
  background: var(--sim-input-bg);
  color: var(--sim-label);
  font-size: 0.85rem;
  padding: 0.4rem 0.75rem;
}

.btn-outline-glm:hover, .btn-outline-glm:focus {
  border-color: var(--dft-accent);
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
}

.btn-outline-glm.disabled, .btn-outline-glm:disabled {
  opacity: 0.55;
}

.glm-downloads {
  display: grid;
  gap: 0.5rem;
}

.glm-downloads .btn {
  width: 100%;
  text-align: left;
}

/* significance marks in the coefficient table */
.glm-signif {
  color: var(--dft-best-text);
  font-weight: 800;
  margin-left: 0.25rem;
}

.glm-steps-inline {
  margin: 0;
  padding-left: 1.1rem;
  color: var(--sim-label);
}

.glm-steps-inline li + li {
  margin-top: 0.35rem;
}
"

#' Script for the formula column chips: clicking a column adds it to the formula
#'
#' @noRd
glm_formula_chips_js <- "
(function () {
  document.addEventListener('click', function (event) {
    var chip = event.target.closest && event.target.closest('.glm-chip');
    if (!chip) return;
    var area = document.getElementById('formula');
    if (!area) return;
    var term = chip.getAttribute('data-term');
    var current = area.value.replace(/\\s+$/, '');
    var endsWithOperator = /[+*:~(\\-]$/.test(current);
    area.value = current === '' ? term : current + (endsWithOperator ? ' ' : ' + ') + term;
    if (window.jQuery) window.jQuery(area).trigger('change');
    area.focus();
  });
})();
"

#' Links offered for each GLM family, the first being the family's default
#'
#' @noRd
glm_family_links <- list(
  gaussian = c("identity", "log", "inverse"),
  poisson = c("log", "identity", "sqrt"),
  binomial = c("logit", "probit", "cloglog", "cauchit", "log"),
  Gamma = c("inverse", "log", "identity"),
  inverse.gaussian = c("1/mu^2", "inverse", "log", "identity")
)

#' User interface of the Shiny GLM fitting tool
#'
#' A function of the request, so that it is built when the app starts, after
#' every helper of the package is defined.
#' @param request The request, supplied by shiny.
#' @return The user interface of the application, a bslib navbar page.
#' @keywords internal
GLMFittingToolUI <- function(request) {
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", "GLM"),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "GLM Fitting Tool"),
        tags$span(class = "sim-brand-subtitle", "Model fitting and diagnostics by NetSimR")
      )
    ),
    window_title = "GLM Fitting Tool",
    id = "main_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#042f2e",
      theme = "dark",
      underline = FALSE
    ),

    theme = bslib::bs_theme(
      version = 5,
      primary = "#0d9488",
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
      ),
      code_font = bslib::font_collection(
        bslib::font_google("JetBrains Mono", local = FALSE),
        "ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace"
      )
    ),

    header = tagList(
      tags$head(
        tags$script(HTML(netsimr_theme_js("netsimr-glm-fitting-theme"))),
        tags$style(HTML(sim_ui_css)),
        tags$style(HTML(dft_ui_css)),
        tags$style(HTML(glm_ui_css))
      ),
      #tells the server which theme is showing, so the chart is drawn in matching colours
      tags$script(HTML(sim_compare_theme_js("app_theme"))),
      tags$script(HTML(glm_formula_chips_js)),
      #Shiny's built-in busy indicators: a top progress bar while the server works,
      #plus spinners on outputs that are recalculating
      useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
      busyIndicatorOptions(
        pulse_background = "linear-gradient(90deg, #2dd4bf, #0d9488, #2dd4bf)",
        pulse_height = "4px"
      )
    ),

    # -------------------------------------------------------------- Welcome
    bslib::nav_panel(
      title = "Welcome",
      value = "welcome",
      icon = icon("house"),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),

        div(
          class = "sim-hero",
          div(class = "sim-hero-eyebrow", "NetSimR"),
          div(class = "sim-hero-title", "GLM Fitting Tool"),
          p(
            class = "sim-hero-text",
            "Import data from a file or a database, fit generalised linear models, ",
            "compare formulas side by side and check actual against predicted ",
            "values across any explanatory variable."
          ),
          tags$button(
            type = "button",
            class = "btn btn-light",
            onclick = "document.querySelector('.navbar a[data-value=\"data\"]').click();",
            icon("upload"), " Import data"
          )
        ),

        bslib::card(
          sim_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Import"),
                          div(class = "sim-step-text", "A CSV file, or a query on MySQL, SQLite, SQL Server or PostgreSQL."))),
              tags$li(div(div(class = "sim-step-title", "Fit"),
                          div(class = "sim-step-text", "Choose the response, family, link, offset, weights and formula."))),
              tags$li(div(div(class = "sim-step-title", "Compare"),
                          div(class = "sim-step-text", "Store two models and compare their AIC."))),
              tags$li(div(div(class = "sim-step-title", "Check and export"),
                          div(class = "sim-step-text", "Actual against predicted, then download the model and predictions.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("database"), "Data sources"),
          div(class = "sim-stat-value", paste(c("CSV", "MySQL", "SQLite", "SQL Server", "PostgreSQL"),
                                              collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("sitemap"), "Model families"),
          div(class = "sim-stat-value", paste(c("Gaussian", "Poisson", "Binomial", "Gamma", "Inverse Gaussian"),
                                              collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("file-export"), "Exports"),
          div(class = "sim-stat-value", paste(c("Model (RDS)", "Summary", "Predictions (CSV)"),
                                              collapse = paste0(" ", intToUtf8(183), " ")))
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

    # ----------------------------------------------------------------- Data
    bslib::nav_panel(
      title = "Data",
      value = "data",
      icon = icon("table"),

      dft_page_intro("Data", "Import a CSV file or the result of a database query."),

      dft_layout(
        dft_settings_card(
          "file-import", "Import", "Choose where the data comes from.",
          div(class = "sim-section-label", "Source"),
          div(class = "pill-radio",
              radioButtons("data_source", NULL, c("CSV file" = "CSV File", "Database" = "Database"),
                           "CSV File", inline = TRUE)),

          conditionalPanel(
            condition = "input.data_source == 'CSV File'",
            fileInput("csv_file", "File", accept = c(
              "text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt", ".tsv"
            ), placeholder = "No file selected"),
            bslib::input_switch("csv_header", "First row holds column names", value = TRUE),
            div(class = "sim-section-label mt-2", "Separator"),
            div(class = "pill-radio",
                radioButtons("csv_sep", NULL, c(Comma = ",", Semicolon = ";", Tab = "\t"), ",", inline = TRUE)),
            div(class = "sim-section-label mt-2", "Decimal mark"),
            div(class = "pill-radio",
                radioButtons("csv_dec", NULL, c("Point (1.5)" = ".", "Comma (1,5)" = ","), ".", inline = TRUE)),
            div(class = "sim-section-label mt-2", "Quote"),
            div(class = "pill-radio",
                radioButtons("csv_quote", NULL, c("Double" = '"', "Single" = "'", "None" = ""), '"', inline = TRUE))
          ),

          conditionalPanel(
            condition = "input.data_source == 'Database'",
            selectInput("db_type", "Database type", choices = c("MySQL", "SQLite", "SQL Server", "PostgreSQL")),
            conditionalPanel(
              condition = "input.db_type != 'SQLite'",
              textInput("db_host", "Host", "localhost")
            ),
            textInput("db_name", "Database name (the file path for SQLite)", "public"),
            conditionalPanel(
              condition = "input.db_type == 'MySQL' || input.db_type == 'PostgreSQL'",
              textInput("db_port", "Port (optional)", "", placeholder = "Default port")
            ),
            conditionalPanel(
              condition = "input.db_type == 'SQL Server'",
              bslib::input_switch("windows_auth", "Windows authentication", value = FALSE)
            ),
            conditionalPanel(
              condition = "input.db_type != 'SQLite' && (input.db_type != 'SQL Server' || !input.windows_auth)",
              textInput("db_user", "User", "root"),
              passwordInput("db_password", "Password", "")
            ),
            textAreaInput("sql_query", "SQL query", "SELECT * FROM your_table", rows = 5, width = "100%"),
            dft_help("The password is used for this connection only; it is never saved with the settings.")
          ),

          actionButton("submit", "Import data", icon = icon("file-import"), class = "btn-run mt-2")
        ),
        uiOutput("data_overview"),
        bslib::card(
          sim_card_header("table-list", "Preview", "The first rows of the imported data."),
          bslib::card_body(uiOutput("selected_input_data_table"))
        )
      )
    ),

    # ---------------------------------------------------------------- Model
    bslib::nav_panel(
      title = "Model",
      value = "model",
      icon = icon("sliders"),

      dft_page_intro("Model", "Fit a generalised linear model to the imported data."),

      dft_layout(
        tagList(
          dft_settings_card(
            "sliders", "Model", "Response, distribution and formula.",
            dft_column_select("response_variable", "Response"),
            div(
              class = "param-grid-2",
              selectInput("glm_distribution", "Family",
                          c("Gaussian" = "gaussian", "Poisson" = "poisson", "Binomial" = "binomial",
                            "Gamma" = "Gamma", "Inverse Gaussian" = "inverse.gaussian"),
                          selected = "gaussian"),
              selectInput("link_function", "Link", glm_family_links$gaussian, selected = "identity")
            ),
            div(
              class = "param-grid-2",
              selectInput("offset", "Offset", choices = c("None")),
              selectInput("weights", "Weights", choices = c("None"))
            ),
            conditionalPanel(
              condition = "input.offset != 'None'",
              bslib::input_switch("offset_log", "Offset is the log of this column", value = TRUE),
              dft_help("With a log link, the offset of an exposure column is its log. ",
                       "Turn this off if the column already holds the offset.")
            ),
            textAreaInput("formula", "Explanatory terms", rows = 4, width = "100%",
                          placeholder = "e.g. age + region + log(sum_insured)"),
            uiOutput("formula_lhs"),
            uiOutput("formula_columns", class = "glm-chips"),
            dft_help("Click a column to add it. Use + between terms, * for interactions, ",
                     "and 1 for an intercept-only model."),
            actionButton("fit_model", "Fit model", icon = icon("play"), class = "btn-run mt-3")
          ),
          bslib::card(
            class = "mt-3",
            sim_card_header("download", "Downloads", "For the fitted model."),
            bslib::card_body(uiOutput("model_downloads"))
          )
        ),
        uiOutput("model_stats"),
        dft_tabs(
          id = "model_results_tabs",
          bslib::nav_panel("Coefficients", uiOutput("coefficients_table")),
          bslib::nav_panel("Summary", verbatimTextOutput("model_summary"))
        ),
        bslib::card(
          sim_card_header("layer-group", "Stored models", "Store the fitted model in a slot to compare it with another."),
          bslib::card_body(
            div(
              class = "glm-slots",
              lapply(1:2, function(i) {
                div(
                  class = "glm-slot",
                  div(class = "glm-slot-title", paste("Model", i)),
                  div(class = "glm-slot-body", uiOutput(paste0("aic_output_", i))),
                  div(
                    class = "glm-slot-buttons",
                    actionButton(paste0("save_formula_", i), "Store fitted", icon = icon("floppy-disk"), class = "btn-outline-glm"),
                    actionButton(paste0("load_formula_", i), "Load", icon = icon("arrow-rotate-left"), class = "btn-outline-glm")
                  )
                )
              })
            )
          )
        )
      )
    ),

    # ---------------------------------------------------- Actual vs predicted
    bslib::nav_panel(
      title = "Actual vs predicted",
      value = "visualisation",
      icon = icon("chart-column"),

      dft_page_intro("Actual vs predicted",
                     "Compare the actual and predicted response across the bands of an explanatory variable."),

      dft_layout(
        dft_settings_card(
          "sliders", "Settings", "Choose the variable to band by.",
          selectInput("visualize_variable", "Explanatory variable", choices = c("None")),
          sliderInput("number_of_bands_input", "Number of bands for numeric variables", min = 2, max = 50, value = 10),
          div(class = "sim-section-label", "Bands"),
          div(class = "pill-radio",
              radioButtons("band_method", NULL, c("Equal counts" = "quantile", "Equal width" = "width"),
                           "quantile", inline = TRUE)),
          actionButton("execute_visualization", "Draw chart", icon = icon("chart-column"), class = "btn-run mt-2"),
          dft_help("Numeric variables with more distinct values than bands are grouped into bands. ",
                   "With an offset, the chart compares rates per unit of exposure; with weights, weighted averages. ",
                   "The bars show the exposure, the weights, or the number of rows.")
        ),
        bslib::card(
          sim_card_header("chart-column", "Actual vs predicted", "Per band of the explanatory variable, for the fitted model."),
          bslib::card_body(dft_plot_output("fitness_plot", height = "520px"), uiOutput("fitness_note"))
        )
      )
    ),

    # --------------------------------------------------------- Save and load
    bslib::nav_panel(
      title = "Save & load",
      value = "settings",
      icon = icon("floppy-disk"),

      dft_page_intro("Save & load", "Keep the settings of a session and restore them later."),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        bslib::card(
          sim_card_header("floppy-disk", "Settings file", "Import options, model choices and the formula."),
          bslib::card_body(
            downloadButton("DownloadDataHandlerConf", "Save settings", class = "btn-run"),
            tags$hr(class = "sim-divider"),
            fileInput("load_config", "Load settings", accept = ".rds", placeholder = "No file selected"),
            dft_help("Database passwords and uploaded files are not saved.")
          )
        ),
        bslib::card(
          sim_card_header("circle-question", "Restoring a session"),
          bslib::card_body(
            tags$ol(
              class = "glm-steps-inline",
              tags$li("Load the settings file: the import options are restored."),
              tags$li("Import the same data again, from the file or the database."),
              tags$li("The response, offset, weights and variable choices from the settings file are selected once the columns are available.")
            )
          )
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(sim_theme_switch())
  )
}
