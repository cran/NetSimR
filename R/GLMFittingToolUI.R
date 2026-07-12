#' UI file for the Shiny GLM fitting tool
#'
#' @return Returns the UI code for the shiny application.

GLMFittingToolUI <- bslib::page_navbar(
  title = div(
    style = "display:flex; align-items:center; gap:12px;",
    div(
      style = paste(
        "width:44px; height:44px; border-radius:12px;",
        "background: linear-gradient(135deg, #14b8a6, #0f766e);",
        "display:flex; align-items:center; justify-content:center;",
        "color:white; font-weight:800; font-size:16px;",
        "box-shadow: 0 6px 18px rgba(0,0,0,0.12);"
      ),
      "GLM"
    ),
    div(
      style = "display:flex; flex-direction:column; line-height:1.05;",
      tags$span(
        "GLM Fitting Tool",
        style = "font-weight:800; font-size:1.05rem; color:white; letter-spacing:-0.02em;"
      ),
      tags$span(
        "Model fitting and diagnostics",
        style = "font-size:12px; color:rgba(255,255,255,0.72); margin-top:2px;"
      )
    )
  ),
  window_title = "GLM Fitting Tool",
  id = "main_navbar",
  selected = "Welcome",

  navbar_options = bslib::navbar_options(
    bg = "#0f766e",
    theme = "dark",
    underline = FALSE
  ),

  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0f766e",
    secondary = "#64748b",
    success = "#15803d",
    info = "#0891b2",
    warning = "#d97706",
    danger = "#dc2626",
    bg = "#f8fafc",
    fg = "#0f172a",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("JetBrains Mono")
  ),

  header = tagList(
    shinybusy::use_busy_spinner(spin = "fading-circle", position = "full-page"),
    tags$head(
      tags$style(HTML("
:root {
  --app-radius: 18px;
  --app-shadow: 0 10px 30px rgba(15, 23, 42, 0.08);
  --app-border: 1px solid rgba(148, 163, 184, 0.22);
}

body {
  background: linear-gradient(180deg, #f8fafc 0%, #f1f5f9 100%);
}

.navbar {
  box-shadow: 0 2px 16px rgba(15, 23, 42, 0.08);
  padding-top: 0.55rem;
  padding-bottom: 0.55rem;
}

.navbar-brand {
  margin-right: 2.8rem !important;
  padding-top: 0.35rem;
  padding-bottom: 0.35rem;
  min-width: 280px;
}

.navbar .container-fluid {
  align-items: center;
}

.navbar .navbar-nav {
  margin-left: 1.2rem !important;
  gap: 0.35rem;
  align-items: center;
}

.navbar .nav-item {
  display: flex;
  align-items: center;
}

.navbar .nav-link {
  color: rgba(255,255,255,0.88) !important;
  font-weight: 700;
  font-size: 0.98rem;
  border-radius: 12px;
  padding: 0.7rem 1rem !important;
  transition: all 0.18s ease-in-out;
}

.navbar .nav-link:hover,
.navbar .nav-link:focus {
  color: #ffffff !important;
  background-color: rgba(255,255,255,0.12);
}

.navbar .nav-link.active,
.navbar .nav-item.show .nav-link {
  color: #0f172a !important;
  background: #f8fafc !important;
  box-shadow: 0 4px 12px rgba(15, 23, 42, 0.10);
}

.tab-content {
  padding-top: 0.75rem;
}

.glm-hero {
  padding: 8px 0 4px 0;
}

.glm-hero-title {
  font-size: 2rem;
  font-weight: 800;
  letter-spacing: -0.03em;
  color: #0f172a;
  margin-bottom: 0.5rem;
}

.glm-hero-subtitle {
  font-size: 1rem;
  color: #475569;
  max-width: 760px;
  margin-bottom: 0;
}

.glm-stat {
  background: white;
  border-radius: var(--app-radius);
  box-shadow: var(--app-shadow);
  border: var(--app-border);
  padding: 1rem 1.1rem;
  height: 100%;
}

.glm-stat-label {
  font-size: 0.78rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: #64748b;
  margin-bottom: 0.35rem;
}

.glm-stat-value {
  font-size: 1.2rem;
  font-weight: 800;
  color: #0f172a;
}

.card {
  border: var(--app-border) !important;
  border-radius: var(--app-radius) !important;
  box-shadow: var(--app-shadow);
  overflow: hidden;
}

.card-header {
  background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
  font-weight: 700;
  border-bottom: 1px solid rgba(148, 163, 184, 0.16);
}

.card-body {
  overflow: visible !important;
}

.scroll-card {
  height: 760px;
}

.scroll-card .card-body,
.scroll-card-body {
  overflow-y: auto !important;
  overflow-x: hidden !important;
  max-height: 680px;
  padding-right: 0.85rem;
}

.import-scroll-card {
  height: 700px;
}

.import-scroll-card .card-body,
.import-scroll-card-body {
  overflow-y: auto !important;
  overflow-x: hidden !important;
  max-height: 620px;
  padding-right: 0.85rem;
}

.form-label, .control-label {
  display: block;
  font-weight: 700;
  color: #334155;
  margin-bottom: 0.45rem;
  line-height: 1.3;
}

.form-control, .form-select, .selectize-input {
  border-radius: 12px !important;
  border: 1px solid #cbd5e1 !important;
  min-height: 46px;
  box-shadow: none !important;
  margin-bottom: 0.25rem;
}

.form-control:focus, .form-select:focus, .selectize-input.focus {
  border-color: #0f766e !important;
  box-shadow: 0 0 0 0.2rem rgba(15, 118, 110, 0.15) !important;
}

textarea.form-control {
  min-height: 140px;
}

.btn {
  border-radius: 12px;
  font-weight: 700;
  min-height: 42px;
}

.btn-primary {
  background: linear-gradient(135deg, #0f766e, #0ea5a4);
  border: none;
}

.btn-primary:hover,
.btn-primary:focus {
  background: linear-gradient(135deg, #0d6b63, #0c948f);
  border: none;
}

.btn-default, .btn-secondary {
  border: 1px solid #cbd5e1;
  background: white;
  color: #334155;
}

.well {
  border-radius: 14px;
  border: var(--app-border);
  background: #ffffff;
  box-shadow: none;
}

pre, .shiny-text-output {
  background: #0f172a;
  color: #e2e8f0;
  border-radius: 14px;
  padding: 1rem;
  border: none;
}

.form-section-title {
  font-size: 0.8rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: #64748b;
  margin: 0 0 1rem 0;
}

.form-block {
  margin-bottom: 1.15rem;
}

.model-controls-body {
  padding-top: 1.25rem;
}

.import-controls-body {
  padding-top: 1rem;
}

.model-controls-body .shiny-input-container,
.import-controls-body .shiny-input-container {
  width: 100%;
  margin-bottom: 1rem;
}

.subgrid-2 {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem 1rem;
  margin-bottom: 0.25rem;
}

.stored-formula-card {
  border: 1px solid rgba(148, 163, 184, 0.22);
  border-radius: 14px;
  padding: 1rem;
  background: #f8fafc;
  height: 100%;
}

.stored-formula-title {
  font-weight: 800;
  color: #334155;
  margin-bottom: 0.75rem;
}

.muted-note {
  color: #64748b;
  font-size: 0.95rem;
  margin-bottom: 0;
}

.contact-link a {
  text-decoration: none;
  font-weight: 600;
}

.shiny-download-link {
  width: 100%;
  margin-bottom: 0.5rem;
}

.html-fill-container,
.html-fill-item {
  min-height: 0;
}

.reactable {
  font-size: 0.95rem;
}

@media (max-width: 991.98px) {
  .navbar-brand {
    min-width: auto;
    margin-right: 1rem !important;
  }

  .navbar .navbar-nav {
    margin-left: 0 !important;
    gap: 0.15rem;
    padding-top: 0.5rem;
  }

  .navbar .nav-link {
    padding: 0.6rem 0.8rem !important;
  }

  .subgrid-2 {
    grid-template-columns: 1fr;
  }

  .scroll-card,
  .import-scroll-card {
    height: auto;
  }

  .scroll-card .card-body,
  .scroll-card-body,
  .import-scroll-card .card-body,
  .import-scroll-card-body {
    max-height: none;
    overflow: visible !important;
    padding-right: 0;
  }
}
"))
    )
  ),

  bslib::nav_panel(
    "Welcome",
    bslib::layout_columns(
      col_widths = c(8, 4),

      bslib::card(
        bslib::card_body(
          class = "glm-hero",
          div(class = "glm-hero-title", "Welcome to the GLM Fitting Tool"),
          p(
            class = "glm-hero-subtitle",
            "A clean workspace for importing data, fitting generalized linear models, comparing formulas, and visualising actual versus predicted results."
          )
        )
      ),

      bslib::card(
        bslib::card_header("About"),
        bslib::card_body(
          p("Designed for fast actuarial and analytical model experimentation."),
          div(
            class = "contact-link",
            p(
              "Created by Yiannis Parizas. Visit ",
              tags$a(
                "LinkedIn profile",
                href = "https://www.linkedin.com/in/yiannisparizas/",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              "."
            ),
            p(
              "Contact: ",
              tags$a(
                "yiannis.parizas@gmail.com",
                href = "mailto:yiannis.parizas@gmail.com"
              )
            )
          )
        )
      ),

      div(
        class = "glm-stat",
        div(class = "glm-stat-label", "Workflow"),
        div(class = "glm-stat-value", "Import -> Fit -> Compare -> Export")
      ),
      div(
        class = "glm-stat",
        div(class = "glm-stat-label", "Data sources"),
        div(class = "glm-stat-value", "CSV and databases")
      ),
      div(
        class = "glm-stat",
        div(class = "glm-stat-label", "Model families"),
        div(class = "glm-stat-value", "Gaussian, Poisson, Binomial, Gamma")
      )
    )
  ),

  bslib::nav_panel(
    "Save - Load inputs",
    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        bslib::card_header("Configuration"),
        bslib::card_body(
          fileInput("load_config", "Load configuration backup"),
          uiOutput("downloadConfButton"),
          tags$hr(),
          p(
            class = "muted-note",
            "Use this section to save your current app state or reload a previous setup."
          )
        )
      ),

      bslib::card(
        bslib::card_header("Instructions"),
        bslib::card_body(
          h4("Recover previous work"),
          p("To fully restore your previous work:"),
          tags$ol(
            tags$li("Load the saved configuration file."),
            tags$li("Reload the original dataset from CSV or database."),
            tags$li("Load the configuration again if some dynamic inputs depend on the imported data.")
          ),
          p(
            class = "muted-note",
            "This two-step restore flow is useful because several model controls depend on the available data columns."
          )
        )
      )
    )
  ),

  bslib::nav_panel(
    "Data import",
    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        full_screen = TRUE,
        height = "700px",
        class = "import-scroll-card",
        bslib::card_header("Import settings"),
        bslib::card_body(
          class = "import-controls-body import-scroll-card-body",

          div(class = "form-section-title", "Source"),
          selectInput(
            "data_source",
            "Select Data Source",
            choices = c("CSV File", "Database")
          ),

          conditionalPanel(
            condition = "input.data_source == 'Database'",
            div(
              div(class = "form-section-title", "Connection"),
              selectInput(
                "db_type",
                "Select Database Type",
                choices = c("MySQL", "SQLite", "SQL Server", "PostgreSQL")
              ),
              textInput("db_host", "Database Host/Path", "localhost"),
              textInput("db_name", "Database Name", "public"),

              conditionalPanel(
                condition = "((input.db_type == 'MySQL') || (input.db_type == 'PostgreSQL'))",
                textInput("db_port", "Database Port (optional)", "")
              ),

              conditionalPanel(
                condition = "input.db_type == 'SQL Server'",
                checkboxInput("windows_auth", "Windows Authentication", value = FALSE)
              ),

              conditionalPanel(
                condition = "(input.db_type != 'SQL Server') || ((input.db_type == 'SQL Server') && !input.windows_auth)",
                div(
                  textInput("db_user", "Database User", "root"),
                  passwordInput("db_password", "Database Password", "")
                )
              ),

              div(class = "form-section-title", "Query"),
              textAreaInput(
                "sql_query",
                "SQL Query",
                "SELECT * FROM your_table",
                rows = 6,
                width = "100%"
              )
            )
          ),

          conditionalPanel(
            condition = "input.data_source == 'CSV File'",
            div(
              div(class = "form-section-title", "File"),
              fileInput("csv_file", "Upload CSV File")
            )
          ),

          tags$br(),
          actionButton("submit", "Submit Query / Upload", class = "btn-primary")
        )
      ),

      bslib::card(
        full_screen = TRUE,
        height = "700px",
        bslib::card_header("Selected input data"),
        bslib::card_body(
          reactable::reactableOutput("selected_input_data_table")
        )
      )
    )
  ),

  bslib::nav_panel(
    "GLM Model Fitting",
    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        full_screen = TRUE,
        height = "760px",
        class = "scroll-card",
        bslib::card_header("Model controls"),
        bslib::card_body(
          class = "model-controls-body scroll-card-body",

          div(class = "form-section-title", "Core setup"),

          div(
            class = "form-block",
            selectInput("response_variable", "Select Response Variable", choices = NULL)
          ),

          div(
            class = "subgrid-2",
            div(
              class = "form-block",
              selectInput(
                "glm_distribution",
                "GLM Distribution",
                choices = c("gaussian", "poisson", "binomial", "Gamma", "inverse.gaussian"),
                selected = "gaussian"
              )
            ),
            div(
              class = "form-block",
              selectInput(
                "link_function",
                "Link Function",
                choices = c("identity", "logit", "probit", "log", "inverse"),
                selected = "identity"
              )
            )
          ),

          div(
            class = "subgrid-2",
            div(
              class = "form-block",
              selectInput("offset", "Offset (optional)", choices = NULL)
            ),
            div(
              class = "form-block",
              selectInput("weights", "Weights (optional)", choices = NULL)
            )
          ),

          div(
            class = "form-block",
            textAreaInput("formula", "Enter Formula", rows = 7, width = "100%")
          ),

          actionButton("fit_model", "Fit GLM Model", class = "btn-primary"),
          tags$hr(),

          div(class = "form-section-title", "Stored formulas"),

          div(
            class = "subgrid-2",
            div(
              class = "stored-formula-card",
              div(class = "stored-formula-title", "Placeholder 1"),
              actionButton("save_formula_1", "Save", class = "btn-secondary"),
              tags$span(style = "display:inline-block; width:8px;"),
              actionButton("load_formula_1", "Load", class = "btn-secondary"),
              tags$br(), tags$br(),
              verbatimTextOutput("aic_output_1")
            ),
            div(
              class = "stored-formula-card",
              div(class = "stored-formula-title", "Placeholder 2"),
              actionButton("save_formula_2", "Save", class = "btn-secondary"),
              tags$span(style = "display:inline-block; width:8px;"),
              actionButton("load_formula_2", "Load", class = "btn-secondary"),
              tags$br(), tags$br(),
              verbatimTextOutput("aic_output_2")
            )
          ),

          tags$hr(),
          div(class = "form-section-title", "Downloads"),
          downloadButton("download_model", "Download Model (RDS)"),
          downloadButton("download_summary", "Download Model Summary"),
          downloadButton("download_data_with_predictions", "Download Predictions CSV")
        )
      ),

      bslib::card(
        full_screen = TRUE,
        height = "760px",
        bslib::card_header("GLM model summary"),
        bslib::card_body(
          verbatimTextOutput("model_summary")
        )
      )
    )
  ),

  bslib::nav_panel(
    "Predictions visualisation",
    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        bslib::card_header("Visualisation controls"),
        bslib::card_body(
          selectInput(
            "visualize_variable",
            "Select Explanatory Variable",
            choices = c("None")
          ),
          sliderInput(
            "number_of_bands_input",
            "Select number of bands:",
            min = 10,
            max = 100,
            value = 10
          ),
          tags$br(),
          actionButton("execute_visualization", "Execute Visualization", class = "btn-primary"),
          tags$hr(),
          p(
            class = "muted-note",
            "Compare actual and predicted values across grouped bands, with exposure shown on the secondary axis."
          )
        )
      ),

      bslib::card(
        full_screen = TRUE,
        height = "720px",
        bslib::card_header("Actual vs predicted"),
        bslib::card_body(
          plotly::plotlyOutput("fitness_plot", height = "620px")
        )
      )
    )
  )
)
