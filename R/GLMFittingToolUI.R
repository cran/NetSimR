#' UI file for the Shiny glm fitting tool
#'
#' @return Returns the UI code for the shiny application.
GLMFittingToolUI = fluidPage(

  use_busy_spinner(spin = "fading-circle", position = "full-page")
  ,headerPanel(div("GLM Fitting tool"), windowTitle = "GLM Fitting Tool")
  ,tabsetPanel(
    tabPanel(
      "Welcome",
      h3("Welcome to the GLM Fitting tool"),
      p("This tool allows you to analyse and fit GLM models to your data."),
      p("Created by Yiannis Parizas. For more information, please visit my ", a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/"), "."),
      p("You can also reach out to me via email at ", a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com"), "."),
      p("Please reach out if you have any feedback or encounter any bugs.")
    ),

    tabPanel(
      "Save - Load inputs"
      ,sidebarLayout(
        sidebarPanel(
          fileInput("load_config", "Load Configuration")
          ,fluidRow(uiOutput("downloadConfButton"))
        )
        ,mainPanel(
          h3(p('You can recover your previous work here. You can load the backup, load the data and load the backup again to fully recover your previous work.'))
        )
      )
    )

    ,tabPanel(
      "Data import",
      sidebarPanel(
        selectInput("data_source", "Select Data Source",
                    choices = c("CSV File", "Database")),
        conditionalPanel(
          condition = "input.data_source == 'Database'",
          selectInput("db_type", "Select Database Type",
                      choices = c("MySQL", "SQLite", "SQL Server", "PostgreSQL")),
          textInput("db_host", "Database Host/Path", "localhost"),
          textInput("db_name", "Database Name", "public"),
          conditionalPanel(
            condition = "((input.db_type == 'MySQL') || (input.db_type == 'PostgreSQL'))",
            textInput("db_port", "Database Port (optional)", ""),
          ),
          conditionalPanel(
            condition = "input.db_type == 'SQL Server'",
            checkboxInput("windows_auth", "Windows Authentication")
          ),
          conditionalPanel(
            condition = "(input.db_type != 'SQL Server') || ((input.db_type == 'SQL Server') && !input.windows_auth)",
            textInput("db_user", "Database User", "root"),
            textInput("db_password", "Database Password", "")
          ),
          textAreaInput("sql_query", "SQL Query", "SELECT * FROM your_table", rows=4, width = '100%')
        ),
        conditionalPanel(
          condition = "input.data_source == 'CSV File'",
          fileInput("csv_file", "Upload CSV File")
        ),
        actionButton("submit", "Submit Query/Upload")
      ),
      mainPanel(
        dataTableOutput("selected_input_data_table")
      )
    )

    ,tabPanel(
      "GLM Model Fitting"
      ,sidebarLayout(
        sidebarPanel(
          selectInput("response_variable", "Select Response Variable", choices = NULL),
          selectInput("link_function", "Link Function",
                      choices = c("identity", "logit", "probit", "log", "inverse"),
                      selected = "identity"),
          selectInput("glm_distribution", "GLM Distribution",
                      choices = c("gaussian", "poisson", "binomial", "Gamma", "inverse.gaussian"),
                      selected = "gaussian"),
          selectInput("offset", "Enter Offset (optional)", choices = NULL),
          selectInput("weights", "Enter Weights (optional)", choices = NULL),
          textAreaInput("formula", "Enter Formula", rows=4, width = '100%'),
          br(),
          actionButton("fit_model", "Fit GLM Model"),
          br(),
          br(),
          actionButton("save_formula_1", "Save to placeholder 1"),
          actionButton("load_formula_1", "Load placeholder 1"),
          verbatimTextOutput("aic_output_1"),
          br(),
          actionButton("save_formula_2", "Save to placeholder 2"),
          actionButton("load_formula_2", "Load placeholder 2"),
          verbatimTextOutput("aic_output_2"),
          br(),
          br(),
          downloadButton("download_model", "Download Model (RDS)"),
          downloadButton("download_summary", "Download Model Summary"),
          downloadButton("download_data_with_predictions", "Download Data with Predictions (CSV)")
        ),
        mainPanel(
          h4("GLM Model Summary"),
          verbatimTextOutput("model_summary")
        )
      )
    )

    ,tabPanel(
      "Predictions visualisation"
      ,sidebarLayout(
        sidebarPanel(
          selectInput("visualize_variable", "Select Explanatory Variable", choices = c("None")),
          sliderInput("number_of_bands_input", "Select number of bands:",
                      min = 10, max = 100, value = 10),
          br(),
          actionButton("execute_visualization", "Execute Visualization")
        ),
        mainPanel(
          br(),
          br(),
          plotly::plotlyOutput("fitness_plot")
        )
      )
    )
  )
)
