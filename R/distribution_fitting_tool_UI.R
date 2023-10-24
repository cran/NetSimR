#' UI file for the Shiny glm fitting tool
#'
#' @return Returns the UI code for the shiny application.

distribution_fitting_tool_UI = fluidPage(



  headerPanel(div("NetDisFit by NetSimR"), windowTitle = "NetSim Distribution Fitting Tool"),

  tabsetPanel(
    tabPanel(
      "Welcome",
      h3("Welcome to the NetDisFit Tool"),
      p("This tool allows you to analyse and fit probability distributions to your data."),
      p("Created by Yiannis Parizas. For more information, please visit my ", a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/"), "."),
      p("You can also reach out to me via email at ", a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com"), "."),
      p("Please reach out if you have any feedback or encounter any bugs.")
    ),

    tabPanel(
      "Data Upload",
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Choose CSV/text File", accept = c(
            'text/csv',
            'text/comma-separated-values,text/plain',
            '.csv'
          )),
          checkboxInput("data_includes_header", "CSV Has Header", value = TRUE),
          radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
          radioButtons('quote', 'Quote', c('Double Quote'='"', 'Single Quote'="'", None=''), '"'),
        ),
        mainPanel(
          br(),
          dataTableOutput("data_table")
        )
      )
    ),


    tabPanel(
      "Frequency Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId='counts_var', label='Claim Counts variable',choices=""),
          checkboxInput("counts_weighted_var", "Weigthed counts fit", value = F),
          conditionalPanel(
            condition = "input.counts_weighted_var",
            selectInput(inputId='counts_weights_var', label='Weigths variable',choices="")
          ),
          actionButton("execute_freq_analysis", "Execute Analysis"),
          br(),
          br(),
          p('Note: the above will remove any negative or non numeric values'),
          br(),
          sliderInput(inputId = "count_hist_bins",
                      label = "Histogram number of bins:",
                      min = 1,
                      max = 100,
                      value = 20),
          selectInput("FreqDistri", "Selected Frequency Distribution",
                      c("Poisson" = "Poisson",
                        "Negative Binomial" = "NegativeBinomial")),
          tabPanel(title="Claim Count Summary", strong("Frequency parameters fitted"), verbatimTextOutput("selected_freq_params"))
        ),
        mainPanel(
          br(),
          tabPanel(title="Claim Count Histogram",plotly::plotlyOutput('count_hist')),
          tabPanel(title="Claim Count CDF vs model fit",plotly::plotlyOutput('freq_fit_plot')),
          tabPanel(title="Claim Count Summary", h5(strong("Frequency data summary"), align="center"), verbatimTextOutput("freq_summary")),
          tabPanel(title="Distribution Proposal", h5(strong("Distribution Proposal"), align="center"), verbatimTextOutput("distribution_proposal")),
          tabPanel(title="Selected distribution Summary", h5(strong("Selected distribution Summary"), align="center"), verbatimTextOutput("selected_distribution_summary"))
        )
      )
    ),



    tabPanel(
      "Severity Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId='severity_var', label='Claim severity variable',choices=""),
          actionButton("execute_sev_analysis", "Execute Analysis"),
          br(),
          br(),
          p('Note: the above will remove any negative, zero or non numeric values'),
          br(),
          sliderInput(inputId = "severity_hist_bins",
                      label = "Histogram number of bins:",
                      min = 1,
                      max = 200,
                      value = 20),
          checkboxInput("sev_fit_log_scale", "Log Scale for Severity cdf", value = F),
          tabPanel(title="Severity parameters summary",
                   strong("Severity parameters fitted"),
                   verbatimTextOutput("sev_param_summary"))
        ),
        mainPanel(
          tabPanel(title="Claim Severity Histogram and cdf fit", plotly::plotlyOutput('sev_hist')),
          tabPanel(title="Claim Severity Summary", h5(strong("Severity data summary"), align="center"), verbatimTextOutput("sev_summary")),
          tabPanel(title="Claim Severity fit log scale", plotly::plotlyOutput('sev_fit_plot'))
        )
      )
    ),



    tabPanel(
      "Sliced Severity Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId='sliced_sev_var', label='Sliced Claim severity variable',choices=""),
          actionButton("execute_sliced_sev_analysis", "Execute Analysis"),
          br(),
          br(),
          p('Note: the above will remove any negative, zero or non numeric values'),
          br(),
          sliderInput(
            inputId = "slicing_point_left",
            label = "Slicing Point",
            min = 0,
            max = 10,
            value = 5
          ),
          sliderInput(
            inputId = "slicing_point_right",
            label = "Second Slicing Point",
            min = 0,
            max = 20,
            value = 10
          ),
          checkboxInput("sev_cens_fit_log_scale", "Log Scale for Severity cdf", value = F),
          tabPanel(
            title="Sliced Severity parameters summary",
            strong("Sliced Severity parameters fitted"),
            verbatimTextOutput("slc_sev_fitted_param_summary")
          )
        ),
        mainPanel(
          tabPanel(title="Mean Excess Function of Claim Severity",plotly::plotlyOutput('mean_excess_func_plot')),
          tabPanel(title="Claim Severity Cumulative Density Function",plotly::plotlyOutput('sliced_sev_cdf_plot'))
        )
      )
    )
  )
)
