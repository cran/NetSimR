#' UI file for the Shiny NetSimR Simulator Tool
#'
#' @return Returns the UI code for the shiny application.
shiny_simulator_ui = fluidPage(
  use_busy_spinner(spin = "fading-circle", position = "full-page"),
  headerPanel(div("NetSimR Claims & Reinsurance Simulator"), windowTitle = "NetSim Claim Simulation Tool"),

  tabsetPanel(
    tabPanel(
      "Welcome",
      icon = icon("house"),
      h3("Welcome to the NetSimR Simulator Tool"),
      p("This tool allows you to simulate insurance claims from various distributions and implement various reinsurance structures."),
      p("Created by Yiannis Parizas. For more information, please visit my ", a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/", target = "_blank"), "."),
      p("You can also reach out to me via email at ", a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com"), "."),
      p("Please reach out if you have any feedback or encounter any bugs.")
    ),

    tabPanel(
      "Claims Simulator",
      icon = icon("dice"),
      br(),

      fluidRow(
        column(4,
               wellPanel(
                 h4(icon("gear"), " Simulation Settings"),
                 sliderInput('numberOfSimulations', 'Number of simulations',
                             min = 10*1000, max = 1000*1000, value = 10*1000, step = 10*1000),
                 checkboxInput('seedSetBinary', 'Custom Seed', value = FALSE),
                 uiOutput("seed_value"),
                 checkboxInput('multiprocessingBinary', 'Multiprocessing (faster for large runs)', value = FALSE)
               )
        ),
        column(4,
               wellPanel(
                 h4(icon("chart-bar"), " Frequency Distribution"),
                 helpText("Models how many claims occur per simulated period."),
                 radioButtons(
                   inputId = 'freqDistr',
                   label = NULL,
                   choiceNames = unname(sapply(freq_dist_options, function(x) x@distr_label)),
                   choiceValues = unname(sapply(freq_dist_options, function(x) x@distrID))
                 ),
                 lapply(1:max(sapply(freq_dist_options, function(x) length(x@paramIDs))),
                        function(i){ uiOutput(paste0('freq_param_', i)) })
               )
        ),
        column(4,
               wellPanel(
                 h4(icon("chart-line"), " Severity Distribution"),
                 helpText("Models the size of each individual claim."),
                 radioButtons(
                   inputId = 'sevDistr',
                   label = NULL,
                   choiceNames = unname(sapply(sev_dist_options, function(x) x@distr_label)),
                   choiceValues = unname(sapply(sev_dist_options, function(x) x@distrID))
                 ),
                 lapply(1:max(sapply(sev_dist_options, function(x) length(x@paramIDs))),
                        function(i){ uiOutput(paste0('sev_param_', i)) })
               )
        )
      ),

      fluidRow(
        column(4,
               wellPanel(
                 h4(icon("scissors"), " Tail Adjustments"),
                 checkboxInput('paretoSlice', 'Apply Pareto Slices', value = FALSE),
                 helpText("Splices a Pareto tail onto the severity distribution above chosen thresholds."),
                 uiOutput("pareto_slice_times"),
                 lapply(1:(max_number_of_pareto_slices*2),
                        function(i){ uiOutput(paste0('slice_pareto_param_', i)) }),
                 hr(),
                 checkboxInput('sevCapBinary', 'Severity Cap', value = FALSE),
                 helpText("Caps any single claim at a maximum amount."),
                 uiOutput("sev_cap_amount")
               )
        ),
        column(4,
               wellPanel(
                 h4(icon("shield-halved"), " Reinsurance - Each & Every Loss (EEL)"),
                 helpText("Applied to each individual claim before it is summed into the total."),
                 radioButtons('reinsuranceStructureEEL', NULL, choices = reinsurance_structures_options),
                 uiOutput("reinsuranceStructureDeductibleEEL"),
                 uiOutput("reinsuranceStructureLimitEEL"),
                 uiOutput("reinsuranceStructureLimitedReinstatements"),
                 uiOutput("reinsuranceStructureReinstatementLimit")
               )
        ),
        column(4,
               wellPanel(
                 h4(icon("umbrella"), " Reinsurance - Aggregate Layer (AL)"),
                 helpText("Applied to the total claims across all EEL-adjusted losses per simulation."),
                 radioButtons('reinsuranceStructureAL', NULL, choices = reinsurance_structures_options),
                 uiOutput("reinsuranceStructureDeductibleAL"),
                 uiOutput("reinsuranceStructureLimitAL")
               )
        )
      ),

      hr(),

      fluidRow(align = "center",
               div(style = "display:inline-block; margin: 5px;",
                   actionButton("RunSimulations", "Run Simulations", icon = icon("play"), class = "btn-primary")),
               div(style = "display:inline-block; margin: 5px;", uiOutput("downloadDataButton")),
               div(style = "display:inline-block; margin: 5px;", uiOutput("downloadReportButton"))
      )
    )
  )
)
