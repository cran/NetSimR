#' UI file for the Shiny GLM Fitting Tool
#'
#' @return Returns the UI code for the shiny application.
shiny_simulator_ui = fluidPage(
  use_busy_spinner(spin = "fading-circle", position = "full-page"),
  headerPanel(div("NetSimR simulator by NetSimR"), windowTitle = "NetSim Claim Simulation Tool"),

  tabsetPanel(
    tabPanel(
      "Welcome",
      h3("Welcome to the NetSimR simulator Tool"),
      p("This tool allows you to simulate insurance claims from various distributions and implement various reinsurance structures."),
      p("Created by Yiannis Parizas. For more information, please visit my ", a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/"), "."),
      p("You can also reach out to me via email at ", a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com"), "."),
      p("Please reach out if you have any feedback or encounter any bugs.")
    ),

    tabPanel(
      'Claims Simulator',
      flowLayout(
        verticalLayout(
          #number of simulations
          sliderInput('numberOfSimulations', 'Number of simulations', min=10*1000, max=500*1000, value=10*1000, step=10*1000)

          #seed
          ,checkboxInput('seedSetBinary', 'Custom Seed', value = F)
          ,uiOutput("seed_value")

          #multiprocessing
          ,checkboxInput('multiprocessingBinary', 'Multiprocessing', value = F)
        )

        ,verticalLayout(
          #freq distribution
          radioButtons(
            inputId = 'freqDistr'
            ,label = 'Frequency Distribution'
            ,choiceNames = unname(sapply(freq_dist_options, function(x) x@distr_label))
            ,choiceValues = unname(sapply(freq_dist_options, function(x) x@distrID))
          )
          ,lapply(1:max(sapply(freq_dist_options, function(x) length(x@paramIDs))), function(i){uiOutput(paste0('freq_param_', i))})
        )

        ,verticalLayout(
          #sev distribution
          radioButtons(
            inputId = 'sevDistr'
            ,label = 'Severity Distribution'
            ,choiceNames = unname(sapply(sev_dist_options, function(x) x@distr_label))
            ,choiceValues = unname(sapply(sev_dist_options, function(x) x@distrID))
          )
          ,lapply(1:max(sapply(sev_dist_options, function(x) length(x@paramIDs))), function(i){uiOutput(paste0('sev_param_', i))})
        )

        ,verticalLayout(
          #apply pareto slices
          checkboxInput('paretoSlice', 'Apply Pareto Slices', value = F)
          ,uiOutput("pareto_slice_times")
          ,lapply(1:(max_number_of_pareto_slices*2), function(i){uiOutput(paste0('slice_pareto_param_', i))})

          #severity cap
          ,checkboxInput('sevCapBinary', 'Severity cap', value = F)
          ,uiOutput("sev_cap_amount")
        )

        ,verticalLayout(
          #reinsurance structure EEL
          radioButtons('reinsuranceStructureEEL', 'Reinsurance Structure - EEL', choices = reinsurance_structures_options)
          ,uiOutput("reinsuranceStructureDeductibleEEL")
          ,uiOutput("reinsuranceStructureLimitEEL")
          ,uiOutput("reinsuranceStructureLimitedReinstatements")
          ,uiOutput("reinsuranceStructureReinstatementLimit")
        )

        ,verticalLayout(
          #reinsurance structure AL
          radioButtons('reinsuranceStructureAL', 'Reinsurance Structure - AL', choices = reinsurance_structures_options)
          ,uiOutput("reinsuranceStructureDeductibleAL")
          ,uiOutput("reinsuranceStructureLimitAL")
        )

      )

      ,br()

      ,fluidRow(align="center",
                div(style="display:inline-block",actionButton("RunSimulations", "Run Simulations"))
                ,div(style="display:inline-block",uiOutput("downloadDataButton"))
                ,div(style="display:inline-block",uiOutput("downloadReportButton"))
      )
    )
  )
)
