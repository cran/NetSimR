#' Server function for the Shiny Simulator application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Returns server rendering for the shiny application.
#' @import rmarkdown
#' @import shiny
#' @import shinybusy
#' @import future.apply
#' @import data.table
#' @importFrom future plan
#' @importFrom future sequential
#' @importFrom future multisession
#' @import rmarkdown
#' @import methods
#' @import stats
#' @import scales
#' @import utils
#' @import reactable
shiny_simulator_server = function(input, output, session) {
  #set data.table threads once per session, leave one core free for the main process
  data.table::setDTthreads(max(1, parallel::detectCores() - 1))

  #ensure any future workers are cleaned up when the session ends
  session$onSessionEnded(function() {
    future::plan(future::sequential)
  })

  #seed input
  output$seed_value <- renderUI({
    if (input$seedSetBinary) {
      sliderInput('seedValue', 'Seed value', min=1, max=100, value=1, step=1)
    }
  })

  #help function to render freq/sev parameters
  render_UI_param <- function(param_id, object){
    return(if(!is.na(object@paramIDs[param_id])){numericInput(
      inputId = object@paramIDs[param_id]
      ,label = object@param_labels[param_id]
      ,min = object@param_min_values[param_id]
      ,max = object@param_max_values[param_id]
      ,value = NULL
    )
    })}

  #render frequency parameters
  lapply(freq_dist_parameter_placeholders$param_number, function(i) {
    output[[paste0("freq_param_", i)]] <- renderUI({
      render_UI_param(param_id=i, object=freq_dist_options[[input$freqDistr]])
    })
  })

  #render severity parameters
  lapply(sev_dist_parameter_placeholders$param_number, function(i) {
    output[[paste0("sev_param_", i)]] <- renderUI({
      render_UI_param(param_id=i, object=sev_dist_options[[input$sevDistr]])
    })
  })

  #render Pareto slice parameters
  output$pareto_slice_times <- renderUI({
    if (input$paretoSlice) {
      selectInput("pareto_slice_times","Number of Pareto Slices",1:max_number_of_pareto_slices)
    }
  })

  lapply(1:(2*max_number_of_pareto_slices), function(i) {
    output[[paste0("slice_pareto_param_", i)]] <- renderUI({
      req(input$pareto_slice_times)
      if(input$paretoSlice){if(as.numeric(input$pareto_slice_times)*2>=i){
        numericInput(
          inputId = paste0("slice_pareto_param_", i)
          ,label = ifelse(i %% 2, paste("Sliced alpha", (1+i)/2), paste('Sliced x_m', i/2))
          ,value = NULL
          ,min = 0
        )
      }}
    })
  })

  #render severity cap amount
  output$sev_cap_amount <- renderUI({
    if (input$sevCapBinary) {
      numericInput('sev_cap_amount', 'Severity Cap Amount', value = NULL, min = 0)
    }
  })

  #reinsurance structure EEL inputs
  output$reinsuranceStructureDeductibleEEL <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Unlimited Layer', 'Exclude Layer', 'Limited Layer')) {
      numericInput('reinsurance_structure_eel_dedctible_amount', 'EEL Deductible Amount', value = NULL, min = 0)
    }
  })

  output$reinsuranceStructureLimitEEL <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Limited Layer', 'Exclude Layer')) {
      numericInput('reinsurance_structure_eel_limit_amount', 'EEL Limit Amount', value = NULL, min = 0)
    }
  })

  output$reinsuranceStructureLimitedReinstatements <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Limited Layer')) {
      checkboxInput('reinsuranceStructureLimitedReinstatements', 'Limited Reinstatments', value = F)
    }
  })

  output$reinsuranceStructureReinstatementLimit <- renderUI({
    req(input$reinsuranceStructureLimitedReinstatements)
    if (input$reinsuranceStructureEEL %in% c('Limited Layer')) {
      if(input$reinsuranceStructureLimitedReinstatements) {
        numericInput('reinsuranceStructureReinstatementLimit', 'Number of Reinstatements', value = NULL, min = 0)
      }
    }
  })

  #reinsurance structure AL inputs
  output$reinsuranceStructureDeductibleAL <- renderUI({
    if (input$reinsuranceStructureAL %in% c('Unlimited Layer', 'Exclude Layer', 'Limited Layer')) {
      numericInput('reinsurance_structure_al_dedctible_amount', 'AL Deductible Amount', value = NULL, min = 0)
    }
  })

  output$reinsuranceStructureLimitAL <- renderUI({
    if (input$reinsuranceStructureAL %in% c('Limited Layer', 'Exclude Layer')) {
      numericInput('reinsurance_structure_al_limit_amount', 'AL Limit Amount', value = NULL, min = 0)
    }
  })

  #create simulation data dataFrame reactive to enable download buttons & simulation settings list
  simulated_data <- reactiveValues(data=NULL)
  simulation_settings <- list()
  is_running <- reactiveVal(FALSE)

  observe({
    if (is_running()) {
      updateActionButton(session, "RunSimulations", label = "Running...")
    } else {
      updateActionButton(session, "RunSimulations", label = "Run Simulations")
    }
  })

  #run simulation button
  observeEvent(input$RunSimulations,{
    if (is_running()) return(NULL)
    is_running(TRUE)

    on.exit({
      is_running(FALSE)
      hide_spinner()
      gc()
    }, add = TRUE)

    show_spinner()

    #set parameters
    simulation_settings <<- list(
      freq_params = unname(sapply(
        freq_dist_options[[input$freqDistr]]@paramIDs
        ,function(x) input[[x]]
      ))
      ,sev_params = unname(sapply(
        sev_dist_options[[input$sevDistr]]@paramIDs
        ,function(y) input[[y]]
      ))
      ,numOfSimulations = input$numberOfSimulations
      ,seedSetBinary = input$seedSetBinary
      ,seedValue = input$seedValue
      ,freqDistr = input$freqDistr
      ,sevDistr = input$sevDistr
      ,paretoSlice = input$paretoSlice
      ,pareto_slice_times = as.numeric(input$pareto_slice_times)
      ,slice_pareto_alphas = if(input$paretoSlice){unname(sapply(
        1:as.numeric(input$pareto_slice_times)
        ,function(y) input[[paste0("slice_pareto_param_", y*2-1)]]
      ))}
      ,slice_pareto_x_ms = if(input$paretoSlice){unname(sapply(
        1:as.numeric(input$pareto_slice_times)
        ,function(y) input[[paste0("slice_pareto_param_", y*2)]]
      ))}
      ,sevCapBinary = input$sevCapBinary
      ,sev_cap_amount = input$sev_cap_amount
      ,reinsuranceStructureEEL = input$reinsuranceStructureEEL
      ,reinsurance_structure_eel_dedctible_amount = input$reinsurance_structure_eel_dedctible_amount
      ,reinsurance_structure_eel_limit_amount = input$reinsurance_structure_eel_limit_amount
      ,reinsuranceStructureAL = input$reinsuranceStructureAL
      ,reinsurance_structure_al_dedctible_amount = input$reinsurance_structure_al_dedctible_amount
      ,reinsurance_structure_al_limit_amount = input$reinsurance_structure_al_limit_amount
      ,multiprocessing = input$multiprocessingBinary
      ,reinsuranceStructureLimitedReinstatements = input$reinsuranceStructureLimitedReinstatements
      ,reinsuranceStructureReinstatementLimit = input$reinsuranceStructureReinstatementLimit
    )
    #run simmulations
    simulated_data$data <- tryCatch(
      {
        do.call(simulate_function, simulation_settings)
      }, error = function(cond) {
        future::plan(future::sequential)
        showNotification(
          paste("Error:", conditionMessage(cond)),
          type = "error",
          duration = NULL
        )
        print(cond)
        NULL
      }
    )
  })

  is_downloading_report <- reactiveVal(FALSE)

  output$downloadReportButton <- renderUI({
    req(simulated_data$data)

    label <- if (is_downloading_report()) {
      "Preparing report..."
    } else {
      "Report"
    }

    downloadButton('downloadReportHandler', label)
  })

  #download data button
  output$DownloadDataHandler <- downloadHandler(
    filename = function() { paste0('Simulation_data', '.csv') },
    content = function(file) {
      showNotification(
        "Preparing data file, save dialog will appear shortly...",
        type = "message",
        duration = 2
      )
      write.csv(simulated_data$data, file, row.names = FALSE)
    }
  )

  output$downloadDataButton <- renderUI({
    req(simulated_data$data)
    downloadButton('DownloadDataHandler', 'Data')
  })

  #download report button
  output$downloadReportHandler <- downloadHandler(
    filename = "simulation_report.html",
    content = function(file) {
      is_downloading_report(TRUE)
      on.exit(is_downloading_report(FALSE), add = TRUE)

      showNotification(
        "Preparing report, save dialog will appear shortly...",
        type = "message",
        duration = 3
      )

      shiny::withProgress(message = "Preparing report", value = 0, {
        incProgress(0.2, detail = "Copying template")
        tempReport <- normalizePath(file.path(tempdir(), "ShinySimulatorReport.Rmd"), mustWork = FALSE)
        file.copy(
          normalizePath(system.file("rmd", "ShinySimulatorReport.Rmd", package = "NetSimR")),
          tempReport,
          overwrite = TRUE
        )

        incProgress(0.5, detail = "Rendering report")
        rmarkdown::render(
          tempReport,
          output_file = file,
          quiet = TRUE,
          params = append(simulation_settings, list(total_claims_data = simulated_data$data$total_claims)),
          envir = new.env(parent = globalenv())
        )

        incProgress(1, detail = "Done")
      })
    }
  )
}
