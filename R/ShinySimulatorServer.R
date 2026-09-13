#' Server function for the Shiny Simulator application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Returns server rendering for the shiny application.
#' @import shiny
#' @import future.apply
#' @importFrom future plan
#' @importFrom future sequential
#' @importFrom future multisession
#' @import methods
#' @import stats
#' @import utils
shiny_simulator_server <- function(input, output, session) {
  #the future plan in effect when the app starts; the app puts it back when the session
  #ends or a run fails, and only shuts down workers that it started itself
  original_plan <- future::plan()
  app_started_workers <- FALSE
  restore_original_plan <- function() {
    if (!app_started_workers) return(invisible(FALSE))
    app_started_workers <<- FALSE
    future::plan(original_plan)
    invisible(TRUE)
  }
  session$onSessionEnded(restore_original_plan)

  #start the parallel workers once, when multiprocessing is switched on, so that every
  #run reuses the warm workers instead of paying the start-up cost each time;
  #a multi-worker plan that the user has set is used as it is
  start_parallel_workers <- function() {
    if (future::nbrOfWorkers() > 1) return(invisible(FALSE))
    showNotification("Starting parallel workers...", type = "message", duration = 3, id = "parallel_workers_notice")
    future::plan(future::multisession)
    app_started_workers <<- TRUE
    invisible(TRUE)
  }
  observeEvent(input$multiprocessingBinary, {
    if (isTRUE(input$multiprocessingBinary)) start_parallel_workers()
  })

  #seed input
  output$seed_value <- renderUI({
    if (input$seedSetBinary) {
      #set.seed() takes any whole number in the integer range
      numericInput('seedValue', 'Seed value', value = 1, min = -.Machine$integer.max,
                   max = .Machine$integer.max, step = 1)
    }
  })

  #implied mean and standard deviation of the chosen distributions, updated as parameters are typed
  distribution_inputs <- function(options, id) {
    lapply(options[[id]]@paramIDs, function(x) input[[x]])
  }
  freq_moments <- reactive({
    req(input$freqDistr)
    distribution_moments(freq_dist_options, input$freqDistr, distribution_inputs(freq_dist_options, input$freqDistr))
  })
  sev_moments <- reactive({
    req(input$sevDistr)
    distribution_moments(
      sev_dist_options, input$sevDistr, distribution_inputs(sev_dist_options, input$sevDistr)
      ,truncate_at_zero = isTRUE(input$sevTruncateAtZero) && input$sevDistr == "Normal"
    )
  })
  format_moment <- function(x) {
    if (is.infinite(x)) return("infinite")
    format(signif(x, 4), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  render_implied_moments <- function(moments) {
    if (anyNA(moments)) {
      return(div(class = "sim-implied sim-implied-empty", "Enter the parameters to see the implied mean"))
    }
    div(
      class = "sim-implied"
      ,"Implied mean ", tags$strong(format_moment(moments[["mean"]]))
      ,paste0(" ", intToUtf8(183), " "), "SD ", tags$strong(format_moment(moments[["sd"]]))
    )
  }
  output$freq_implied_moments <- renderUI(render_implied_moments(freq_moments()))
  output$sev_implied_moments <- renderUI(render_implied_moments(sev_moments()))

  #expected gross claims per period, E[N] * E[X]; hidden while either figure is unavailable
  output$expected_gross_claims <- renderUI({
    expected <- freq_moments()[["mean"]] * sev_moments()[["mean"]]
    if (is.na(expected)) return(NULL)
    div(
      class = "sim-expected"
      ,div(class = "sim-expected-label", "Expected gross claims per period")
      ,div(class = "sim-expected-value", format_moment(expected))
      ,div(class = "sim-expected-note", "before tail adjustments and reinsurance")
    )
  })

  #help function to render freq/sev parameters
  render_UI_param <- function(param_id, object){
    return(if(!is.na(object@paramIDs[param_id])){numericInput(
      inputId = object@paramIDs[param_id]
      ,label = object@param_labels[param_id]
      ,min = object@param_min_values[param_id]
      ,max = object@param_max_values[param_id]
      ,step = if (isTRUE(object@param_whole_numbers[param_id])) 1 else NA
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

  #Pareto slices: the hidden input pareto_slice_times holds the number of slices and decides
  #which slice rows the UI shows; the add and remove buttons change it, and saved settings
  #restore it like any other number
  pareto_slice_count <- reactive({
    n <- suppressWarnings(as.numeric(input$pareto_slice_times))
    if (length(n) != 1 || is.na(n)) 0 else min(max(round(n), 0), max_number_of_pareto_slices)
  })
  #slice j has its alpha in slice_pareto_param_(2j - 1) and its threshold in slice_pareto_param_(2j)
  slice_input_id <- function(slice, part) paste0("slice_pareto_param_", 2 * slice - (part == "alpha"))

  observeEvent(input$add_pareto_slice, {
    n <- pareto_slice_count()
    if (n < max_number_of_pareto_slices) updateNumericInput(session, "pareto_slice_times", value = n + 1)
  })

  lapply(seq_len(max_number_of_pareto_slices), function(i) {
    observeEvent(input[[paste0("remove_pareto_slice_", i)]], {
      n <- pareto_slice_count()
      if (i > n) return()
      #move the later slices up one place and clear the last one, so no values are lost
      for (j in seq_len(n - i) + i - 1) {
        for (part in c("alpha", "threshold")) {
          value <- input[[slice_input_id(j + 1, part)]]
          updateNumericInput(session, slice_input_id(j, part), value = if (is.null(value)) NA else value)
        }
      }
      for (part in c("alpha", "threshold")) updateNumericInput(session, slice_input_id(n, part), value = NA)
      updateNumericInput(session, "pareto_slice_times", value = n - 1)
    }, ignoreInit = TRUE)
  })

  #render severity cap amount
  output$sev_cap_amount_ui <- renderUI({
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

  output$reinsuranceStructureLimitedReinstatements_ui <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Limited Layer')) {
      checkboxInput('reinsuranceStructureLimitedReinstatements', 'Limited reinstatements', value = FALSE)
    }
  })

  output$reinsuranceStructureReinstatementLimit_ui <- renderUI({
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

  #save and load of the simulator settings, with built-in examples
  sim_settings_io_server(input, output, session)

  #create simulation data dataFrame reactive to enable download buttons & simulation settings list
  simulated_data <- reactiveValues(data=NULL)
  simulation_settings <- list()

  #the latest successful run (id, settings, data, finished), for the results tabs
  last_run <- reactiveVal(NULL)
  run_counter <- 0L

  #in-app Report and Compare tabs (R/ShinySimulatorTabs.R); both follow last_run
  sim_report_tab_server("report", last_run, reactive(input$sim_navbar))
  sim_compare_tab_server("compare", last_run)

  #run simulation button
  #the browser disables the button and shows "Running..." on click; this message re-enables it
  observeEvent(input$RunSimulations,{
    on.exit(session$sendCustomMessage("netsimr-run-finished", TRUE), add = TRUE)

    #collect the settings for this run
    slice_count <- pareto_slice_count()
    #sapply on purpose: it gives a numeric vector when every field is filled in, and a list
    #when a field is still empty (NULL), which find_missing_simulation_settings() reports by name
    new_settings <- list(
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
      ,paretoSlice = slice_count > 0
      ,pareto_slice_times = if (slice_count > 0) slice_count
      ,slice_pareto_alphas = if (slice_count > 0) unname(sapply(
        seq_len(slice_count)
        ,function(y) input[[slice_input_id(y, "alpha")]]
      ))
      ,slice_pareto_x_ms = if (slice_count > 0) unname(sapply(
        seq_len(slice_count)
        ,function(y) input[[slice_input_id(y, "threshold")]]
      ))
      ,sevCapBinary = input$sevCapBinary
      ,sev_cap_amount = input$sev_cap_amount
      ,reinsuranceStructureEEL = input$reinsuranceStructureEEL
      ,reinsurance_structure_eel_dedctible_amount = input$reinsurance_structure_eel_dedctible_amount
      ,reinsurance_structure_eel_limit_amount = input$reinsurance_structure_eel_limit_amount
      ,reinsuranceStructureAL = input$reinsuranceStructureAL
      ,reinsurance_structure_al_dedctible_amount = input$reinsurance_structure_al_dedctible_amount
      ,reinsurance_structure_al_limit_amount = input$reinsurance_structure_al_limit_amount
      ,multiprocessing = input$multiprocessingBinary
      ,sevTruncateAtZero = isTRUE(input$sevTruncateAtZero) && input$sevDistr == "Normal"
      ,reinsuranceStructureLimitedReinstatements = input$reinsuranceStructureLimitedReinstatements
      ,reinsuranceStructureReinstatementLimit = input$reinsuranceStructureReinstatementLimit
    )

    #stop before running and tell the user which fields are empty
    missing_settings <- find_missing_simulation_settings(new_settings)
    if (length(missing_settings) > 0) {
      showNotification(
        tags$div(
          tags$strong("Please fix these settings before running:"),
          tags$ul(lapply(missing_settings, tags$li))
        ),
        type = "error",
        duration = 10,
        id = "missing_settings_notice"
      )
      return(NULL)
    }
    removeNotification("missing_settings_notice")

    on.exit(gc(), add = TRUE)

    #workers may have been shut down after an earlier error; make sure they are up before a parallel run
    if (isTRUE(new_settings$multiprocessing)) start_parallel_workers()

    #run simmulations
    results <- tryCatch(
      {
        if (isTRUE(new_settings$multiprocessing)) {
          showNotification("Running the simulations on the parallel workers...", type = "message", duration = 3, id = "parallel_run_notice")
          do.call(simulate_function, new_settings)
        } else {
          #sequential runs report their progress chunk by chunk
          shiny::withProgress(message = "Simulating", value = 0, {
            do.call(simulate_function, c(new_settings, list(
              progress = function(value, detail) shiny::setProgress(value, detail = detail)
            )))
          })
        }
      }, error = function(cond) {
        #the workers may be broken after an error; shut down the ones the app started
        restore_original_plan()
        showNotification(
          paste("Error:", conditionMessage(cond)),
          type = "error",
          duration = NULL
        )
        message("Simulation failed: ", conditionMessage(cond))
        NULL
      }
    )

    #a failed run keeps the previous results, so the downloads and the results tabs still agree
    if (is.null(results)) return(NULL)

    #record the run for the downloads and the results tabs
    simulation_settings <<- new_settings
    simulated_data$data <- results
    run_counter <<- run_counter + 1L
    last_run(list(
      id = run_counter
      ,settings = simulation_settings
      ,data = simulated_data$data
      ,finished = Sys.time()
    ))
  })

  #the browser shows "Preparing report..." on click; the report handler re-enables the button
  output$downloadReportButton <- renderUI({
    req(simulated_data$data)
    downloadButton('downloadReportHandler', 'Report', icon = icon("file-lines"), class = "btn-outline-primary")
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
    downloadButton('DownloadDataHandler', 'CSV data', icon = icon("file-csv"), class = "btn-outline-primary")
  })

  #download report button
  output$downloadReportHandler <- downloadHandler(
    filename = "simulation_report.html",
    content = function(file) {
      on.exit(session$sendCustomMessage("netsimr-report-finished", TRUE), add = TRUE)

      showNotification(
        "Preparing report, save dialog will appear shortly...",
        type = "message",
        duration = 3
      )

      shiny::withProgress(message = "Preparing report", value = 0.3, detail = "Building report", {
        #the report is assembled in R (see write_simulation_report), so no pandoc is needed
        tryCatch(
          write_simulation_report(
            file = file,
            settings = simulation_settings,
            results = simulated_data$data
          ),
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

        incProgress(1, detail = "Done")
      })
    }
  )

  #keep dynamic outputs rendering even while their empty containers are collapsed by the UI styles
  dynamic_outputs <- c(
    freq_dist_parameter_placeholders$param_id
    ,sev_dist_parameter_placeholders$param_id
    ,"reinsuranceStructureDeductibleEEL", "reinsuranceStructureLimitEEL"
    ,"reinsuranceStructureDeductibleAL", "reinsuranceStructureLimitAL"
    ,"downloadDataButton", "downloadReportButton"
    ,"freq_implied_moments", "sev_implied_moments", "expected_gross_claims"
  )
  for (output_name in dynamic_outputs) {
    outputOptions(output, output_name, suspendWhenHidden = FALSE)
  }
}
