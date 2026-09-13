#save and load of the simulator settings, with built-in examples

#' Format tag written into every saved simulator settings file
#'
#' @noRd
sim_settings_format <- "NetSimR simulator settings"

#' Version of the saved settings format
#'
#' @noRd
sim_settings_version <- 2

#' Name of the file offered by the save button
#'
#' @noRd
sim_settings_file_name <- "netsimr_simulator_settings.rds"

#' Ids of every simulator input that a settings file can hold
#'
#' @return A character vector of input ids, including the dynamic inputs that only
#' exist for some options.
#' @noRd
sim_settings_input_ids <- function() {
  unique(c(
    "freqDistr", "sevDistr"
    ,unlist(lapply(freq_dist_options, function(x) x@paramIDs), use.names = FALSE)
    ,unlist(lapply(sev_dist_options, function(x) x@paramIDs), use.names = FALSE)
    ,"numberOfSimulations", "seedSetBinary", "seedValue", "multiprocessingBinary"
    ,"pareto_slice_times"
    ,paste0("slice_pareto_param_", seq_len(2 * max_number_of_pareto_slices))
    ,"sevCapBinary", "sev_cap_amount", "sevTruncateAtZero"
    ,"reinsuranceStructureEEL"
    ,"reinsurance_structure_eel_dedctible_amount", "reinsurance_structure_eel_limit_amount"
    ,"reinsuranceStructureLimitedReinstatements", "reinsuranceStructureReinstatementLimit"
    ,"reinsuranceStructureAL"
    ,"reinsurance_structure_al_dedctible_amount", "reinsurance_structure_al_limit_amount"
  ))
}

#' Build the list that a settings file contains
#'
#' @param input_values A named list (or the shiny input object) with the current
#' values of the simulator inputs. Ids that are not simulator inputs are ignored and
#' NULL values are skipped.
#' @return A list with the fields \code{format}, \code{version}, \code{saved} and
#' \code{inputs}, the last one a named list of the input values.
#' @noRd
sim_settings_collect <- function(input_values) {
  values <- list()
  for (id in sim_settings_input_ids()) {
    value <- input_values[[id]]
    if (!is.null(value)) values[[id]] <- value
  }
  list(
    format = sim_settings_format
    ,version = sim_settings_version
    ,saved = Sys.time()
    ,inputs = values
  )
}

#' Check that an object is a readable simulator settings list
#'
#' @param x The object read from a settings file.
#' @return TRUE when the settings can be loaded, otherwise a character error message.
#' @noRd
sim_settings_validate <- function(x) {
  not_settings <- "This is not a NetSimR simulator settings file."
  if (!is.list(x) || is.data.frame(x) || is.null(names(x))) return(not_settings)
  if (!identical(x$format, sim_settings_format)) return(not_settings)
  version <- x$version
  if (!is.numeric(version) || length(version) != 1 || is.na(version)) {
    return("The settings file has no valid version number.")
  }
  if (version > sim_settings_version) {
    return(paste0(
      "The settings file was saved by a newer version of NetSimR (settings version "
      ,version, ") and cannot be loaded by this version."
    ))
  }
  inputs <- x$inputs
  if (!is.list(inputs) || is.data.frame(inputs)) return("The settings file holds no inputs.")
  if (length(inputs) > 0 && (is.null(names(inputs)) || any(is.na(names(inputs)) | names(inputs) == ""))) {
    return("The settings file holds inputs without names.")
  }
  #the choice inputs must hold values the app offers
  check_choice <- function(id, choices, label) {
    value <- inputs[[id]]
    if (is.null(value)) return(NULL)
    if (!is.character(value) || length(value) != 1 || !(value %in% choices)) {
      return(paste0("The settings file has an unknown ", label, ": '", paste(value, collapse = ", "), "'."))
    }
    NULL
  }
  for (problem in list(
    check_choice("freqDistr", names(freq_dist_options), "frequency distribution")
    ,check_choice("sevDistr", names(sev_dist_options), "severity distribution")
    ,check_choice("reinsuranceStructureEEL", reinsurance_structures_options, "EEL reinsurance structure")
    ,check_choice("reinsuranceStructureAL", reinsurance_structures_options, "AL reinsurance structure")
  )) {
    if (!is.null(problem)) return(problem)
  }
  TRUE
}

#' Bring the inputs of an older settings file up to date
#'
#' Settings version 1 stored the Normal severity's mean and standard deviation under the
#' Log-Normal's ids (mu, sigma); from version 2 the Normal has its own ids.
#'
#' @param inputs The named list of saved input values.
#' @param version The settings version of the file.
#' @return The inputs, with old ids renamed.
#' @noRd
sim_settings_migrate <- function(inputs, version) {
  #older files have a switch that turned the Pareto slices on; now the number of slices does
  if ("paretoSlice" %in% names(inputs)) {
    if (!isTRUE(inputs$paretoSlice)) inputs$pareto_slice_times <- 0
    inputs$paretoSlice <- NULL
  }
  if (is.numeric(version) && length(version) == 1 && !is.na(version) && version < 2 &&
      identical(inputs$sevDistr, "Normal")) {
    if (is.null(inputs$normal_mean)) inputs$normal_mean <- inputs$mu
    if (is.null(inputs$normal_sd)) inputs$normal_sd <- inputs$sigma
    inputs$mu <- NULL
    inputs$sigma <- NULL
  }
  inputs
}

#' Compare an input value with the value a settings file asks for
#'
#' @param current The current value of the input.
#' @param target The value from the settings file, whose type decides the comparison.
#' @return TRUE when the two values agree.
#' @noRd
sim_settings_values_match <- function(current, target) {
  if (is.null(current) || is.null(target)) return(FALSE)
  if (length(current) != length(target)) return(FALSE)
  if (is.numeric(target)) {
    a <- suppressWarnings(as.numeric(current))
    b <- as.numeric(target)
    if (!identical(is.na(a), is.na(b))) return(FALSE)
    return(isTRUE(all.equal(a[!is.na(a)], b[!is.na(b)])))
  }
  if (is.logical(target)) return(identical(as.logical(current), target))
  identical(as.character(current), as.character(target))
}

#' Send one input update to the browser
#'
#' @param session The shiny session.
#' @param id The input id.
#' @param kind The kind of input: "radio", "switch", "checkbox", "select" or "numeric".
#' @param value The value to set.
#' @noRd
sim_settings_apply_update <- function(session, id, kind, value) {
  switch(
    kind
    ,radio = updateRadioButtons(session, id, selected = as.character(value))
    ,switch = bslib::update_switch(id, value = isTRUE(as.logical(value)), session = session)
    ,checkbox = updateCheckboxInput(session, id, value = isTRUE(as.logical(value)))
    ,select = updateSelectInput(session, id, selected = as.character(value))
    ,numeric = updateNumericInput(session, id, value = suppressWarnings(as.numeric(value)))
    ,stop("Unknown input kind: ", kind)
  )
  invisible(NULL)
}

#' Plan how to restore a list of saved inputs
#'
#' The controls that are always on the page are applied at once. The fields that the
#' server renders only for some choices are applied later, once the choices they
#' depend on have reached the browser and the fields exist.
#'
#' @param inputs The named list of saved input values.
#' @return A list of entries with \code{id}, \code{kind}, \code{value} and, for the
#' fields applied later, a \code{gate} function of the input object that returns TRUE
#' once the field can be set. Saved values that the chosen options do not use are left out.
#' @noRd
sim_settings_plan <- function(inputs) {
  layers_with_deductible <- c("Unlimited Layer", "Limited Layer", "Exclude Layer")
  layers_with_limit <- c("Limited Layer", "Exclude Layer")
  same <- sim_settings_values_match
  entries <- list()
  add <- function(id, kind, gate = NULL) {
    value <- inputs[[id]]
    if (is.null(value) || length(value) == 0) return(invisible(NULL))
    if (length(value) == 1 && is.na(value)) return(invisible(NULL))
    entries[[length(entries) + 1]] <<- list(id = id, kind = kind, value = value, gate = gate)
    invisible(NULL)
  }

  #stage one: the controls that decide which fields exist
  add("freqDistr", "radio")
  add("sevDistr", "radio")
  add("reinsuranceStructureEEL", "radio")
  add("reinsuranceStructureAL", "radio")
  #the truncation switch waits for the Normal severity, in stage two
  for (id in c("seedSetBinary", "multiprocessingBinary", "sevCapBinary")) {
    add(id, "switch")
  }
  add("numberOfSimulations", "numeric")

  #stage two: the fields rendered for the chosen options
  freq <- inputs$freqDistr
  if (is.character(freq) && length(freq) == 1 && freq %in% names(freq_dist_options)) {
    for (id in freq_dist_options[[freq]]@paramIDs) {
      add(id, "numeric", gate = function(input) same(input$freqDistr, freq))
    }
  }
  sev <- inputs$sevDistr
  if (is.character(sev) && length(sev) == 1 && sev %in% names(sev_dist_options)) {
    for (id in sev_dist_options[[sev]]@paramIDs) {
      add(id, "numeric", gate = function(input) same(input$sevDistr, sev))
    }
    #the truncation switch is only shown for the Normal severity
    if (sev == "Normal") {
      add("sevTruncateAtZero", "switch", gate = function(input) same(input$sevDistr, sev))
    }
  }
  if (isTRUE(inputs$seedSetBinary)) {
    add("seedValue", "numeric", gate = function(input) isTRUE(input$seedSetBinary))
  }
  #the slice rows are always in the page; the number of slices decides which are shown
  slices <- suppressWarnings(as.numeric(inputs$pareto_slice_times))
  if (length(slices) == 1 && !is.na(slices)) {
    slices <- min(max(round(slices), 0), max_number_of_pareto_slices)
    add("pareto_slice_times", "numeric")
    for (i in seq_len(2 * slices)) add(paste0("slice_pareto_param_", i), "numeric")
  }
  if (isTRUE(inputs$sevCapBinary)) {
    add("sev_cap_amount", "numeric", gate = function(input) isTRUE(input$sevCapBinary))
  }
  eel <- inputs$reinsuranceStructureEEL
  if (is.character(eel) && length(eel) == 1) {
    eel_gate <- function(input) same(input$reinsuranceStructureEEL, eel)
    if (eel %in% layers_with_deductible) add("reinsurance_structure_eel_dedctible_amount", "numeric", gate = eel_gate)
    if (eel %in% layers_with_limit) add("reinsurance_structure_eel_limit_amount", "numeric", gate = eel_gate)
    if (eel == "Limited Layer") {
      add("reinsuranceStructureLimitedReinstatements", "checkbox", gate = eel_gate)
      if (isTRUE(inputs$reinsuranceStructureLimitedReinstatements)) {
        add(
          "reinsuranceStructureReinstatementLimit", "numeric"
          ,gate = function(input) eel_gate(input) && isTRUE(input$reinsuranceStructureLimitedReinstatements)
        )
      }
    }
  }
  al <- inputs$reinsuranceStructureAL
  if (is.character(al) && length(al) == 1) {
    al_gate <- function(input) same(input$reinsuranceStructureAL, al)
    if (al %in% layers_with_deductible) add("reinsurance_structure_al_dedctible_amount", "numeric", gate = al_gate)
    if (al %in% layers_with_limit) add("reinsurance_structure_al_limit_amount", "numeric", gate = al_gate)
  }
  entries
}

#' Built-in example settings for the simulator
#'
#' Each example is a named list of input values, in the same form as the
#' \code{inputs} field of a saved settings file.
#'
#' @noRd
sim_settings_examples <- list(
  "Motor: excess of loss layer" = list(
    freqDistr = "Poisson", lamda = 5
    ,sevDistr = "LogNormal", mu = 9, sigma = 1.3
    ,numberOfSimulations = 50000
    ,seedSetBinary = TRUE, seedValue = 1
    ,multiprocessingBinary = FALSE
    ,pareto_slice_times = 0
    ,sevCapBinary = FALSE
    ,sevTruncateAtZero = FALSE
    ,reinsuranceStructureEEL = "Limited Layer"
    ,reinsurance_structure_eel_dedctible_amount = 50000
    ,reinsurance_structure_eel_limit_amount = 200000
    ,reinsuranceStructureLimitedReinstatements = TRUE
    ,reinsuranceStructureReinstatementLimit = 2
    ,reinsuranceStructureAL = "No Reinsurance Structure"
  )
  ,"Property: Pareto tail and aggregate cover" = list(
    freqDistr = "Negative_Binomial", r = 4, beta = 2.5
    ,sevDistr = "Gamma", shape = 1.5, scale = 20000
    ,numberOfSimulations = 50000
    ,seedSetBinary = FALSE
    ,multiprocessingBinary = FALSE
    ,pareto_slice_times = 1
    ,slice_pareto_param_1 = 1.8, slice_pareto_param_2 = 150000
    ,sevCapBinary = TRUE, sev_cap_amount = 5000000
    ,sevTruncateAtZero = FALSE
    ,reinsuranceStructureEEL = "No Reinsurance Structure"
    ,reinsuranceStructureAL = "Limited Layer"
    ,reinsurance_structure_al_dedctible_amount = 1000000
    ,reinsurance_structure_al_limit_amount = 2000000
  )
  ,"Simple: Normal claims truncated at zero" = list(
    freqDistr = "Poisson", lamda = 3
    ,sevDistr = "Normal", normal_mean = 1000, normal_sd = 600
    ,sevTruncateAtZero = TRUE
    ,numberOfSimulations = 20000
    ,seedSetBinary = FALSE
    ,multiprocessingBinary = FALSE
    ,pareto_slice_times = 0
    ,sevCapBinary = FALSE
    ,reinsuranceStructureEEL = "No Reinsurance Structure"
    ,reinsuranceStructureAL = "No Reinsurance Structure"
  )
)

#' Styles for the settings block of the Simulation card
#'
#' @noRd
sim_settings_io_css <- "
.sim-settings-io .shiny-input-container {
  margin-bottom: 0.6rem;
}

.sim-settings-io .btn-sm {
  font-size: 0.82rem;
  padding: 0.4rem 0.8rem;
}

.sim-settings-io .shiny-download-link {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 0.4rem;
  width: 100%;
  margin-bottom: 0.75rem;
}

.sim-settings-io .input-group .btn {
  font-weight: 600;
  font-size: 0.86rem;
  border-color: var(--sim-border-strong);
  border-radius: 10px 0 0 10px;
}

.sim-settings-io .input-group .form-control {
  font-size: 0.86rem;
  border-radius: 0 10px 10px 0;
}

.sim-settings-io .shiny-file-input-progress {
  height: 6px;
  margin-top: 0.3rem;
  margin-bottom: 0;
  border-radius: 999px;
}

.sim-settings-io .shiny-file-input-progress .progress-bar {
  font-size: 0;
}

.sim-settings-example {
  display: grid;
  grid-template-columns: minmax(0, 1fr) auto;
  gap: 0.5rem;
  align-items: end;
}

.sim-settings-example .shiny-input-container {
  margin-bottom: 0;
}

.sim-settings-example .form-select {
  font-size: 0.86rem;
  padding: 0.42rem 2rem 0.42rem 0.75rem;
}

.sim-settings-example .btn {
  white-space: nowrap;
}
"

#' Settings block for the Simulation card
#'
#' Save the current settings to a file, load a saved file, or load a built-in example.
#'
#' @return A tag list to place inside the Simulation card of the simulator UI.
#' @noRd
sim_settings_io_ui <- function() {
  div(
    class = "sim-settings-io",
    tags$style(HTML(sim_settings_io_css)),
    div(class = "sim-section-label", "Settings"),
    downloadButton(
      "settingsIO_save", "Save settings"
      ,icon = icon("download"), class = "btn-outline-primary btn-sm"
    ),
    fileInput(
      "settingsIO_load", "Load settings"
      ,accept = ".rds", width = "100%"
      ,buttonLabel = tagList(icon("folder-open"), "Browse")
      ,placeholder = "No file selected"
    ),
    div(
      class = "sim-settings-example",
      selectInput(
        "settingsIO_example", "Examples"
        ,choices = names(sim_settings_examples), selectize = FALSE, width = "100%"
      ),
      actionButton(
        "settingsIO_load_example", "Load example"
        ,icon = icon("wand-magic-sparkles"), class = "btn-outline-primary btn-sm"
      )
    ),
    helpText("Settings are saved as an .rds file that can be loaded back later.")
  )
}

#' Server logic for saving and loading the simulator settings
#'
#' Called inside \code{shiny_simulator_server} with the main input, output and
#' session objects, so it can read and update the simulator inputs by their ids.
#'
#' Loading happens in two stages. The controls that are always on the page are updated
#' at once. The fields that the server renders only for some choices are kept pending
#' and set once the choices they depend on have reached the browser and the fields
#' exist. A value is treated as done once the input has held it for a moment, so a
#' field that the browser re-renders after the update is set again. Pending values for
#' fields that never appear are dropped quietly after the timeout in
#' \code{getOption("netsimr.settings_io_timeout", 6)} seconds.
#'
#' @param input Input of the simulator server function.
#' @param output Output of the simulator server function.
#' @param session Session of the simulator server function.
#' @return Called for its side effects.
#' @noRd
sim_settings_io_server <- function(input, output, session) {
  #how long to wait for dynamic fields, and how to retry a value the browser reset
  timeout_seconds <- getOption("netsimr.settings_io_timeout", 6)
  retry_seconds <- 0.5
  max_attempts <- 3
  settle_seconds <- 0.5
  poll_millis <- 250

  #save the current settings
  output$settingsIO_save <- downloadHandler(
    filename = function() sim_settings_file_name,
    content = function(file) {
      ids <- sim_settings_input_ids()
      values <- isolate(stats::setNames(lapply(ids, function(id) input[[id]]), ids))
      saveRDS(sim_settings_collect(values), file)
    }
  )

  #values waiting for the fields they belong to
  pending <- reactiveVal(list())
  deadline <- NULL

  load_settings <- function(settings, loaded_message, error_prefix) {
    problem <- sim_settings_validate(settings)
    if (!isTRUE(problem)) {
      showNotification(paste(error_prefix, problem), type = "error", duration = 8)
      return(invisible(FALSE))
    }
    entries <- sim_settings_plan(sim_settings_migrate(settings$inputs, settings$version))
    is_pending <- vapply(entries, function(entry) !is.null(entry$gate), logical(1))

    #stage one: the controls that decide which fields exist
    for (entry in entries[!is_pending]) {
      sim_settings_apply_update(session, entry$id, entry$kind, entry$value)
    }

    #stage two: the fields that appear once the browser has applied stage one
    later <- lapply(entries[is_pending], function(entry) {
      entry$attempts <- 0L
      entry$sent_at <- NULL
      entry$matched_at <- NULL
      entry
    })
    deadline <<- Sys.time() + timeout_seconds
    pending(later)

    showNotification(loaded_message, type = "message", duration = 5)
    invisible(TRUE)
  }

  observe({
    entries <- pending()
    if (length(entries) == 0) return(invisible(NULL))
    now <- Sys.time()
    if (!is.null(deadline) && now > deadline) {
      #give up quietly on fields that never appeared
      pending(list())
      return(invisible(NULL))
    }
    invalidateLater(poll_millis)

    changed <- FALSE
    keep <- list()
    for (entry in entries) {
      if (!isTRUE(entry$gate(input))) {
        keep[[length(keep) + 1]] <- entry
        next
      }
      current <- input[[entry$id]]
      if (is.null(current)) {
        #the field is not on the page yet
        keep[[length(keep) + 1]] <- entry
        next
      }
      if (sim_settings_values_match(current, entry$value)) {
        #done once the value has held for a moment, in case the browser re-renders the field
        if (is.null(entry$matched_at)) {
          entry$matched_at <- now
          changed <- TRUE
        } else if (as.numeric(now - entry$matched_at, units = "secs") >= settle_seconds) {
          changed <- TRUE
          next
        }
        keep[[length(keep) + 1]] <- entry
        next
      }
      entry$matched_at <- NULL
      waited <- if (is.null(entry$sent_at)) Inf else as.numeric(now - entry$sent_at, units = "secs")
      if (entry$attempts < max_attempts && waited >= retry_seconds) {
        sim_settings_apply_update(session, entry$id, entry$kind, entry$value)
        entry$attempts <- entry$attempts + 1L
        entry$sent_at <- now
        changed <- TRUE
      }
      keep[[length(keep) + 1]] <- entry
    }
    if (changed) pending(keep)
  })

  #load settings from a file
  observeEvent(input$settingsIO_load, {
    file_info <- input$settingsIO_load
    req(file_info$datapath)
    file_label <- paste0("'", file_info$name, "'")
    read_failed <- FALSE
    settings <- tryCatch(
      readRDS(file_info$datapath)
      ,error = function(cond) { read_failed <<- TRUE; NULL }
      ,warning = function(cond) { read_failed <<- TRUE; NULL }
    )
    if (read_failed) {
      showNotification(
        paste0(file_label, " could not be read. Please choose an .rds file saved with 'Save settings'.")
        ,type = "error", duration = 8
      )
      return(invisible(NULL))
    }
    load_settings(
      settings
      ,loaded_message = paste0("Settings loaded from ", file_label, ".")
      ,error_prefix = paste0(file_label, ":")
    )
  })

  #load a built-in example
  observeEvent(input$settingsIO_load_example, {
    label <- input$settingsIO_example
    example <- if (is.character(label) && length(label) == 1) sim_settings_examples[[label]] else NULL
    if (is.null(example)) {
      showNotification("Please choose an example to load.", type = "error", duration = 8)
      return(invisible(NULL))
    }
    load_settings(
      sim_settings_collect(example)
      ,loaded_message = paste0("Loaded example '", label, "'.")
      ,error_prefix = paste0("Example '", label, "':")
    )
  })

  invisible(NULL)
}
