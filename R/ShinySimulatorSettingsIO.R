#save and load of the simulator settings, with built-in examples

#' Tool name written into every saved simulator settings file
#'
#' Settings files are plain text, written and read by write_settings_file() and
#' read_settings_file() (R/settings_file.R). A file of another tool, or an .rds settings
#' file of NetSimR 0.3.0 and before, is refused with a message.
#' @noRd
sim_settings_tool <- "claims simulator"

#' Version of the saved settings format
#'
#' Versions 1 and 2 were .rds files, which are no longer read; version 3 is the text file.
#' @noRd
sim_settings_version <- 3

#' Name of the file offered by the save button
#'
#' @noRd
sim_settings_file_name <- "claims_simulator_settings.txt"

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

#' Collect the input values that a settings file holds
#'
#' @param input_values A named list (or the shiny input object) with the current
#' values of the simulator inputs. Ids that are not simulator inputs are ignored, and so
#' are NULL and empty values.
#' @return A named list with one vector of numbers, strings or logicals per input id, as
#' write_settings_file() writes them (one "id: value" line each).
#' @noRd
sim_settings_collect <- function(input_values) {
  values <- list()
  for (id in sim_settings_input_ids()) {
    value <- input_values[[id]]
    if (is.null(value) || length(value) == 0 || !is.atomic(value)) next
    values[[id]] <- value
  }
  values
}

#' Write the settings file that the save button offers
#'
#' @param input_values The current input values, as for sim_settings_collect().
#' @param file Path of the text file to write.
#' @noRd
sim_settings_write <- function(input_values, file) {
  write_settings_file(sim_settings_collect(input_values), file, tool = sim_settings_tool, version = sim_settings_version)
}

#' Read a saved settings file
#'
#' Reading never evaluates code (see read_settings_file()).
#' @param file Path of the file.
#' @return A list with \code{version} and \code{inputs}, the named list of saved input
#' values; or a character error message for a file that is not a settings file of the
#' simulator (an .rds file, say, or one saved by another tool) or holds a value that
#' cannot be read.
#' @noRd
sim_settings_read <- function(file) {
  tryCatch({
    settings <- read_settings_file(file, tool = sim_settings_tool)
    list(version = settings$version, inputs = settings$values)
  }, error = function(cond) conditionMessage(cond))
}

#' Check that saved inputs can be loaded
#'
#' @param inputs The named list of saved input values.
#' @param version The settings version of the file.
#' @return TRUE when the settings can be loaded, otherwise a character error message.
#' @noRd
sim_settings_validate <- function(inputs, version = sim_settings_version) {
  if (!is.numeric(version) || length(version) != 1 || is.na(version)) {
    return("The settings file has no valid version number.")
  }
  if (version > sim_settings_version) {
    return(paste0(
      "The settings file was saved by a newer version of NetSimR (settings version "
      ,version, ") and cannot be loaded by this version."
    ))
  }
  if (version < sim_settings_version) {
    return(paste0("The settings file has settings version ", version, ", which this version of NetSimR no longer loads."))
  }
  if (!is.list(inputs) || is.data.frame(inputs)) return("The settings file holds no inputs.")
  if (length(inputs) > 0 && (is.null(names(inputs)) || any(is.na(names(inputs)) | names(inputs) == ""))) {
    return("The settings file holds inputs without names.")
  }
  if (!all(vapply(inputs, function(x) is.null(x) || is.atomic(x), logical(1)))) {
    return("The settings file holds inputs that are not numbers, text or TRUE/FALSE values.")
  }
  #every input holds one value; a vector (an edited file, or one built to be huge) would be
  #pushed to the browser as it is
  several <- names(inputs)[lengths(inputs) > 1]
  if (length(several) > 0) {
    return(paste0("The settings file holds inputs with more than one value: '", paste(several, collapse = "', '"), "'."))
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

#' Show a message about saving or loading settings
#'
#' @param message The text to show.
#' @param type "message" or "error"; errors stay on screen for longer.
#' @noRd
sim_settings_notify <- function(message, type = "message") {
  showNotification(message, type = type, duration = if (identical(type, "error")) 8 else 5)
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
#' once the field can be set. Saved values that the chosen options do not use are left out,
#' except two that stay in the page and would otherwise keep the values of earlier settings:
#' the truncation switch is set to the file's value (FALSE when the file has none, e.g. for
#' another severity), and the slice fields beyond the loaded number of slices are cleared.
#' @noRd
sim_settings_plan <- function(inputs) {
  layers_with_deductible <- c("Unlimited Layer", "Limited Layer", "Exclude Layer")
  layers_with_limit <- c("Limited Layer", "Exclude Layer")
  same <- sim_settings_values_match
  entries <- list()
  add <- function(id, kind, gate = NULL, value = inputs[[id]]) {
    if (is.null(value) || length(value) == 0) return(invisible(NULL))
    if (length(value) == 1 && is.na(value)) return(invisible(NULL))
    entries[[length(entries) + 1]] <<- list(id = id, kind = kind, value = value, gate = gate)
    invisible(NULL)
  }
  #empties a numeric field that is always in the page
  clear <- function(id) {
    entries[[length(entries) + 1]] <<- list(id = id, kind = "numeric", value = NA_real_, gate = NULL)
    invisible(NULL)
  }

  #stage one: the controls that decide which fields exist
  add("freqDistr", "radio")
  add("sevDistr", "radio")
  add("reinsuranceStructureEEL", "radio")
  add("reinsuranceStructureAL", "radio")
  for (id in c("seedSetBinary", "multiprocessingBinary", "sevCapBinary")) {
    add(id, "switch")
  }
  #the truncation switch is always in the page (shown for the Normal only), so it is set at
  #once; a file without it, or saved with another severity, switches it off, so that an
  #earlier Normal's truncation does not reappear when the Normal is picked later
  add("sevTruncateAtZero", "switch", value = isTRUE(as.logical(inputs$sevTruncateAtZero)))
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
  }
  if (isTRUE(inputs$seedSetBinary)) {
    add("seedValue", "numeric", gate = function(input) isTRUE(input$seedSetBinary))
  }
  #the slice rows are always in the page; the number of slices decides which are shown
  slices <- suppressWarnings(as.numeric(inputs$pareto_slice_times))
  if (length(slices) == 1 && !is.na(slices)) {
    slices <- min(max(round(slices), 0), max_number_of_pareto_slices)
    add("pareto_slice_times", "numeric", value = slices)
    for (i in seq_len(2 * slices)) add(paste0("slice_pareto_param_", i), "numeric")
    #the later slice fields are emptied, so 'Add slice' does not bring back earlier values
    for (i in setdiff(seq_len(2 * max_number_of_pareto_slices), seq_len(2 * slices))) clear(paste0("slice_pareto_param_", i))
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
#' Each example is a named list of input values, in the same form as the inputs read
#' from a saved settings file.
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
  #names are kept short enough to show in full in the select at every width
  ,"Property: aggregate cover" = list(
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
  ,"Simple: truncated Normal" = list(
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

/* the select has the full width of the card, so example names are not cut off, and the
   button sits under it */
.sim-settings-example {
  display: grid;
  grid-template-columns: minmax(0, 1fr);
  gap: 0.5rem;
}

.sim-settings-example .shiny-input-container {
  margin-bottom: 0;
}

.sim-settings-example .form-select {
  font-size: 0.86rem;
  padding: 0.42rem 2rem 0.42rem 0.75rem;
  text-overflow: ellipsis;
}

.sim-settings-example .btn {
  white-space: nowrap;
  width: 100%;
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
      ,accept = c(".txt", "text/plain"), width = "100%"
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
    helpText("Settings are saved as a plain text (.txt) file that can be loaded back later.")
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
#' exist. Their values are also handed to \code{remember} at once, so a field that the
#' server builds (or rebuilds) after the load starts from the loaded value, however slow
#' the browser is. A pending value is done as soon as the input holds it; from then on
#' the field is the user's, and a value typed after the load is never set back. An update
#' that the browser lost (the field still has the value it had when the update was sent)
#' is sent again; any other value is taken as typed by the user and kept. Pending values
#' for fields that never appear are dropped quietly after the timeout in
#' \code{getOption("netsimr.settings_io_timeout", 6)} seconds.
#'
#' @param input Input of the simulator server function.
#' @param output Output of the simulator server function.
#' @param session Session of the simulator server function.
#' @param remember NULL, or a function of an input id and a value that stores the value
#' the server builds that field from (the simulator's remembered typed values).
#' @param apply NULL, or a function of an input id, kind and value that sends the update
#' to the browser in place of \code{sim_settings_apply_update}, for the inputs whose sent
#' values the server keeps track of itself (the number of slices).
#' @return Called for its side effects.
#' @noRd
sim_settings_io_server <- function(input, output, session, remember = NULL, apply = NULL) {
  #how long to wait for dynamic fields, and how to retry an update the browser lost
  timeout_seconds <- getOption("netsimr.settings_io_timeout", 6)
  retry_seconds <- 0.5
  max_attempts <- 3
  poll_millis <- 250
  apply_update <- function(id, kind, value) {
    if (is.function(apply)) apply(id, kind, value) else sim_settings_apply_update(session, id, kind, value)
  }

  #save the current settings
  output$settingsIO_save <- downloadHandler(
    filename = function() sim_settings_file_name,
    content = function(file) {
      ids <- sim_settings_input_ids()
      values <- isolate(stats::setNames(lapply(ids, function(id) input[[id]]), ids))
      sim_settings_write(values, file)
    }
  )

  #values waiting for the fields they belong to
  pending <- reactiveVal(list())
  deadline <- NULL

  load_settings <- function(inputs, version, loaded_message, error_prefix) {
    problem <- sim_settings_validate(inputs, version)
    if (!isTRUE(problem)) {
      sim_settings_notify(paste(error_prefix, problem), type = "error")
      return(invisible(FALSE))
    }
    entries <- sim_settings_plan(inputs)
    is_pending <- vapply(entries, function(entry) !is.null(entry$gate), logical(1))

    #stage one: the controls that decide which fields exist
    for (entry in entries[!is_pending]) {
      apply_update(entry$id, entry$kind, entry$value)
    }

    #stage two: the fields that appear once the browser has applied stage one
    later <- lapply(entries[is_pending], function(entry) {
      entry$attempts <- 0L
      entry$sent_at <- NULL
      entry$before <- NULL
      entry
    })
    #a field built after this point (its choice reaches the server later, or the browser
    #is slow and the pending value times out) starts from the loaded value
    if (is.function(remember)) {
      for (entry in later) {
        value <- switch(
          entry$kind
          ,numeric = suppressWarnings(as.numeric(entry$value))
          ,checkbox = isTRUE(as.logical(entry$value))
          ,entry$value
        )
        remember(entry$id, value)
      }
    }
    deadline <<- Sys.time() + timeout_seconds
    pending(later)

    sim_settings_notify(loaded_message)
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
        #done: a field rebuilt later starts from the remembered value, and a value the
        #user types from now on must not be set back (it was, on slow machines, when a
        #value that had just arrived had to hold for a moment before it counted)
        changed <- TRUE
        next
      }
      if (!is.null(entry$sent_at) && !sim_settings_values_match(current, entry$before)) {
        #the field changed since the update was sent, to another value: the user typed it
        changed <- TRUE
        next
      }
      waited <- if (is.null(entry$sent_at)) Inf else as.numeric(now - entry$sent_at, units = "secs")
      if (entry$attempts < max_attempts && waited >= retry_seconds) {
        #the first update, or again when the browser lost it (the field kept its old value)
        apply_update(entry$id, entry$kind, entry$value)
        entry$attempts <- entry$attempts + 1L
        entry$sent_at <- now
        entry$before <- current
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
    #an .rds file or one of another tool gets the reader's message, e.g. "This is not a
    #NetSimR settings file."
    settings <- sim_settings_read(file_info$datapath)
    if (is.character(settings)) {
      sim_settings_notify(
        paste0(file_label, ": ", settings, " Please choose a .txt file saved with 'Save settings' in this app.")
        ,type = "error"
      )
      return(invisible(NULL))
    }
    load_settings(
      settings$inputs, settings$version
      ,loaded_message = paste0("Settings loaded from ", file_label, ".")
      ,error_prefix = paste0(file_label, ":")
    )
  })

  #load a built-in example
  observeEvent(input$settingsIO_load_example, {
    label <- input$settingsIO_example
    example <- if (is.character(label) && length(label) == 1) sim_settings_examples[[label]] else NULL
    if (is.null(example)) {
      sim_settings_notify("Please choose an example to load.", type = "error")
      return(invisible(NULL))
    }
    load_settings(
      sim_settings_collect(example), sim_settings_version
      ,loaded_message = paste0("Loaded example '", label, "'.")
      ,error_prefix = paste0("Example '", label, "':")
    )
  })

  invisible(NULL)
}
