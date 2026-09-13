#' A function to run the GLM fitting tool application
#'
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   shiny::runApp().
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_glm_fitting_tool()
#' }
run_shiny_glm_fitting_tool <- function(){
  shinyApp(ui = GLMFittingToolUI, server = GLMFittingToolServer, onStart = shiny_tool_on_start)
}

# Maximum upload size (in bytes) accepted by the file inputs of the GLM and
# distribution fitting tools. Shiny's default is 5 MB, which is too small for
# claims datasets; 64 GB was the limit introduced in version 0.1.5 and is kept
# so that no previously accepted file is rejected. In practice the available
# memory limits what read.csv() can load, not this setting.
shiny_tool_max_request_size <- 64 * 1024^3

# onStart hook shared by the GLM and distribution fitting tools: raises the
# upload limit only while the app runs and restores the user's option when it
# stops, so that loading the package does not change the R session.
shiny_tool_on_start <- function() {
  old_options <- options(shiny.maxRequestSize = shiny_tool_max_request_size)
  onStop(function() options(old_options))
}

# Inputs of the GLM fitting tool that its settings file keeps, with how each is
# restored. Passwords and uploaded files are never kept; "column" inputs refer
# to columns of the data and are applied once data with that column is imported.
glm_settings_inputs <- data.frame(
  id = c("data_source", "csv_header", "csv_sep", "csv_dec", "csv_quote",
         "db_type", "db_host", "db_name", "db_port", "windows_auth", "db_user", "sql_query",
         "glm_distribution", "link_function", "offset_log", "formula", "number_of_bands_input", "band_method",
         "response_variable", "offset", "weights", "visualize_variable"),
  kind = c("radio", "switch", "radio", "radio", "radio",
           "select", "text", "text", "text", "switch", "text", "textarea",
           "select", "select", "switch", "textarea", "slider", "radio",
           "column", "column", "column", "column"),
  stringsAsFactors = FALSE
)

glm_settings_tool <- "NetSimR GLM fitting tool"

# Allowed values of the inputs with a fixed set of choices.
glm_settings_choices <- list(
  data_source = c("CSV File", "Database"),
  csv_sep = c(",", ";", "\t"),
  csv_dec = c(".", ","),
  csv_quote = c("\"", "'", ""),
  db_type = c("MySQL", "SQLite", "SQL Server", "PostgreSQL"),
  glm_distribution = c("gaussian", "poisson", "binomial", "Gamma", "inverse.gaussian"),
  link_function = c("identity", "log", "inverse", "sqrt", "logit", "probit", "cloglog", "cauchit", "1/mu^2"),
  band_method = c("quantile", "width")
)

# The usable values of a settings file: a named list of the known inputs whose
# values have the right type (and an allowed value), or NULL when the file is not
# a settings file. Files saved by earlier versions (every input of the app) are
# read too; anything unknown, such as a password, is ignored.
glm_settings_values <- function(settings) {
  if (!is.list(settings)) return(NULL)
  inputs <- if (identical(settings$tool, glm_settings_tool)) settings$inputs else settings
  if (!is.list(inputs) || is.null(names(inputs))) return(NULL)
  kinds <- stats::setNames(glm_settings_inputs$kind, glm_settings_inputs$id)
  values <- list()
  for (id in intersect(names(inputs), names(kinds))) {
    value <- inputs[[id]]
    if (!is.atomic(value) || length(value) != 1 || is.na(value)) next
    ok <- switch(
      kinds[[id]],
      switch = is.logical(value),
      slider = is.numeric(value) && is.finite(value),
      is.character(value)
    )
    if (!ok) next
    if (!is.null(glm_settings_choices[[id]]) && !value %in% glm_settings_choices[[id]]) next
    values[[id]] <- value
  }
  if (length(values) == 0) NULL else values
}

# Database driver packages used by the GLM fitting tool. They are in Suggests,
# so a user who only imports CSV files does not need to install them.
glm_tool_db_packages <- c(
  "MySQL" = "RMySQL",
  "SQLite" = "RSQLite",
  "SQL Server" = "RODBC",
  "PostgreSQL" = "RPostgreSQL"
)

# Checks that the packages needed for the selected database type are installed.
# Returns TRUE when they are; otherwise shows a modal explaining what to install
# and returns FALSE.
glm_tool_db_package_available <- function(db_type) {
  pkg <- glm_tool_db_packages[db_type]
  if (is.na(pkg)) {
    showModal(modalDialog(
      title = "Unknown database type",
      paste0("The database type '", db_type, "' is not supported."),
      easyClose = TRUE
    ))
    return(FALSE)
  }
  # the DBI drivers also need DBI itself; RODBC is self-contained
  needed <- if (pkg == "RODBC") pkg else c("DBI", pkg)
  missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    showModal(modalDialog(
      title = "Package required",
      paste0(
        "Install the ", paste(missing_pkgs, collapse = " and "), " package",
        if (length(missing_pkgs) > 1) "s" else "",
        " to connect to ", db_type, " databases, for example with install.packages(",
        paste0("\"", missing_pkgs, "\"", collapse = ", "), ")."
      ),
      easyClose = TRUE
    ))
    return(FALSE)
  }
  TRUE
}

# Runs a query through a DBI driver and always closes the connection.
glm_tool_query_dbi <- function(driver, sql, ...) {
  con <- DBI::dbConnect(driver, ...)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql)
}

# A value for an ODBC connection string, in braces (with any closing brace
# doubled), so that a ; in a host, user or password cannot add attributes.
glm_tool_odbc_value <- function(x) {
  paste0("{", gsub("}", "}}", if (is.null(x)) "" else x, fixed = TRUE), "}")
}

# Runs a query through an ODBC connection string and always closes the connection.
glm_tool_query_odbc <- function(connection_string, sql) {
  con <- RODBC::odbcDriverConnect(connection = connection_string)
  # odbcDriverConnect() returns -1 with a warning instead of an error when it cannot connect
  if (!inherits(con, "RODBC")) stop("could not connect to the database")
  on.exit(RODBC::odbcClose(con), add = TRUE)
  result <- RODBC::sqlQuery(con, sql)
  # RODBC returns a character vector with the error text instead of signalling an error
  if (!is.data.frame(result)) stop(paste(result, collapse = "\n"))
  result
}
