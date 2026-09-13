#' Server function for the GLM Fitting tool application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @import shiny
#' @importFrom plotly plot_ly add_lines layout add_bars renderPlotly
#' @importFrom plotly plotlyOutput
GLMFittingToolServer <- function(input, output, session) {

  ######################
  #help functions
  ######################

  or_default <- function(x, default) if (is.null(x) || length(x) == 0) default else x

  # the chart is drawn in the colours of the theme the page reports
  dark <- reactive(identical(input$app_theme, "dark"))

  # a column name as it must be written in a formula
  term <- function(column) if (identical(make.names(column), column)) column else paste0("`", gsub("`", "\\\\`", column), "`")

  ######################
  #import data
  ######################

  import_error <- function(message) {
    showModal(modalDialog(
      title = "The data could not be imported",
      message,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    NULL
  }

  db_port <- function(default) {
    port <- trimws(or_default(input$db_port, ""))
    if (port == "") return(default)
    value <- suppressWarnings(as.numeric(port))
    if (is.na(value) || value != round(value) || value <= 0) stop("the port must be a whole number, such as ", default)
    value
  }

  selected_data <- eventReactive(input$submit, {
    tryCatch({
      if (identical(input$data_source, "Database")) {
        # The database driver packages are optional (Suggests): check the one
        # needed for the selected database type before trying to connect
        if (!glm_tool_db_package_available(input$db_type)) {
          return(NULL)
        }
        df_input <- switch(
          input$db_type,
          "MySQL" = glm_tool_query_dbi(
            RMySQL::MySQL(), input$sql_query,
            host = input$db_host, user = input$db_user, password = input$db_password,
            port = db_port(3306), dbname = input$db_name
          ),
          "SQLite" = {
            # connecting to a file that does not exist would create an empty database
            if (!file.exists(input$db_name)) stop("the SQLite file '", input$db_name, "' does not exist")
            glm_tool_query_dbi(RSQLite::SQLite(), input$sql_query, dbname = input$db_name)
          },
          "SQL Server" = glm_tool_query_odbc(
            # values are braced, so that a ; in them cannot add attributes to the connection string
            if (isTRUE(input$windows_auth)) {
              # Use Windows authentication
              paste0("Driver={SQL Server};Server=", glm_tool_odbc_value(input$db_host),
                     ";Database=", glm_tool_odbc_value(input$db_name), ";Trusted_Connection=yes")
            } else {
              # Use username and password
              paste0("Driver={SQL Server};Server=", glm_tool_odbc_value(input$db_host),
                     ";Database=", glm_tool_odbc_value(input$db_name),
                     ";Uid=", glm_tool_odbc_value(input$db_user), ";Pwd=", glm_tool_odbc_value(input$db_password))
            },
            input$sql_query
          ),
          "PostgreSQL" = glm_tool_query_dbi(
            RPostgreSQL::PostgreSQL(), input$sql_query,
            host = input$db_host, port = db_port(5432), dbname = input$db_name,
            user = input$db_user, password = input$db_password
          )
        )
      } else {
        file <- input$csv_file
        if (is.null(file)) {
          return(import_error("Choose a CSV file first."))
        }
        df_input <- dft_read_data(
          file$datapath,
          header = !isFALSE(input$csv_header),
          sep = or_default(input$csv_sep, ","),
          quote = or_default(input$csv_quote, "\""),
          dec = or_default(input$csv_dec, ".")
        )
      }
      if (!is.data.frame(df_input) || ncol(df_input) == 0 || nrow(df_input) == 0) {
        return(import_error("The file or query returned no rows."))
      }
      df_input
    }, error = function(e) {
      import_error(paste0(
        "Check the settings and try again. The error was: ", conditionMessage(e)
      ))
    })
  })

  data_columns <- reactive(names(selected_data()))
  numeric_columns <- reactive(names(selected_data())[vapply(selected_data(), is.numeric, logical(1))])

  preview_rows <- 10000

  output$data_overview <- renderUI({
    df <- req(selected_data())
    source <- if (identical(isolate(input$data_source), "Database")) isolate(input$db_type) else isolate(input$csv_file$name)
    div(
      class = "dft-stats",
      dft_stat_tile("Source", div(class = "dft-file-name", or_default(source, "")), icon_name = "database"),
      dft_stat_tile("Rows", format(nrow(df), big.mark = ","), icon_name = "bars",
                    note = if (nrow(df) > preview_rows) paste("The preview shows the first", format(preview_rows, big.mark = ","))),
      dft_stat_tile("Columns", format(ncol(df), big.mark = ","), icon_name = "table-columns",
                    note = if (ncol(df) == 1) "Only one column: check the separator" else paste(length(numeric_columns()), "numeric"))
    )
  })

  # Display the query result or uploaded data in a data table
  output$selected_input_data_table <- reactable::renderReactable({
    validate(need(input$submit, "Import data to see a preview here."))
    df <- selected_data()
    validate(need(!is.null(df), "No data has been imported."))
    if (nrow(df) > preview_rows) df <- df[seq_len(preview_rows), , drop = FALSE]
    reactable::reactable(
      df,
      searchable = TRUE, highlight = TRUE, compact = TRUE, resizable = TRUE,
      defaultPageSize = 15, showPageSizeOptions = TRUE, pageSizeOptions = c(15, 50, 100),
      defaultColDef = reactable::colDef(minWidth = 100),
      theme = dft_reactable_theme()
    )
  })

  ######################
  #column choices
  ######################

  # column choices loaded from a settings file before the data they refer to was imported
  pending_columns <- reactiveVal(list())

  observeEvent(selected_data(), {
    columns <- data_columns()
    numbers <- numeric_columns()
    pending <- pending_columns()
    choose <- function(id, options, default) {
      wanted <- or_default(pending[[id]], input[[id]])
      if (!is.null(wanted) && wanted %in% options) wanted else default
    }
    response <- choose("response_variable", columns, if (length(numbers) > 0) numbers[1] else columns[1])
    updateSelectInput(session, "response_variable", choices = columns, selected = response)
    updateSelectInput(session, "offset", choices = c("None", columns), selected = choose("offset", columns, "None"))
    updateSelectInput(session, "weights", choices = c("None", columns), selected = choose("weights", columns, "None"))
    others <- setdiff(columns, response)
    updateSelectInput(session, "visualize_variable", choices = c("None", columns),
                      selected = choose("visualize_variable", columns, if (length(others) > 0) others[1] else "None"))
    pending_columns(list())
  })

  # a link to select once the family it belongs to is selected (a stored model or a settings file)
  pending_link <- reactiveVal(NULL)

  # only the links that make sense for the family are offered, with the family's default selected
  observeEvent(input$glm_distribution, {
    links <- glm_family_links[[input$glm_distribution]]
    req(links)
    wanted <- pending_link()
    pending_link(NULL)
    updateSelectInput(session, "link_function", choices = links,
                      selected = if (!is.null(wanted) && wanted %in% links) wanted else links[1])
  }, ignoreInit = TRUE)

  # the columns, as buttons that add them to the formula
  output$formula_columns <- renderUI({
    columns <- setdiff(req(data_columns()), c(input$response_variable, input$offset, input$weights))
    lapply(columns, function(column) {
      tags$button(type = "button", class = "glm-chip", `data-term` = term(column), column)
    })
  })

  # the left-hand side of the formula that the tool adds to the terms
  offset_term <- function(offset, offset_log) {
    if (is.null(offset) || identical(offset, "None")) return(NULL)
    if (isTRUE(offset_log)) paste0("offset(log(", term(offset), "))") else paste0("offset(", term(offset), ")")
  }

  output$formula_lhs <- renderUI({
    req(input$response_variable, input$response_variable != "")
    div(class = "glm-formula-lhs",
        paste(c(paste(term(input$response_variable), "~"), offset_term(input$offset, input$offset_log)), collapse = " "),
        if (!is.null(offset_term(input$offset, input$offset_log))) " +", " ...")
  })

  ######################
  #modelling
  ######################

  current_spec <- function() {
    list(
      response = input$response_variable,
      family = or_default(input$glm_distribution, "gaussian"),
      link = input$link_function,
      offset = or_default(input$offset, "None"),
      offset_log = isTRUE(input$offset_log),
      weights = or_default(input$weights, "None"),
      formula = trimws(or_default(input$formula, ""))
    )
  }

  fit_glm <- function(spec, model_data) {
    if (is.null(model_data)) stop("import data on the Data tab first")
    columns <- names(model_data)
    if (is.null(spec$response) || !spec$response %in% columns) stop("choose the response column")
    for (column in c(spec$offset, spec$weights)) {
      if (column != "None" && !column %in% columns) stop("the column '", column, "' is not in the data")
    }
    links <- glm_family_links[[spec$family]]
    if (is.null(links)) stop("unknown family '", spec$family, "'")
    link <- if (!is.null(spec$link) && spec$link %in% links) spec$link else links[1]
    if (spec$offset != "None") {
      offset_values <- model_data[[spec$offset]]
      if (!is.numeric(offset_values)) stop("the offset column must be numeric")
      if (spec$offset_log && any(offset_values <= 0, na.rm = TRUE)) {
        stop("the log of the offset needs positive values; '", spec$offset, "' has zero or negative values")
      }
    }
    if (grepl("~", spec$formula, fixed = TRUE)) {
      stop("enter only the terms after ~; the response is chosen above")
    }
    terms_text <- if (spec$formula == "") {
      "1"
    } else if (spec$formula == ".") {
      # every other column; the offset and weights columns are not predictors
      predictors <- setdiff(columns, c(spec$response, spec$offset, spec$weights))
      if (length(predictors) == 0) "1" else paste(vapply(predictors, term, character(1)), collapse = " + ")
    } else {
      spec$formula
    }
    rhs <- paste(c(offset_term(spec$offset, spec$offset_log), terms_text), collapse = " + ")
    model_formula <- stats::as.formula(paste(term(spec$response), "~", rhs), env = globalenv())
    warnings <- character(0)
    # a text response of a binomial model is a factor: glm() takes its first level as failure and
    # every other level as success; a logical response is left as it is, TRUE being success
    if (spec$family == "binomial" && is.character(model_data[[spec$response]])) {
      model_data[[spec$response]] <- factor(model_data[[spec$response]])
      outcomes <- levels(model_data[[spec$response]])
      if (length(outcomes) > 2) {
        warnings <- paste0("the response has ", length(outcomes), " values: '", outcomes[1],
                           "' is failure and every other value is success")
      }
    }
    # the call is written out, so that the summary shows the real formula, family and weights; it
    # refers to the data as model_data, so update() on a downloaded model needs the data assigned to
    # model_data or passed as data =. The weights are a column of the data, so the rows left out
    # for missing values leave out their weight too
    family_call <- call(spec$family, link = link)
    weights_arg <- if (spec$weights != "None") as.name(spec$weights) else NULL
    fit_call <- if (is.null(weights_arg)) {
      bquote(stats::glm(.(model_formula), family = .(family_call), data = model_data, na.action = stats::na.exclude))
    } else {
      bquote(stats::glm(.(model_formula), family = .(family_call), data = model_data, weights = .(weights_arg),
                        na.action = stats::na.exclude))
    }
    if (spec$offset != "None" && spec$offset_log && link != "log") {
      warnings <- c(warnings, "an exposure offset only multiplies the mean with the log link")
    }
    model <- withCallingHandlers(
      eval(fit_call),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    spec$link <- link
    list(model = model, spec = spec, data = model_data, warnings = unique(warnings), error = NULL)
  }

  fit_result <- eventReactive(input$fit_model, {
    spec <- current_spec()
    result <- tryCatch(
      fit_glm(spec, selected_data()),
      error = function(e) {
        showNotification(paste("Model fitting failed:", conditionMessage(e)), type = "error", duration = 10)
        list(model = NULL, spec = spec, error = conditionMessage(e))
      }
    )
    if (length(result$warnings) > 0) {
      showNotification(paste("The model was fitted with warnings:", paste(result$warnings, collapse = "; ")),
                       type = "warning", duration = 12)
    }
    result
  })

  # predictions for every row of the data the model was fitted to (NA where a variable is missing)
  model_predictions <- function(result) {
    unname(stats::predict(result$model, type = "response"))
  }

  # the response as glm() codes it (a text binomial response is 0 for its first level and 1 for
  # every other), for every row of the data (NA where the fit left the row out)
  model_actuals <- function(result) {
    unname(stats::naresid(result$model$na.action, result$model$y))
  }

  fitted_model <- reactive({
    if (!isTruthy(input$fit_model)) return(NULL)
    fit_result()$model
  })

  #print the model summary
  output$model_summary <- renderPrint({
    validate(need(input$fit_model, "Choose the model and click Fit model."))
    result <- fit_result()
    if (is.null(result$model)) {
      cat("Model fitting failed:", result$error, "\nPlease check your formula and dataset.\n")
    } else {
      summary(result$model)
    }
  })

  # figures of a fitted model shared by the tiles and the stored models
  model_facts <- function(model) {
    list(
      aic = AIC(model),
      deviance_explained = if (model$null.deviance > 0) 1 - model$deviance / model$null.deviance else NA_real_,
      n = stats::nobs(model),
      dropped = length(model$na.action),
      parameters = sum(!is.na(stats::coef(model)))
    )
  }

  output$model_stats <- renderUI({
    model <- req(fitted_model())
    facts <- model_facts(model)
    dispersion <- summary(model)$dispersion
    # the dispersion is fixed at 1 for these families; the Pearson statistic shows whether the data agrees
    fixed <- model$family$family %in% c("poisson", "binomial")
    pearson <- if (fixed && model$df.residual > 0) sum(stats::residuals(model, type = "pearson")^2, na.rm = TRUE) / model$df.residual else NA_real_
    div(
      class = "dft-stats",
      dft_stat_tile("Observations", format(facts$n, big.mark = ","), icon_name = "hashtag",
                    note = if (facts$dropped > 0) paste(format(facts$dropped, big.mark = ","), "rows with missing values left out")),
      dft_stat_tile("Parameters", facts$parameters, icon_name = "list-ol"),
      dft_stat_tile("AIC", dft_fmt(facts$aic, 7), icon_name = "scale-balanced", accent = TRUE),
      dft_stat_tile("Deviance explained", if (is.finite(facts$deviance_explained)) paste0(dft_fmt(100 * facts$deviance_explained, 3), "%") else dft_dash,
                    icon_name = "chart-pie"),
      dft_stat_tile(if (fixed) "Pearson dispersion" else "Dispersion", dft_fmt(if (fixed) pearson else dispersion, 4),
                    icon_name = "wave-square",
                    note = if (fixed) {
                      if (is.finite(pearson) && pearson > 1.5) "Well above 1: the data is overdispersed" else "Pearson chi-squared / residual df"
                    })
    )
  })

  output$coefficients_table <- renderUI({
    validate(need(input$fit_model, "Choose the model and click Fit model."))
    model <- fitted_model()
    validate(need(!is.null(model), paste("The model could not be fitted:", fit_result()$error)))
    estimates <- stats::coef(model)
    table <- summary(model)$coefficients
    link <- model$family$link
    relativity_label <- switch(link, log = "Relativity", logit = "Odds ratio", NULL)
    stars <- function(p) {
      if (!is.finite(p)) return(NULL)
      mark <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""
      if (mark != "") tags$span(class = "glm-signif", mark)
    }
    rows <- lapply(names(estimates), function(name) {
      present <- name %in% rownames(table)
      value <- function(column) if (present) table[name, column] else NA_real_
      cells <- list(
        name,
        dft_fmt(estimates[[name]], 5),
        dft_fmt(value(2), 4),
        dft_fmt(value(3), 4),
        tagList(if (present) format.pval(value(4), digits = 3, eps = 1e-4) else dft_dash, stars(value(4)))
      )
      if (!is.null(relativity_label)) cells <- c(cells, list(if (name == "(Intercept)") dft_dash else dft_fmt(exp(estimates[[name]]), 4)))
      cells
    })
    statistic <- colnames(table)[3]
    dft_html_table(
      c("Term", "Estimate", "Std. error", statistic, "p-value", relativity_label), rows,
      note = paste0(
        "Significance: *** 0.001, ** 0.01, * 0.05, . 0.1.",
        if (!is.null(relativity_label)) paste0(" ", relativity_label, " = exp(estimate)."),
        if (any(is.na(estimates))) " A dash marks a term that is aliased with others and was not estimated." else ""
      )
    )
  })

  ######################
  #stored models
  ######################

  stored_models <- list(reactiveVal(NULL), reactiveVal(NULL))

  store_model <- function(i) {
    result <- if (isTruthy(input$fit_model)) fit_result() else NULL
    if (is.null(result) || is.null(result$model)) {
      showNotification("Fit a model first, then store it.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    stored_models[[i]](c(list(spec = result$spec), model_facts(result$model)))
  }

  load_model <- function(i) {
    stored <- stored_models[[i]]()
    if (is.null(stored)) {
      showNotification(paste("Model", i, "is empty."), type = "warning", duration = 6)
      return(invisible(NULL))
    }
    apply_model_spec(stored$spec)
    showNotification(paste0("Model ", i, " loaded. Click Fit model to fit it again."), type = "message", duration = 5)
  }

  apply_model_spec <- function(spec) {
    if (!identical(spec$family, input$glm_distribution)) pending_link(spec$link)
    updateSelectInput(session, "glm_distribution", selected = spec$family)
    updateSelectInput(session, "link_function", choices = glm_family_links[[spec$family]], selected = spec$link)
    for (id in c("response_variable", "offset", "weights")) {
      value <- spec[[switch(id, response_variable = "response", id)]]
      if (value %in% c("None", data_columns())) updateSelectInput(session, id, selected = value)
    }
    bslib::update_switch("offset_log", value = isTRUE(spec$offset_log), session = session)
    updateTextAreaInput(session, "formula", value = spec$formula)
  }

  observeEvent(input$save_formula_1, store_model(1))
  observeEvent(input$save_formula_2, store_model(2))
  observeEvent(input$load_formula_1, load_model(1))
  observeEvent(input$load_formula_2, load_model(2))

  describe_stored <- function(i) {
    stored <- stored_models[[i]]()
    if (is.null(stored)) return(div(class = "glm-slot-empty", "Empty. Fit a model and store it here."))
    other <- stored_models[[3 - i]]()
    spec <- stored$spec
    # AIC only compares models fitted to the same observations of the same response
    comparable <- !is.null(other) && identical(other$spec$response, spec$response) && other$n == stored$n
    lhs <- paste(c(paste(term(spec$response), "~"), offset_term(spec$offset, spec$offset_log)), collapse = " ")
    tagList(
      div(class = "glm-slot-formula", paste(lhs, if (!is.null(offset_term(spec$offset, spec$offset_log))) "+", if (spec$formula == "") "1" else spec$formula)),
      div(class = "sim-muted", paste0(spec$family, " (", spec$link, " link)",
                                      if (spec$weights != "None") paste0(", weights ", spec$weights), ", ",
                                      format(stored$n, big.mark = ","), " observations")),
      div(class = "glm-slot-aic", paste("AIC", dft_fmt(stored$aic, 7)),
          if (comparable && stored$aic < other$aic) tags$span(class = "glm-better", " lower")),
      if (!is.null(other) && !comparable) div(class = "sim-muted", "Not comparable with the other model: different response or observations.")
    )
  }

  output$aic_output_1 <- renderUI(describe_stored(1))
  output$aic_output_2 <- renderUI(describe_stored(2))

  ######################
  #downloads
  ######################

  output$model_downloads <- renderUI({
    if (is.null(fitted_model())) {
      return(div(class = "sim-downloads-hint", icon("circle-info"), "Fit a model to enable the downloads."))
    }
    div(
      class = "glm-downloads",
      downloadButton("download_model", "Model (RDS)", class = "btn-outline-glm"),
      downloadButton("download_summary", "Model summary (text)", class = "btn-outline-glm"),
      downloadButton("download_data_with_predictions", "Data with predictions (CSV)", class = "btn-outline-glm"),
      dft_help("The model refers to its data as model_data: to update() it in R, assign the data to ",
               "model_data first, or pass it to update() as data =.")
    )
  })

  #download the model summary
  output$download_summary <- downloadHandler(
    filename = function() "glm_summary.txt",
    content = function(file) {
      writeLines(utils::capture.output(print(summary(req(fitted_model())))), file)
    }
  )

  # Download the GLM model object as an RDS file
  output$download_model <- downloadHandler(
    filename = function() "glm_model.rds",
    content = function(file) {
      saveRDS(req(fitted_model()), file)
    }
  )

  # the data with the model's prediction for every row (NA where a variable is missing)
  output$download_data_with_predictions <- downloadHandler(
    filename = function() "predicted_data.csv",
    content = function(file) {
      req(fitted_model())
      result <- fit_result()
      utils::write.csv(cbind(result$data, prediction = model_predictions(result)), file, row.names = FALSE)
    }
  )

  ######################
  #visualisation
  ######################

  # actual and predicted per band of the chosen variable; with an offset the chart compares
  # rates per unit of exposure, and weights (e.g. binomial trials) weigh each row
  # redrawn when the chart is asked for and when the model is refitted
  fitness_data <- eventReactive(list(input$execute_visualization, input$fit_model), {
    model <- fitted_model()
    validate(need(!is.null(model), "Fit a model on the Model tab first."))
    result <- fit_result()
    spec <- result$spec
    # the data and columns the model was fitted with, not the current choices
    df <- result$data
    variable <- input$visualize_variable
    validate(need(!is.null(variable) && variable %in% names(df), "Choose the explanatory variable."))
    # the response as the model codes it, on the same scale as the prediction
    actual <- model_actuals(result)
    predicted <- model_predictions(result)
    # the exposure: the offset column when the offset is its log, exp(offset) for a raw offset with the log link
    exposure <- if (spec$offset == "None") {
      rep(1, nrow(df))
    } else if (spec$offset_log) {
      df[[spec$offset]]
    } else if (spec$link == "log") {
      exp(df[[spec$offset]])
    } else {
      rep(1, nrow(df))
    }
    weight <- if (spec$weights == "None") rep(1, nrow(df)) else df[[spec$weights]]
    grouping <- df[[variable]]
    bands <- or_default(input$number_of_bands_input, 10)
    if (is.numeric(grouping) && length(unique(grouping[!is.na(grouping)])) > bands) {
      breaks <- if (identical(input$band_method, "width")) {
        bands
      } else {
        # bands with about the same number of rows each
        unique(stats::quantile(grouping, seq(0, 1, length.out = bands + 1), na.rm = TRUE, names = FALSE))
      }
      grouping <- cut(grouping, breaks = breaks, include.lowest = TRUE, dig.lab = 6)
    } else {
      grouping <- factor(grouping, levels = sort(unique(grouping[!is.na(grouping)])))
    }
    keep <- !is.na(actual) & !is.na(predicted) & !is.na(grouping) & is.finite(exposure) & is.finite(weight)
    validate(need(any(keep), "No row has the response, the prediction and the variable all present."))
    band <- droplevels(grouping[keep])
    w <- weight[keep]
    e <- exposure[keep]
    total <- function(x) as.numeric(tapply(x, band, sum))
    denominator <- total(w * e)
    plot_data <- data.frame(
      band = levels(band),
      actual = total(w * actual[keep]) / denominator,
      predicted = total(w * predicted[keep]) / denominator,
      exposure = denominator,
      stringsAsFactors = FALSE
    )
    attr(plot_data, "dropped") <- sum(!keep)
    plot_data
  })

  output$fitness_note <- renderUI({
    # the chart shows the message when there is no data; the note stays empty
    plot_data <- tryCatch(fitness_data(), error = function(e) NULL)
    dropped <- attr(req(plot_data), "dropped")
    if (isTRUE(dropped > 0)) {
      dft_help(format(dropped, big.mark = ","), if (dropped == 1) " row is" else " rows are",
               " left out: the response, the prediction or the variable is missing.")
    }
  })

  output$fitness_plot <- renderPlotly({
    validate(need(input$execute_visualization, "Choose a variable and click Draw chart."))
    plot_data <- fitness_data()
    spec <- isolate(fit_result()$spec)
    colours <- dft_plot_colours(dark())
    exposure_label <- if (spec$offset != "None") paste0("Exposure (", spec$offset, ")") else if (spec$weights != "None") paste0("Weight (", spec$weights, ")") else "Rows"
    p <- plot_ly(plot_data, x = ~band)
    p <- add_bars(p, y = ~exposure, name = exposure_label, yaxis = "y2",
                  marker = list(color = if (dark()) "rgba(20, 184, 166, 0.22)" else "rgba(13, 148, 136, 0.18)"),
                  hovertemplate = "%{y:,.4~g}")
    p <- plotly::add_trace(p, y = ~actual, name = "Actual", type = "scatter", mode = "lines+markers",
                           line = list(color = colours$empirical, width = 2.5), marker = list(color = colours$empirical, size = 7))
    p <- plotly::add_trace(p, y = ~predicted, name = "Predicted", type = "scatter", mode = "lines+markers",
                           line = list(color = dft_model_palette[2], width = 2.5), marker = list(color = dft_model_palette[2], size = 7))
    y_title <- if (spec$offset != "None") paste(spec$response, "per unit of exposure") else paste("Average", spec$response)
    p <- dft_plot_layout(p, isolate(input$visualize_variable), y_title, dark())
    layout(
      p,
      xaxis = list(type = "category", categoryorder = "array", categoryarray = plot_data$band),
      yaxis = list(rangemode = "tozero"),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = exposure_label, font = list(color = colours$muted)),
                    tickfont = list(color = colours$muted)),
      margin = list(r = 70),
      bargap = 0.15
    )
  })

  ######################
  #save and load settings
  ######################

  #save configurations: the import options and model choices, never the password or files
  output$DownloadDataHandlerConf <- downloadHandler(
    filename = function() "glm_tool_settings.rds",
    content = function(file) {
      ids <- glm_settings_inputs$id
      values <- isolate(stats::setNames(lapply(ids, function(id) input[[id]]), ids))
      saveRDS(list(tool = glm_settings_tool, version = 1L, inputs = Filter(Negate(is.null), values)), file)
    }
  )

  #load configurations
  observeEvent(input$load_config, {
    file_info <- input$load_config
    req(file_info$datapath)
    settings <- tryCatch(readRDS(file_info$datapath), error = function(e) NULL, warning = function(w) NULL)
    values <- glm_settings_values(settings)
    if (is.null(values)) {
      showNotification(paste0("'", file_info$name, "' is not a settings file of the GLM fitting tool."), type = "error", duration = 8)
      return(invisible(NULL))
    }
    kinds <- stats::setNames(glm_settings_inputs$kind, glm_settings_inputs$id)
    # before the first import there are no columns yet (data_columns() stops silently)
    columns <- tryCatch(data_columns(), error = function(e) NULL)
    pending <- list()
    family <- values$glm_distribution
    if (!is.null(family) && !is.null(values$link_function) && !identical(family, input$glm_distribution)) {
      pending_link(values$link_function)
    }
    for (id in names(values)) {
      value <- values[[id]]
      switch(
        kinds[[id]],
        radio = updateRadioButtons(session, id, selected = value),
        switch = bslib::update_switch(id, value = isTRUE(value), session = session),
        text = updateTextInput(session, id, value = value),
        textarea = updateTextAreaInput(session, id, value = value),
        slider = updateSliderInput(session, id, value = value),
        select = if (id == "link_function" && !is.null(family) && !is.null(glm_family_links[[family]])) {
          updateSelectInput(session, id, choices = glm_family_links[[family]], selected = value)
        } else {
          updateSelectInput(session, id, selected = value)
        },
        column = if (!is.null(columns) && value %in% c("None", columns)) {
          updateSelectInput(session, id, selected = value)
        } else {
          pending[[id]] <- value
        }
      )
    }
    pending_columns(pending)
    showNotification(
      paste0("Settings loaded from '", file_info$name, "'.",
             if (length(pending) > 0) " The column choices are applied when the data is imported." else ""),
      type = "message", duration = 6
    )
  })

  # the tiles and the formula helpers are hidden while empty, and shiny does not render
  # hidden outputs, so they must render even when hidden or they would never appear
  for (id in c("data_overview", "model_stats", "formula_columns", "formula_lhs")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
