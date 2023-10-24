#' Server function for the GLM Fitting tool application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Returns server rendering for the shiny application.
#' @import shiny
#' @import DBI
#' @importFrom RMySQL MySQL
#' @importFrom RSQLite SQLite
#' @import RODBC
#' @import RPostgreSQL
#' @importFrom shinyjs useShinyjs
#' @importFrom plotly plot_ly add_lines layout add_bars renderPlotly
#' @import shinybusy

GLMFittingToolServer = function(input, output, session) {

  #import data
  selected_data <- eventReactive(input$submit, {
    show_spinner()
    result <- tryCatch({
      if (input$data_source == "Database") {
        # need to test all connectors work
        if (input$db_type == "MySQL") {
          port <- if (input$db_port == "") 3306 else as.numeric(input$db_port)
          con <- dbConnect(RMySQL::MySQL(),
                           host = input$db_host,
                           user = input$db_user,
                           password = input$db_password,
                           port = port,
                           dbname = input$db_name)
          # Execute the query
          df_input <- dbGetQuery(con, input$sql_query)
          # Close the connection
          dbDisconnect(con)

        } else if (input$db_type == "SQLite") {
          con <- dbConnect(RSQLite::SQLite(),
                           dbname = input$db_name)
          # Execute the query
          df_input <- dbGetQuery(con, input$sql_query)
          # Close the connection
          dbDisconnect(con)

        } else if (input$db_type == "SQL Server") {
          if (isTRUE(input$windows_auth)) {
            # Use Windows authentication
            con <- odbcDriverConnect(
              connection = paste0(
                "Driver={SQL Server};Server=", input$db_host,
                ";Database=", input$db_name,
                ";Trusted_Connection=yes"
              )
            )
          } else {
            # Ask for username and password
            con <- odbcDriverConnect(
              connection = paste0(
                "Driver={SQL Server};Server=", input$db_host,
                ";Database=", input$db_name,
                ";Uid=", input$db_user,
                ";Pwd=", input$db_password
              )
            )
          }
          # Execute the query
          df_input <- sqlQuery(con, input$sql_query)
          # Close the connection
          odbcClose(con)

        } else if (input$db_type == "PostgreSQL") {
          port <- if (input$db_port == "") 5432 else as.numeric(input$db_port)
          con <- dbConnect(
            RPostgreSQL::PostgreSQL(),
            host = input$db_host,
            port = port,
            dbname = input$db_name,
            user = input$db_user,
            password = input$db_password
          )
          # Execute the query
          df_input <- dbGetQuery(con, input$sql_query)
          # Close the connection
          dbDisconnect(con)
        }

        #return data
        hide_spinner()
        return(df_input)

      } else if (input$data_source == "CSV File") {

        # Read the uploaded CSV file
        file <- input$csv_file
        if (is.null(file)) {
          hide_spinner()
          return(NULL)
        }
        df <- read.csv(file$datapath)
        hide_spinner()
        return(df)
      }
    }, error = function(e) {
      hide_spinner()
      showModal(modalDialog(
        title = "Error",
        "An error occurred while importing data. Please check your settings and try again.",
        footer = NULL
      ))
      return(NULL)
    })
    return(result)
  })

  # Display the query result or uploaded data in a data table
  output$selected_input_data_table <- renderDataTable({selected_data()})

  #save configurtations
  output$DownloadDataHandlerConf <- downloadHandler(
    filename = function() {paste0('app_config','.rds')}
    ,content = function(file) {saveRDS(
      reactiveValuesToList(input)
      ,file
    )}
  )
  output$downloadConfButton <- renderUI({
    downloadButton('DownloadDataHandlerConf', 'Save data')
  })

  #load configurations
  observeEvent(input$load_config, {
    input_config <- readRDS(input$load_config$datapath)
    lapply(
      names(input_config)
      ,function(x) session$sendInputMessage(x, list(value = input_config[[x]]))
    )
  })


  #modelling
  observe({
    data_columns <- names(selected_data())
    updateSelectInput(session, "response_variable", choices = data_columns)
    updateSelectInput(session, "offset", choices = c(c("None"), data_columns))
    updateSelectInput(session, "weights", choices = c(c("None"), data_columns))
    updateSelectInput(session, "visualize_variable", choices = c(c("None"), data_columns))
  })

  fitted_model <- eventReactive(input$fit_model, {
    show_spinner()
    tryCatch({
      offset_formula <- if (input$offset != "None") paste("offset(", input$offset, ") +", sep = "")
      formula_str <- paste(input$response_variable, "~", offset_formula, input$formula)
      glm_model <- glm(
        as.formula(formula_str)
        ,data = selected_data()
        #not working need to fix
        ,weights = if (input$weights != "None") eval(parse(text = input$weights)) else NULL
        ,family = eval(parse(text = paste0(input$glm_distribution, '(link = ', input$link_function,')')))
      )
      hide_spinner()
      return(glm_model)
    }, error = function(e) {
      message("Model fitting failed:", e)
      hide_spinner()
      return(NULL)
    })
  })

  # Store the formula text
  formula_text_1 <- reactiveVal()
  formula_text_2 <- reactiveVal()

  # Save the formula when the "Save Formula" button is clicked
  observeEvent(input$save_formula_1, {
    formula_text_1(input$formula)
  })
  observeEvent(input$save_formula_2, {
    formula_text_2(input$formula)
  })

  # Load the saved formula when the "Load Formula" button is clicked
  observeEvent(input$load_formula_1, {
    if (!is.null(formula_text_1())) {
      updateTextAreaInput(session, "formula", value = formula_text_1())
    }
  })
  observeEvent(input$load_formula_2, {
    if (!is.null(formula_text_2())) {
      updateTextAreaInput(session, "formula", value = formula_text_2())
    }
  })

  # Calculate AIC and display it
  aic_output_1 <- eventReactive(input$save_formula_1, {
    if (!is.null(fitted_model())) {
      aic <- AIC(fitted_model())
      return(paste("AIC formula 1:", round(aic, 2)))
    }
  })

  output$aic_output_1 <- renderText({
    aic_output_1()
  })

  aic_output_2 <- eventReactive(input$save_formula_2, {
    if (!is.null(fitted_model())) {
      aic <- AIC(fitted_model())
      return(paste("AIC formula 2:", round(aic, 2)))
    }
  })

  output$aic_output_2 <- renderText({
    aic_output_2()
  })


  #print the model summary
  output$model_summary <- renderPrint({
    if (is.null(fitted_model())) {
      "Model fitting failed. Please check your formula and dataset."
    } else {
      summary(fitted_model())
    }
  })

  #download the model summary
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("glm_summary.txt", sep = "")
    },
    content = function(file) {
      writeLines(capture.output(print(summary(fitted_model()))), file)
    }
  )

  # Download the GLM model object as an RDS file
  #consider adding option to remove data
  output$download_model <- downloadHandler(
    filename = function() {
      paste("glm_model.rds", sep = "")
    },
    content = function(file) {
      saveRDS(fitted_model(), file)
    }
  )

  # Download handler for the CSV file
  output$download_data_with_predictions <- downloadHandler(
    filename = function() {
      "predicted_data.csv"
    },
    content = function(file) {
      write.csv(
        cbind(
          selected_data()
          ,prediction=predict(fitted_model(), selected_data(), type="response")
        )
        , file, row.names = FALSE
      )
    }
  )

  #visualisation
  fitness_plot <- eventReactive(input$execute_visualization, {
    show_spinner()
    if (input$visualize_variable == "None") {
      hide_spinner()
      return(NULL)
    }
    #create temporary dataframe
    plot_data <- data.frame(
      Actual = selected_data()[,input$response_variable],
      Predicted = predict(object = fitted_model(), selected_data(), type="response"),
      Visualized = selected_data()[,input$visualize_variable],
      Exposure = 1
    )
    if (input$offset != "None") {
      plot_data$Exposure <- selected_data()[,input$offset]
    }
    #band the visualisation variable
    if (is.numeric(plot_data$Visualized)) {
      if (length(unique(plot_data$Visualized))>input$number_of_bands_input){
        plot_data$Visualized <- cut(plot_data$Visualized, breaks = input$number_of_bands_input)
      }
    }
    # aggregate the dataframe
    plot_data <- aggregate(plot_data[,c('Actual', 'Predicted', 'Exposure')], by=list(plot_data$Visualized), sum)
    plot_data$Actual <- plot_data$Actual/plot_data$Exposure
    plot_data$Predicted <- plot_data$Predicted/plot_data$Exposure

    p <- plot_ly(data = plot_data, x = ~Group.1)
    p <- add_lines(p, y = ~Actual, name = "Actual", type = "scatter", mode = "lines")
    p <- add_lines(p, y = ~Predicted, name = "Predicted", type = "scatter", mode = "lines")

    # Assuming you want counts on the second y-axis
    p <- layout(p, yaxis2 = list(overlaying = "y", side = "right"))
    p <- add_bars(p, y = ~Exposure, name = "Exposure", yaxis = "y2", opacity = 0.5)

    #set axis names
    p <- layout(p, xaxis = list(title = input$visualize_variable))
    p <- layout(p, yaxis = list(title = input$response_variable))
    p <- layout(p, yaxis2 = list(title = 'Expopsure'))
    p <- layout(p, title = "Comparison of Actual and Predicted Values")

    hide_spinner()
    return(p)
  })

  # Render the plot in the main panel
  output$fitness_plot <- renderPlotly({
    fitness_plot()
  })

}
