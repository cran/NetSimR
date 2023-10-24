#' Server function for the Distribution Fitting tool application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Returns server rendering for the shiny application.
#' @import shiny
#' @import MASS
#' @import shinyWidgets
#' @importFrom plotly plot_ly add_lines layout add_bars renderPlotly
#' @import fitdistrplus

distribution_fitting_tool_Server = function(input, output, session) {

  ######################
  #help functions
  ######################

  clean_and_convert_to_numeric <- function(input_vector, greater_than_zero = F){
    # Remove non-numeric elements
    numeric_data <- as.numeric(input_vector[!is.na(as.numeric(input_vector))])
    # Remove negative values
    if (greater_than_zero) {
      numeric_data <- numeric_data[numeric_data > 0]
    } else {
      numeric_data <- numeric_data[numeric_data >= 0]
    }
    return(numeric_data)
  }

  generate_xaxis_points <- function(claim_vector, num_of_points) {
    claim_vector_unique <- unique(claim_vector)
    if (max(claim_vector_unique) < num_of_points) {
      return(0:max(claim_vector_unique))
    } else if (length(claim_vector_unique) <= num_of_points) {
      return(sort(unique(claim_vector_unique)))
    } else {
      num_of_individual_tail_points <- num_of_points * 0.2
      tail_points <- head(claim_vector_unique[order(claim_vector_unique, decreasing = TRUE)], n = num_of_individual_tail_points)
      step_sliced_sev <- round(max(claim_vector_unique)/num_of_points,0)+1
      xaxis <- seq(0, max(claim_vector_unique), step_sliced_sev)
      xaxis <- xaxis[xaxis<min(tail_points)]
      xaxis <- c(xaxis, tail_points)
      return(sort(xaxis))
    }
  }

  kstest <- function(claims, predictions){round(max(abs(rank(claims)/(length(claims)+1)-predictions))*sqrt(length(claims)),2)}

  existence_check_shiny <- function(df){
    tryCatch({
      df
      return(T)
    },
    shiny.silent.error = function(e) {return(F)}
    )
  }


  fit_slice_pareto <- function (sev_data, slic_pont_lft, slic_pont_rght) {
    z <- subset(sev_data, sev_data > slic_pont_lft)
    w <- subset(z, z <= slic_pont_rght)
    dat <- data.frame(severity = w, empirical = rank(w) / (length(z) + 1))
    SumOfSquares <- function(data, par) {
      sum((1 - (slic_pont_lft / data$severity)^par[1] - data$empirical)^2)
    }
    x=1
    result <- optim(par = x, SumOfSquares, data = dat, lower = 0.0001, method = "L-BFGS-B")
    return(result$par)
  }

  ######################
  #Data input
  ######################

  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    header <- if (input$data_includes_header) TRUE else FALSE
    df <- read.csv(inFile$datapath, header = header, sep = input$sep, quote = input$quote)
    updateSelectInput(session, inputId = 'counts_var', label = 'Frequency Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'counts_weights_var', label = 'Weights Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'severity_var', label = 'Severity Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'sliced_sev_var', label = 'Sliced Severity Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'piecewise_pareto_var', label = 'Piecwise Pareto Severity Variable',
                      choices = names(df), selected = names(df))
    return(df)
  })

  output$data_table <- renderDataTable({data()})

  ######################
  #Frequency analysis
  ######################

  counts_data <- eventReactive(input$execute_freq_analysis, {
    clean_and_convert_to_numeric(data()[, input$counts_var])
  })
  weights_data <- eventReactive(input$execute_freq_analysis, {
    if (input$counts_weighted_var) {
      clean_and_convert_to_numeric(data()[, input$counts_weights_var])
    } else { NULL }
  })

  freq_nb_fit <- reactive({
    if (input$counts_weighted_var) {
      suppressWarnings(fitdist(counts_data(), "nbinom", weights = weights_data()))
    } else {
      suppressWarnings(fitdist(counts_data(), "nbinom"))
    }
  })
  freq_po_fit <- reactive({
    if (input$counts_weighted_var) {
      suppressWarnings(fitdist(counts_data(), "pois", weights = weights_data()))
    } else {
      suppressWarnings(fitdist(counts_data(), "pois"))
    }
  })
  mean_counts <- reactive({mean(counts_data())})
  median_counts <- reactive({median(counts_data())})
  var_counts <- reactive({var(counts_data())})

  output$selected_freq_params <- renderPrint({
    param <- NULL
    if (input$FreqDistri == "Poisson") {
      param <- data.frame(Mean = format(freq_po_fit()$estimate[1], scientific = TRUE, digits = 4))
    } else {
      param <- data.frame(
        Beta = format(freq_nb_fit()$estimate[2]/freq_nb_fit()$estimate[1], scientific = TRUE, digits = 4), # also mu/size
        r = format(freq_nb_fit()$estimate[1], scientific = TRUE, digits = 4) # r is also called size
      )
    }
    rownames(param) <- "Value"
    noquote(format(param, justify = "left"))
  })

  output$freq_summary <- renderPrint({summary(counts_data())})

  output$distribution_proposal <- renderPrint({
    summary <- matrix(c(
      format(round(mean_counts(),2), big.mark=",")
      ,format(round(var_counts(),2), big.mark=",")
      ,if (var_counts()>mean_counts()) {"Negative Binomial"} else {"Poisson"}
    ), nrow = 1, ncol = 3, byrow = TRUE)
    colnames(summary)<-c("Mean  ","Variance  ","Recomended Freq. Distr.")
    rownames(summary)<-c("Value  ")
    noquote(summary)
  })

  output$selected_distribution_summary <- renderPrint({
    if (input$FreqDistri == "Poisson") {summary(freq_po_fit())} else {summary(freq_nb_fit())}
  })

  output$count_hist <- renderPlotly({
    p <- plot_ly(
      x = counts_data()
      ,type = "histogram"
      ,nbinsx  = input$count_hist_bins
      ,marker = list(line = list(color = "black", width = 2))
      ,name = "Empirical pdf"
    )
    p <- layout(p, xaxis = list(title = "Number of claims"), yaxis = list(title = "Counts"))
    p <- layout(p, title = "Histogram of Claim Counts")
    return(p)
  })

  output$freq_fit_plot <- renderPlotly({
    x_axis <- generate_xaxis_points(counts_data(), 1000)
    empirical_cdf <- unlist(lapply(x_axis, function(x) sum(counts_data() <= x)))/length(counts_data())
    p <- plot_ly(x = x_axis, y = empirical_cdf, type = "scatter", mode = "lines", name = "Empirical cdf")
    p <- layout(p, xaxis = list(title = "Claim frequency"), yaxis = list(title = "CDF"))
    p <- layout(p, title =  "Claim Frequency CDF and model fit")
    poisson_cdf <- ppois(x_axis, lambda = freq_po_fit()$estimate[1])
    p <- add_lines(p, x = x_axis, y = poisson_cdf, type = 'scatter', mode = 'lines', name = 'Poisson cdf')
    nb_cdf <- pnbinom(x_axis, size = freq_nb_fit()$estimate[1], mu = freq_nb_fit()$estimate[2])
    p <- add_lines(p, x = x_axis, y = nb_cdf, type = 'scatter', mode = 'lines', name = 'Negative Binomial cdf')
    return(p)
  })

  ######################
  #Severity analysis
  ######################

  severity_data <- eventReactive(input$execute_sev_analysis, {
    clean_and_convert_to_numeric(data()[, input$severity_var], T)
  })

  sev_norm_fit <- reactive({fitdist(severity_data(), "norm")})
  sev_lnorm_fit <- reactive({fitdist(severity_data(), "lnorm")})
  sev_gamma_fit <- reactive({fitdistr(severity_data(), 'gamma', method = "L-BFGS-B", lower = c(0, 0), start = list(scale = 1, shape = 1))})

  sev_pareto_xm <- reactive({ min(severity_data()) })
  sev_pareto_alpha <- reactive({ length(severity_data())/(sum(log(severity_data()))-length(severity_data())*log(sev_pareto_xm())) })


  output$sev_param_summary <- renderPrint({
    KSNorm <- kstest(severity_data(), pnorm(severity_data(),sev_norm_fit()$estimate[1],sev_norm_fit()$estimate[2]))
    KSlNorm <- kstest(severity_data(), plnorm(severity_data(), meanlog = sev_lnorm_fit()$estimate[1], sdlog = sev_lnorm_fit()$estimate[2]))
    KSPareto <- kstest(severity_data(), 1-(sev_pareto_xm()/severity_data())^sev_pareto_alpha())
    KSExpo <- kstest(severity_data(), pexp(severity_data(),1/sev_norm_fit()$estimate[1]))
    KSGamma <- kstest(severity_data(), pgamma(severity_data(),scale = sev_gamma_fit()$estimate[1],shape = sev_gamma_fit()$estimate[2]))

    param <- matrix(c(round(sev_norm_fit()$estimate[1],0),round(sev_norm_fit()$estimate[2],2),KSNorm
                      ,"","","","Mu","Sigma", "K-S Dist",round(sev_lnorm_fit()$estimate[1],4),round(sev_lnorm_fit()$estimate[2],4),KSlNorm
                      ,"","","","xm","alpha", "K-S Dist",round(sev_pareto_xm(),0),round(sev_pareto_alpha(),4),KSPareto
                      ,"","","","Mean","", "K-S Dist",round(sev_norm_fit()$estimate[1],0),"",KSExpo
                      ,"","","","Scale","Shape", "K-S Dist",round(sev_gamma_fit()$estimate[1],4),round(sev_gamma_fit()$estimate[2],4),KSGamma
    ),
    nrow = 13, ncol = 3, byrow = TRUE)
    colnames(param)<-c("Mean", "Std. Dev.", "K-S Dist")
    rownames(param)<-c("Normal","","","LogNormal","","","Pareto","","","Exponential","","","Gamma")
    noquote(param)
  })


  output$sev_hist <- renderPlotly({
    p <- plot_ly(
      x = severity_data()
      ,type = "histogram"
      ,nbinsx  = input$severity_hist_bins
      ,marker = list(line = list(color = "black", width = 2))
      ,name = "Empirical pdf"
    )
    p <- layout(p, xaxis = list(title = "Claim Severity"), yaxis = list(title = "Severity"))
    p <- layout(p, title = "Histogram of Claim Severity")
    return(p)
  })

  output$sev_summary <- renderPrint({summary(severity_data())})

  output$sev_fit_plot <- renderPlotly({
    x_axis <- generate_xaxis_points(severity_data(), 1000)
    x_axis_after_log_option <- if (input$sev_fit_log_scale) {log(x_axis)} else {x_axis}
    empirical_cdf <- unlist(lapply(x_axis, function(x) sum(severity_data() <= x)))/length(severity_data())
    p <- plot_ly(x = x_axis_after_log_option, y = empirical_cdf, type = "scatter", mode = "lines", name = "Empirical cdf")
    p <- layout(p, xaxis = list(title = paste("Claim Severity", ifelse(T,"Log Scale",""))), yaxis = list(title = "Severity log scale"))
    p <- layout(p, title =  paste("Claim Severity and severity model fit", ifelse(T,"Log Scale","")))
    normal_cdf <- pnorm(x_axis, mean=sev_norm_fit()$estimate[1], sd=sev_norm_fit()$estimate[2])
    p <- add_lines(p, x = x_axis_after_log_option, y = normal_cdf, type = 'scatter', mode = 'lines', name = 'Normal cdf')
    lnormal_cdf <- plnorm(x_axis, meanlog=sev_lnorm_fit()$estimate[1], sdlog=sev_lnorm_fit()$estimate[2])
    p <- add_lines(p, x = x_axis_after_log_option, y = lnormal_cdf, type = 'scatter', mode = 'lines', name = 'LogNormal cdf')
    exponential_cdf <- pexp(x_axis, 1/sev_norm_fit()$estimate[1])
    p <- add_lines(p, x = x_axis_after_log_option, y = exponential_cdf, type = 'scatter', mode = 'lines', name = 'Exponential cdf')
    gamma_cdf <- pgamma(x_axis, scale = sev_gamma_fit()$estimate[1], shape = sev_gamma_fit()$estimate[2])
    p <- add_lines(p, x = x_axis_after_log_option, y = gamma_cdf, type = 'scatter', mode = 'lines', name = 'Gamma cdf')
    pareto_cdf <- 1-(sev_pareto_xm()/x_axis)^sev_pareto_alpha()
    p <- add_lines(p, x = x_axis_after_log_option, y = pareto_cdf, type = 'scatter', mode = 'lines', name = 'Pareto cdf')
    return(p)
  })


  ######################
  #Sliced Sev analysis
  ######################

  sliced_sev_data <- eventReactive(input$execute_sliced_sev_analysis, {
    clean_and_convert_to_numeric(data()[, input$sliced_sev_var], T)
  })

  x_axis_sliced_sev <- eventReactive(input$execute_sliced_sev_analysis, {
    generate_xaxis_points(sliced_sev_data(), 1000)
  })

  output$mean_excess_func_plot <- renderPlotly({
    p <- plot_ly(
      y = unlist(lapply(x_axis_sliced_sev(), function(x) mean(x_axis_sliced_sev()[x_axis_sliced_sev() >= x])))
      ,x = x_axis_sliced_sev(), type = "scatter", mode = "lines"
    )
    p <- layout(
      p,
      xaxis = list(title = "Claim Severity"),
      yaxis = list(title = "Mean Excess Function"),
      title = "Mean Excess Function of Claim Severity"
    )
  })

  observeEvent(input$execute_sliced_sev_analysis,{
    updateSliderInput(
      session
      ,"slicing_point_left"
      ,min = round(min(sliced_sev_data()),0)
      ,max = round(max(sliced_sev_data()),0)
      ,value = (round(min(sliced_sev_data()),0)+round(max(sliced_sev_data()),0))/2
    )
  })

  observeEvent(input$execute_sliced_sev_analysis,{
    updateSliderInput(
      session
      ,"slicing_point_right"
      ,min = (round(min(sliced_sev_data()),0)+round(max(sliced_sev_data()),0))/2
      ,max = round(max(sliced_sev_data()),0)
      ,value = 3*(round(min(sliced_sev_data()),0)+round(max(sliced_sev_data()),0))/4
    )
  })

  observeEvent(input$slicing_point_left,{
    updateSliderInput(
      session
      ,"slicing_point_right"
      ,min = input$slicing_point_left
      ,value = (input$slicing_point_left+round(max(sliced_sev_data()),0))/2
    )
    update
  })

  slc_sev_lnorm_fit <- reactive({fitdist(sliced_sev_data(), "lnorm")})

  slc_sev_censored_lnorm_fit <- reactive({
    tryCatch({
      if(!existence_check_shiny(sliced_sev_data())){return(data.frame(par=c(1,2)))}
      w <- subset(sliced_sev_data(), sliced_sev_data() <= input$slicing_point_left)
      dat <- data.frame(severity = w, empirical = rank(w) / (length(sliced_sev_data()) + 1))
      SumOfSquares <- function(data, par) {
        sum((plnorm(data$severity, meanlog = par[1], sdlog = par[2]) - data$empirical)^2)
      }
      result <- optim(par = c(mean(log(w)), sd(log(w))), SumOfSquares, data = dat, lower=0.0001, method = "L-BFGS-B")
      if(result$convergence != 0) {return(NULL)}
      return(result)
    }, error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
      return(e)
    })
  })

  zGreaterThanXm1 <- reactive({ sliced_sev_data()[sliced_sev_data()>input$slicing_point_left] })
  zGreaterThanXm2 <- reactive({ sliced_sev_data()[sliced_sev_data()>input$slicing_point_right] })
  paretoX1Alpha <- reactive({ length(zGreaterThanXm1())/(sum(log(zGreaterThanXm1()))-length(zGreaterThanXm1())*log(input$slicing_point_left)) })
  paretoX2AlphaMod <- reactive({
    tryCatch({
      if(!existence_check_shiny(sliced_sev_data())){return(1)}
      return(fit_slice_pareto(sliced_sev_data(), input$slicing_point_left, input$slicing_point_right))
    }, error = function(e) {
      cat("An error occurred: ", conditionMessage(e), "\n")
      return(e)
    })
  })
  paretoX2Alpha <- reactive({ length(zGreaterThanXm2())/(sum(log(zGreaterThanXm2()))-length(zGreaterThanXm2())*log(input$slicing_point_right)) })
  PrXLessThanParetoX1 <- reactive({ plnorm(input$slicing_point_left, meanlog = slc_sev_censored_lnorm_fit()$par[1], sdlog = slc_sev_censored_lnorm_fit()$par[2]) })

  output$sliced_sev_cdf_plot <- renderPlotly({
    LogNormalParetoInv<- function(x){
      ifelse(
        x<=input$slicing_point_left
        ,plnorm(x, meanlog = slc_sev_censored_lnorm_fit()$par[1], sdlog = slc_sev_censored_lnorm_fit()$par[2])
        ,(1-(input$slicing_point_left/x)^paretoX1Alpha())*(1-PrXLessThanParetoX1())+PrXLessThanParetoX1()

      )
    }

    PrXLessThanParetoX2<-(1-(input$slicing_point_left/input$slicing_point_right)^paretoX2AlphaMod())*(1-PrXLessThanParetoX1())+PrXLessThanParetoX1()

    LogNormalParetoParetoInv<- function(x){
      ifelse(
        x<=input$slicing_point_right
        ,ifelse(
          x<=input$slicing_point_left
          ,plnorm(x, meanlog = slc_sev_censored_lnorm_fit()$par[1], sdlog = slc_sev_censored_lnorm_fit()$par[2])
          ,(1-(input$slicing_point_left/x)^paretoX2AlphaMod())*(1-PrXLessThanParetoX1())+PrXLessThanParetoX1()
        )
        ,(1-(input$slicing_point_right/x)^paretoX2Alpha())*(1-PrXLessThanParetoX2)+PrXLessThanParetoX2

      )
    }

    x_axis_after_log_option <- if (input$sev_cens_fit_log_scale) {log(x_axis_sliced_sev())} else {x_axis_sliced_sev()}
    empirical_cdf <- unlist(lapply(x_axis_sliced_sev(), function(x) sum(sliced_sev_data() <= x)))/length(sliced_sev_data())
    p <- plot_ly(x = x_axis_after_log_option, y = empirical_cdf, type = "scatter", mode = "lines", name = "Empirical cdf")
    p <- layout(p, xaxis = list(title = paste("Claim Severity", ifelse(T,"Log Scale",""))), yaxis = list(title = "Severity log scale"))
    p <- layout(p, title =  paste("Claim Severity and severity model fit", ifelse(T,"Log Scale","")))
    lnormal_cdf <- plnorm(x_axis_sliced_sev(), meanlog=slc_sev_lnorm_fit()$estimate[1], sdlog=slc_sev_lnorm_fit()$estimate[2])
    p <- add_lines(p, x = x_axis_after_log_option, y = lnormal_cdf, type = 'scatter', mode = 'lines', name = 'LogNormal cdf')
    LogNormalParetoInv <- LogNormalParetoInv(x_axis_sliced_sev())
    p <- add_lines(p, x = x_axis_after_log_option, y = LogNormalParetoInv, type = 'scatter', mode = 'lines', name = 'LogNormal - Pareto cdf')
    LogNormalParetoParetoInv <- LogNormalParetoParetoInv(x_axis_sliced_sev())
    p <- add_lines(p, x = x_axis_after_log_option, y = LogNormalParetoParetoInv, type = 'scatter', mode = 'lines', name = 'LogNormal - Pareto - Pareto cdf')
    return(p)
  })



  output$slc_sev_fitted_param_summary <- renderPrint({
    LogNormalParetoInv<- function(x){
      ifelse(
        x<=input$slicing_point_left
        ,plnorm(x, meanlog = slc_sev_censored_lnorm_fit()$par[1], sdlog = slc_sev_censored_lnorm_fit()$par[2])
        ,(1-(input$slicing_point_left/x)^paretoX1Alpha())*(1-PrXLessThanParetoX1())+PrXLessThanParetoX1()

      )
    }

    PrXLessThanParetoX2<-(1-(input$slicing_point_left/input$slicing_point_right)^paretoX2AlphaMod())*(1-PrXLessThanParetoX1())+PrXLessThanParetoX1()

    LogNormalParetoParetoInv<- function(x){
      ifelse(
        x<=input$slicing_point_right
        ,ifelse(
          x<=input$slicing_point_left
          ,plnorm(x, meanlog = slc_sev_censored_lnorm_fit()$par[1], sdlog = slc_sev_censored_lnorm_fit()$par[2])
          ,(1-(input$slicing_point_left/x)^paretoX2AlphaMod())*(1-PrXLessThanParetoX1())+PrXLessThanParetoX1()
        )
        ,(1-(input$slicing_point_right/x)^paretoX2Alpha())*(1-PrXLessThanParetoX2)+PrXLessThanParetoX2

      )
    }

    KSlNorm <- kstest(sliced_sev_data(), plnorm(sliced_sev_data(), meanlog = slc_sev_lnorm_fit()$estimate[1], sdlog = slc_sev_lnorm_fit()$estimate[2]))
    KSlNormPareto <- kstest(sliced_sev_data(), LogNormalParetoInv(sliced_sev_data()))
    KSlNormParetoPareto <- kstest(sliced_sev_data(), LogNormalParetoParetoInv(sliced_sev_data()))

    param <- matrix(c(round(slc_sev_lnorm_fit()$estimate[1],4),round(slc_sev_lnorm_fit()$estimate[2],4),KSlNorm
                      ,"","","","mu","sigma", "",round(slc_sev_censored_lnorm_fit()$par[1],4),round(slc_sev_censored_lnorm_fit()$par[2],4),""
                      ,"","","","xm","alpha", "K-S Dist",input$slicing_point_left,round(paretoX1Alpha(),4),KSlNormPareto
                      ,"","","","xm","alpha", "",input$slicing_point_left,round(paretoX2AlphaMod(),4),""
                      ,"","","","xm","alpha", "K-S Dist",input$slicing_point_right,round(paretoX2Alpha(),4),KSlNormParetoPareto
    ),
    nrow = 13, ncol = 3, byrow = TRUE)
    colnames(param)<-c("Mu", "Sigma", "K-S Dist")
    rownames(param)<-c("LogNormal","","","LogNormal Sliced","","","Pareto x1","","","Pareto x2 lower","","","Pareto x2 upper")
    noquote(param)
  })


}
