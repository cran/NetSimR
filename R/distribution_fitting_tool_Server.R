#' Server function for the Distribution Fitting tool application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @import shiny
#' @importFrom plotly plot_ly add_lines layout add_bars renderPlotly
#' @importFrom fitdistrplus fitdist

distribution_fitting_tool_Server <- function(input, output, session) {

  ######################
  #help functions
  ######################

  or_default <- function(x, default) if (is.null(x)) default else x

  # the charts are drawn in the colours of the theme the page reports
  dark <- reactive(identical(input$app_theme, "dark"))

  # reports a failed fit to the user (instead of the console) and returns NULL,
  # so that the dependent outputs show a dash rather than an error
  failed_fit <- function(what, e) {
    showNotification(paste0(what, " failed: ", conditionMessage(e)), type = "error", duration = 10)
    NULL
  }

  # message shown in place of an output until its analysis has run
  need_run <- function(button, what) {
    validate(
      need(input$file1, "Upload a data file on the Data tab first."),
      need(isTruthy(input[[button]]), paste0("Choose the ", what, " column and click Run analysis."))
    )
  }

  # the claim sizes of a column: positive numbers only, with the number of rows left out
  claim_sizes <- function(column) {
    df <- data()
    validate(need(!is.null(column) && column %in% names(df), "Choose the claim size column."))
    values <- dft_as_numeric(df[[column]], or_default(input$dec, "."))
    keep <- is.finite(values) & values > 0
    claims <- values[keep]
    validate(
      need(length(claims) >= 2, "The column needs at least two positive claim sizes."),
      need(length(unique(claims)) >= 2, "All the claim sizes are the same, so no distribution can be fitted.")
    )
    list(claims = claims, dropped = sum(!keep))
  }

  rows_left_out_note <- function(dropped) {
    if (dropped > 0) paste(format(dropped, big.mark = ","), if (dropped == 1) "row left out" else "rows left out")
  }

  # lognormal maximum likelihood estimates, as fitdistrplus::fitdist(x, "lnorm") gives them
  fit_lnorm_mle <- function(x) {
    meanlog <- mean(log(x))
    list(estimate = c(meanlog = meanlog, sdlog = sqrt(mean((log(x) - meanlog)^2))))
  }

  # least squares fit of the Pareto alpha of the layer between the slicing points to the empirical cdf
  fit_slice_pareto <- function (sev_data, slic_pont_lft, slic_pont_rght) {
    z <- subset(sev_data, sev_data > slic_pont_lft)
    w <- subset(z, z <= slic_pont_rght)
    dat <- data.frame(severity = w, empirical = rank(w) / (length(z) + 1))
    SumOfSquares <- function(data, par) {
      sum((1 - (slic_pont_lft / data$severity)^par[1] - data$empirical)^2)
    }
    result <- optim(par = 1, SumOfSquares, data = dat, lower = 0.0001, method = "L-BFGS-B")
    return(result$par)
  }

  # empirical cdf drawn as a step line
  add_empirical_cdf <- function(p, claims, weights = NULL, points = dft_ecdf_points(claims)) {
    add_lines(p, x = points, y = empirical_cdf_at(points, claims, weights), name = "Empirical",
              line = list(color = dft_plot_colours(dark())$empirical, width = 2.5, shape = "hv"))
  }

  add_model_line <- function(p, x, y, name, i, shape = "linear") {
    add_lines(p, x = x, y = y, name = name,
              line = list(color = dft_model_palette[(i - 1) %% length(dft_model_palette) + 1], width = 2, shape = shape))
  }

  ######################
  #Data input
  ######################

  data <- reactive({
    validate(need(input$file1, "Upload a data file to see a preview here."))
    read_error <- NULL
    df <- tryCatch(
      dft_read_data(
        input$file1$datapath,
        header = isTRUE(input$data_includes_header),
        sep = or_default(input$sep, ","),
        quote = or_default(input$quote, "\""),
        dec = or_default(input$dec, ".")
      ),
      error = function(e) {
        read_error <<- conditionMessage(e)
        NULL
      }
    )
    validate(need(is.null(read_error), paste0("The file could not be read (", read_error, "). Check the separator and quote settings.")))
    validate(need(ncol(df) > 0 && nrow(df) > 0, "The file has no data rows."))
    df
  })

  numeric_columns <- reactive(dft_numeric_columns(data(), or_default(input$dec, ".")))

  # offer the columns of the file in every analysis tab, keeping a choice the new file also has;
  # otherwise guess from the column names, then from the values
  observeEvent(data(), {
    df <- data()
    columns <- names(df)
    numbers <- numeric_columns()
    by_name <- function(pattern) {
      matched <- numbers[grepl(pattern, numbers, ignore.case = TRUE)]
      if (length(matched) > 0) matched[1] else NULL
    }
    means <- vapply(numbers, function(column) mean(dft_as_numeric(df[[column]], or_default(input$dec, ".")), na.rm = TRUE), numeric(1))
    first <- if (length(numbers) > 0) numbers[1] else columns[1]
    counts_default <- or_default(by_name("count|freq|number|claims?_n|n_claims"), first)
    severity_default <- or_default(by_name("amount|sever|size|loss|cost|paid|incurred|value"),
                                   if (length(numbers) > 0) numbers[which.max(means)] else first)
    weights_default <- or_default(by_name("weight|exposure"), setdiff(c(numbers, first), counts_default)[1])
    choose <- function(id, default) {
      current <- input[[id]]
      if (!is.null(current) && current %in% columns) current else default
    }
    updateSelectizeInput(session, "counts_var", choices = columns, selected = choose("counts_var", counts_default))
    updateSelectizeInput(session, "counts_weights_var", choices = columns, selected = choose("counts_weights_var", weights_default))
    for (id in c("severity_var", "sliced_sev_var", "piecewise_pareto_var")) {
      updateSelectizeInput(session, id, choices = columns, selected = choose(id, severity_default))
    }
  })

  preview_rows <- 10000

  output$data_overview <- renderUI({
    req(input$file1)
    df <- data()
    truncated <- nrow(df) > preview_rows
    div(
      class = "dft-stats",
      dft_stat_tile("File", div(class = "dft-file-name", input$file1$name), icon_name = "file-lines"),
      dft_stat_tile("Rows", format(nrow(df), big.mark = ","), icon_name = "bars",
                    note = if (truncated) paste("The preview shows the first", format(preview_rows, big.mark = ","))),
      dft_stat_tile("Columns", format(ncol(df), big.mark = ","), icon_name = "table-columns",
                    note = if (ncol(df) == 1) "Only one column: check the separator" else paste(length(numeric_columns()), "numeric"))
    )
  })

  output$data_table <- reactable::renderReactable({
    df <- data()
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
  #Frequency analysis
  ######################

  # counts and weights are cleaned together, so that each count keeps the weight of its own row
  freq_input <- eventReactive(input$execute_freq_analysis, {
    df <- data()
    validate(need(!is.null(input$counts_var) && input$counts_var %in% names(df), "Choose the claim counts column."))
    dec <- or_default(input$dec, ".")
    counts <- dft_as_numeric(df[[input$counts_var]], dec)
    weighted <- isTRUE(input$counts_weighted_var)
    if (weighted) {
      validate(need(!is.null(input$counts_weights_var) && input$counts_weights_var %in% names(df), "Choose the weights column."))
      weights <- dft_as_numeric(df[[input$counts_weights_var]], dec)
    } else {
      weights <- rep(1, length(counts))
    }
    keep <- is.finite(counts) & counts >= 0 & is.finite(weights) & weights > 0
    counts <- counts[keep]
    weights <- weights[keep]
    validate(
      need(length(counts) >= 2, "The column needs at least two valid claim counts."),
      need(all(counts == round(counts)),
           "Claim counts must be whole numbers. For claim amounts, use the Severity tab."),
      need(!weighted || all(weights == round(weights)),
           "Weights must be whole numbers: each row counts as that many observations.")
    )
    list(counts = counts, weights = if (weighted) weights else NULL, weighted = weighted, dropped = sum(!keep))
  })

  counts_data <- reactive(freq_input()$counts)
  weights_data <- reactive(freq_input()$weights)

  fit_counts <- function(distr, label) {
    x <- freq_input()
    tryCatch(
      suppressWarnings(
        if (x$weighted) fitdist(x$counts, distr, weights = x$weights) else fitdist(x$counts, distr)
      ),
      error = function(e) failed_fit(label, e)
    )
  }
  freq_po_fit <- reactive(fit_counts("pois", "Poisson fit"))
  freq_nb_fit <- reactive(fit_counts("nbinom", "Negative Binomial fit"))

  # weighted moments; without weights they are the usual mean and sample variance
  freq_moments <- reactive({
    x <- freq_input()
    w <- if (is.null(x$weights)) rep(1, length(x$counts)) else x$weights
    total <- sum(w)
    mean <- sum(w * x$counts) / total
    list(n = total, mean = mean, variance = sum(w * (x$counts - mean)^2) / (total - 1))
  })

  fit_value <- function(fit, name) if (is.null(fit)) NA_real_ else unname(fit$estimate[[name]])
  fit_stat <- function(fit, name) if (is.null(fit)) NA_real_ else fit[[name]]

  output$freq_stats <- renderUI({
    m <- freq_moments()
    x <- freq_input()
    overdispersed <- isTRUE(m$variance > m$mean)
    div(
      class = "dft-stats",
      dft_stat_tile(if (x$weighted) "Weighted observations" else "Observations", format(m$n, big.mark = ","),
                    note = rows_left_out_note(x$dropped), icon_name = "hashtag"),
      dft_stat_tile("Mean", dft_fmt(m$mean), icon_name = "bullseye"),
      dft_stat_tile("Variance", dft_fmt(m$variance), icon_name = "arrows-left-right",
                    note = if (m$mean > 0) paste("Variance / mean:", dft_fmt(m$variance / m$mean, 3))),
      dft_stat_tile("Suggested model", if (overdispersed) "Negative Binomial" else "Poisson",
                    note = if (overdispersed) "The variance is above the mean" else "The variance is not above the mean",
                    icon_name = "lightbulb", accent = TRUE)
    )
  })

  output$selected_freq_params <- renderUI({
    need_run("execute_freq_analysis", "claim counts")
    po <- freq_po_fit()
    nb <- freq_nb_fit()
    size <- fit_value(nb, "size")
    rows <- list(
      list("Poisson", dft_param_cell("Mean", fit_value(po, "lambda")), dft_param_cell("", NA),
           dft_fmt(fit_stat(po, "loglik"), 6), dft_fmt(fit_stat(po, "aic"), 6)),
      # Beta = mu / size, as the simulator's Negative Binomial: mean r * Beta, variance r * Beta * (1 + Beta)
      list("Negative Binomial", dft_param_cell("r", size), dft_param_cell("Beta", fit_value(nb, "mu") / size),
           dft_fmt(fit_stat(nb, "loglik"), 6), dft_fmt(fit_stat(nb, "aic"), 6))
    )
    aic <- c(fit_stat(po, "aic"), fit_stat(nb, "aic"))
    dft_html_table(
      c("Distribution", "Parameter 1", "Parameter 2", "Log-likelihood", "AIC"), rows,
      best = if (any(is.finite(aic))) which.min(aic), best_label = "Lowest AIC",
      note = "Negative Binomial with mean r x Beta and variance r x Beta x (1 + Beta), as in the NetSimR simulator."
    )
  })

  output$freq_summary <- renderPrint({
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    print(summary(x$counts))
    if (x$weighted) cat("\nTotal weight:", format(sum(x$weights), big.mark = ","), "\n")
  })

  output$selected_distribution_summary <- renderPrint({
    need_run("execute_freq_analysis", "claim counts")
    fit <- if (identical(input$FreqDistri, "NegativeBinomial")) freq_nb_fit() else freq_po_fit()
    validate(need(!is.null(fit), "This distribution could not be fitted to the data."))
    summary(fit)
  })

  # observed share of observations in each bin of counts, against the probability of each fitted model
  output$count_hist <- renderPlotly({
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    counts <- x$counts
    w <- if (is.null(x$weights)) rep(1, length(counts)) else x$weights
    lowest <- min(counts)
    width <- max(1, ceiling((max(counts) - lowest + 1) / max(1, or_default(input$count_hist_bins, 20))))
    starts <- seq(lowest, max(counts), by = width)
    ends <- starts + width - 1
    bin <- (counts - lowest) %/% width + 1
    observed <- vapply(seq_along(starts), function(i) sum(w[bin == i]), numeric(1)) / sum(w)
    labels <- if (width == 1) format(starts, scientific = FALSE, trim = TRUE) else
      paste0(format(starts, scientific = FALSE, trim = TRUE), "-", format(ends, scientific = FALSE, trim = TRUE))
    colours <- dft_plot_colours(dark())

    p <- plot_ly()
    p <- add_bars(p, x = labels, y = observed, name = "Observed",
                  marker = list(color = colours$bar, line = list(color = colours$bar_line, width = 1)))
    models <- list(
      list(name = "Poisson", fit = freq_po_fit(), cdf = function(q, fit) ppois(q, fit_value(fit, "lambda"))),
      list(name = "Negative Binomial", fit = freq_nb_fit(),
           cdf = function(q, fit) pnbinom(q, size = fit_value(fit, "size"), mu = fit_value(fit, "mu")))
    )
    for (i in seq_along(models)) {
      if (is.null(models[[i]]$fit)) next
      probability <- models[[i]]$cdf(ends, models[[i]]$fit) - models[[i]]$cdf(starts - 1, models[[i]]$fit)
      p <- plotly::add_trace(p, x = labels, y = probability, type = "scatter", mode = "lines+markers", name = models[[i]]$name,
                             line = list(color = dft_model_palette[i], width = 2), marker = list(color = dft_model_palette[i], size = 6))
    }
    p <- dft_plot_layout(p, "Number of claims", "Share of observations", dark())
    layout(p, xaxis = list(type = "category"), yaxis = list(tickformat = ".0%"), bargap = 0.08)
  })

  output$freq_fit_plot <- renderPlotly({
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    top <- max(x$counts)
    points <- if (top <= 5000) seq(0, top) else unique(round(seq(0, top, length.out = 5000)))
    p <- add_empirical_cdf(plot_ly(), x$counts, x$weights, points)
    po <- freq_po_fit()
    if (!is.null(po)) p <- add_model_line(p, points, ppois(points, fit_value(po, "lambda")), "Poisson", 1, "hv")
    nb <- freq_nb_fit()
    if (!is.null(nb)) {
      p <- add_model_line(p, points, pnbinom(points, size = fit_value(nb, "size"), mu = fit_value(nb, "mu")),
                          "Negative Binomial", 2, "hv")
    }
    dft_plot_layout(p, "Number of claims", "Cumulative probability", dark(), y_range = c(0, 1.02))
  })

  ######################
  #Severity analysis
  ######################

  sev_input <- eventReactive(input$execute_sev_analysis, claim_sizes(input$severity_var))
  severity_data <- reactive(sev_input()$claims)

  # maximum likelihood fits
  sev_norm_fit <- reactive({
    x <- severity_data()
    list(estimate = c(mean = mean(x), sd = sqrt(mean((x - mean(x))^2))))
  })
  sev_lnorm_fit <- reactive(fit_lnorm_mle(severity_data()))
  sev_gamma_fit <- reactive({
    tryCatch(fit_gamma_mle(severity_data()), error = function(e) failed_fit("Gamma fit", e))
  })
  sev_pareto_xm <- reactive(min(severity_data()))
  sev_pareto_alpha <- reactive(length(severity_data()) / sum(log(severity_data() / sev_pareto_xm())))

  # the fitted models, each with its parameters, mean, cdf and log-likelihood
  sev_models <- reactive({
    x <- severity_data()
    norm <- unname(sev_norm_fit()$estimate)
    lnorm <- unname(sev_lnorm_fit()$estimate)
    gamma <- if (is.null(sev_gamma_fit())) c(NA_real_, NA_real_) else unname(sev_gamma_fit()$estimate)
    xm <- sev_pareto_xm()
    alpha <- sev_pareto_alpha()
    rate <- 1 / mean(x)
    pareto_cdf <- function(q) ifelse(q < xm, 0, 1 - (xm / q)^alpha)
    list(
      list(name = "Normal", params = c(Mean = norm[1], `Std. dev.` = norm[2]), mean = norm[1],
           cdf = function(q) pnorm(q, norm[1], norm[2]), loglik = sum(dnorm(x, norm[1], norm[2], log = TRUE)), k = 2),
      list(name = "LogNormal", params = c(mu = lnorm[1], sigma = lnorm[2]), mean = exp(lnorm[1] + lnorm[2]^2 / 2),
           cdf = function(q) plnorm(q, lnorm[1], lnorm[2]), loglik = sum(dlnorm(x, lnorm[1], lnorm[2], log = TRUE)), k = 2),
      list(name = "Exponential", params = c(Mean = mean(x)), mean = mean(x),
           cdf = function(q) pexp(q, rate), loglik = sum(dexp(x, rate, log = TRUE)), k = 1),
      list(name = "Gamma", params = c(Scale = gamma[1], Shape = gamma[2]), mean = gamma[1] * gamma[2],
           cdf = if (anyNA(gamma)) NULL else function(q) pgamma(q, scale = gamma[1], shape = gamma[2]),
           loglik = sum(dgamma(x, scale = gamma[1], shape = gamma[2], log = TRUE)), k = 2),
      list(name = "Pareto", params = c(xm = xm, alpha = alpha), mean = if (alpha > 1) alpha * xm / (alpha - 1) else Inf,
           cdf = pareto_cdf, loglik = length(x) * (log(alpha) + alpha * log(xm)) - (alpha + 1) * sum(log(x)), k = 2)
    )
  })

  output$sev_stats <- renderUI({
    x <- severity_data()
    div(
      class = "dft-stats",
      dft_stat_tile("Claims", format(length(x), big.mark = ","), note = rows_left_out_note(sev_input()$dropped), icon_name = "hashtag"),
      dft_stat_tile("Mean", dft_fmt(mean(x)), icon_name = "bullseye"),
      dft_stat_tile("Median", dft_fmt(median(x)), icon_name = "arrows-left-right"),
      dft_stat_tile("Coefficient of variation", dft_fmt(sd(x) / mean(x), 3), icon_name = "wave-square"),
      dft_stat_tile("Largest claim", dft_fmt(max(x)), icon_name = "arrow-up-right-dots")
    )
  })

  output$sev_param_summary <- renderUI({
    need_run("execute_sev_analysis", "claim size")
    x <- severity_data()
    models <- sev_models()
    ks <- vapply(models, function(m) if (is.null(m$cdf)) NA_real_ else ks_distance(x, m$cdf), numeric(1))
    rows <- lapply(seq_along(models), function(i) {
      m <- models[[i]]
      params <- names(m$params)
      list(
        m$name,
        dft_param_cell(params[1], m$params[[1]]),
        if (length(params) > 1) dft_param_cell(params[2], m$params[[2]]) else dft_param_cell("", NA),
        if (is.infinite(m$mean)) "Infinite" else dft_fmt(m$mean),
        dft_fmt(ks[i], 3),
        dft_fmt(2 * m$k - 2 * m$loglik, 6)
      )
    })
    dft_html_table(
      c("Distribution", "Parameter 1", "Parameter 2", "Mean", "K-S distance", "AIC"), rows,
      best = if (any(is.finite(ks))) which.min(ks), best_label = "Closest",
      note = paste0("Empirical mean ", dft_fmt(mean(x)), ". The Pareto starts at the smallest claim.")
    )
  })

  output$sev_hist <- renderPlotly({
    need_run("execute_sev_analysis", "claim size")
    colours <- dft_plot_colours(dark())
    p <- plot_ly(x = severity_data(), type = "histogram", nbinsx = or_default(input$severity_hist_bins, 20), name = "Claims",
                 marker = list(color = colours$bar, line = list(color = colours$bar_line, width = 1)))
    dft_plot_layout(p, "Claim size", "Number of claims", dark(), hovermode = "closest")
  })

  output$sev_summary <- renderPrint({
    need_run("execute_sev_analysis", "claim size")
    x <- severity_data()
    print(summary(x))
    cat("\nPercentiles\n")
    print(quantile(x, c(0.75, 0.9, 0.95, 0.99, 0.995)))
  })

  output$sev_fit_plot <- renderPlotly({
    need_run("execute_sev_analysis", "claim size")
    x <- severity_data()
    log_scale <- isTRUE(input$sev_fit_log_scale)
    grid <- dft_severity_grid(x, log_scale)
    p <- add_empirical_cdf(plot_ly(), x)
    models <- sev_models()
    for (i in seq_along(models)) {
      if (!is.null(models[[i]]$cdf)) p <- add_model_line(p, grid, models[[i]]$cdf(grid), models[[i]]$name, i)
    }
    dft_plot_layout(p, "Claim size", "Cumulative probability", dark(), x_log = log_scale, y_range = c(0, 1.02))
  })

  ######################
  #Sliced Sev analysis
  ######################

  sliced_input <- eventReactive(input$execute_sliced_sev_analysis, claim_sizes(input$sliced_sev_var))
  sliced_sev_data <- reactive(sliced_input()$claims)

  # the sliders cover the data; they start at the 75th and 95th percentiles
  observeEvent(sliced_input(), {
    x <- sliced_sev_data()
    lowest <- dft_nice_bound(min(x))
    highest <- dft_nice_bound(max(x), up = TRUE)
    left <- signif(quantile(x, 0.75, names = FALSE), 3)
    right <- signif(quantile(x, 0.95, names = FALSE), 3)
    if (right <= left) right <- signif((left + max(x)) / 2, 3)
    # a fine step, so that claims below 1 or heavy tails can still be sliced precisely
    step <- signif((highest - lowest) / 1000, 1)
    updateSliderInput(session, "slicing_point_left", min = lowest, max = highest, value = left, step = step)
    updateSliderInput(session, "slicing_point_right", min = lowest, max = highest, value = right, step = step)
  })

  # moving the first point past the second moves the second above it
  observeEvent(input$slicing_point_left, {
    x <- sliced_sev_data()
    if (isTRUE(input$slicing_point_right <= input$slicing_point_left)) {
      updateSliderInput(session, "slicing_point_right", value = signif((input$slicing_point_left + max(x)) / 2, 3))
    }
  }, ignoreInit = TRUE)

  slicing_points <- reactive({
    x <- sliced_sev_data()
    x1 <- req(input$slicing_point_left)
    x2 <- req(input$slicing_point_right)
    below <- x[x <= x1]
    validate(
      need(x2 > x1, "The second slicing point must be above the first."),
      need(length(unique(below)) >= 2, "Move the first slicing point up: at least two different claims must lie below it."),
      need(any(x > x1 & x <= x2), "Move the slicing points apart: some claims must lie between them."),
      need(any(x > x2), "Move the second slicing point down: some claims must lie above it.")
    )
    c(x1, x2)
  })

  slc_sev_lnorm_fit <- reactive(fit_lnorm_mle(sliced_sev_data()))

  # least squares fit of the lognormal body to the empirical cdf below the first slicing point
  slc_sev_censored_lnorm_fit <- reactive({
    x1 <- slicing_points()[1]
    tryCatch({
      w <- sliced_sev_data()[sliced_sev_data() <= x1]
      dat <- data.frame(severity = w, empirical = rank(w) / (length(sliced_sev_data()) + 1))
      SumOfSquares <- function(data, par) {
        sum((plnorm(data$severity, meanlog = par[1], sdlog = par[2]) - data$empirical)^2)
      }
      # meanlog can be negative (claims below 1); only sdlog must be positive
      result <- optim(par = c(mean(log(w)), sd(log(w))), SumOfSquares, data = dat,
                      lower = c(-Inf, 0.0001), method = "L-BFGS-B")
      if (result$convergence != 0) stop("the optimisation did not converge")
      result
    }, error = function(e) {
      failed_fit("Sliced LogNormal fit", e)
      list(par = rep(NA_real_, 2))
    })
  })

  zGreaterThanXm1 <- reactive(sliced_sev_data()[sliced_sev_data() > slicing_points()[1]])
  zGreaterThanXm2 <- reactive(sliced_sev_data()[sliced_sev_data() > slicing_points()[2]])
  paretoX1Alpha <- reactive(length(zGreaterThanXm1()) / sum(log(zGreaterThanXm1() / slicing_points()[1])))
  paretoX2AlphaMod <- reactive({
    tryCatch(
      fit_slice_pareto(sliced_sev_data(), slicing_points()[1], slicing_points()[2]),
      error = function(e) {
        failed_fit("Sliced Pareto fit", e)
        NA_real_
      }
    )
  })
  paretoX2Alpha <- reactive(length(zGreaterThanXm2()) / sum(log(zGreaterThanXm2() / slicing_points()[2])))

  # cdfs of the LogNormal - Pareto and LogNormal - Pareto - Pareto models
  sliced_cdfs <- reactive({
    x1 <- slicing_points()[1]
    x2 <- slicing_points()[2]
    body <- slc_sev_censored_lnorm_fit()$par
    a1 <- paretoX1Alpha()
    a2_lower <- paretoX2AlphaMod()
    a2_upper <- paretoX2Alpha()
    p1 <- plnorm(x1, meanlog = body[1], sdlog = body[2])
    p2 <- (1 - (x1 / x2)^a2_lower) * (1 - p1) + p1
    list(
      one = function(q) {
        ifelse(q <= x1, plnorm(q, meanlog = body[1], sdlog = body[2]), (1 - (x1 / q)^a1) * (1 - p1) + p1)
      },
      two = function(q) {
        ifelse(q <= x2,
               ifelse(q <= x1, plnorm(q, meanlog = body[1], sdlog = body[2]), (1 - (x1 / q)^a2_lower) * (1 - p1) + p1),
               (1 - (x2 / q)^a2_upper) * (1 - p2) + p2)
      }
    )
  })

  output$slc_sev_fitted_param_summary <- renderUI({
    need_run("execute_sliced_sev_analysis", "claim size")
    x <- sliced_sev_data()
    points <- slicing_points()
    whole <- unname(slc_sev_lnorm_fit()$estimate)
    body <- slc_sev_censored_lnorm_fit()$par
    cdfs <- sliced_cdfs()
    ks <- c(ks_distance(x, function(q) plnorm(q, whole[1], whole[2])), ks_distance(x, cdfs$one), ks_distance(x, cdfs$two))
    blank <- ""
    rows <- list(
      list("LogNormal", "Whole distribution", dft_param_cell("mu", whole[1]), dft_param_cell("sigma", whole[2]), dft_fmt(ks[1], 3)),
      list("LogNormal - Pareto", "Body below the first point", dft_param_cell("mu", body[1]), dft_param_cell("sigma", body[2]), dft_fmt(ks[2], 3)),
      list(blank, "Tail above the first point", dft_param_cell("xm", points[1]), dft_param_cell("alpha", paretoX1Alpha()), blank),
      list("LogNormal - Pareto - Pareto", "Body below the first point", dft_param_cell("mu", body[1]), dft_param_cell("sigma", body[2]), dft_fmt(ks[3], 3)),
      list(blank, "Layer between the points", dft_param_cell("xm", points[1]), dft_param_cell("alpha", paretoX2AlphaMod()), blank),
      list(blank, "Tail above the second point", dft_param_cell("xm", points[2]), dft_param_cell("alpha", paretoX2Alpha()), blank)
    )
    dft_html_table(
      c("Model", "Part", "Parameter 1", "Parameter 2", "K-S distance"), rows,
      best = if (any(is.finite(ks))) c(1, 2, 4)[which.min(ks)], best_label = "Closest",
      note = paste("The body and the alpha of the layer between the points are fitted to the empirical cdf by least squares;",
                   "the alphas of the tails above each point are maximum likelihood estimates.")
    )
  })

  output$sliced_sev_cdf_plot <- renderPlotly({
    need_run("execute_sliced_sev_analysis", "claim size")
    x <- sliced_sev_data()
    points <- slicing_points()
    log_scale <- isTRUE(input$sev_cens_fit_log_scale)
    grid <- sort(unique(c(dft_severity_grid(x, log_scale), points)))
    whole <- unname(slc_sev_lnorm_fit()$estimate)
    cdfs <- sliced_cdfs()
    p <- add_empirical_cdf(plot_ly(), x)
    p <- add_model_line(p, grid, plnorm(grid, whole[1], whole[2]), "LogNormal", 1)
    p <- add_model_line(p, grid, cdfs$one(grid), "LogNormal - Pareto", 2)
    p <- add_model_line(p, grid, cdfs$two(grid), "LogNormal - Pareto - Pareto", 3)
    p <- dft_plot_layout(p, "Claim size", "Cumulative probability", dark(), x_log = log_scale, y_range = c(0, 1.02))
    layout(p, shapes = dft_vlines(points, log_scale, dark()))
  })

  output$mean_excess_func_plot <- renderPlotly({
    need_run("execute_sliced_sev_analysis", "claim size")
    x <- sliced_sev_data()
    log_scale <- isTRUE(input$sev_cens_fit_log_scale)
    points <- dft_ecdf_points(x)
    mean_excess <- mean_excess_at(points, x)
    # no claim exceeds the largest point, so its mean excess is undefined
    defined <- is.finite(mean_excess)
    p <- add_lines(plot_ly(), x = points[defined], y = mean_excess[defined], name = "Mean excess",
                   line = list(color = dft_plot_colours(dark())$bar, width = 2))
    p <- dft_plot_layout(p, "Threshold", "Mean excess over the threshold", dark(), x_log = log_scale, hovermode = "closest")
    points_ok <- tryCatch(slicing_points(), error = function(e) NULL)
    if (!is.null(points_ok)) p <- layout(p, shapes = dft_vlines(points_ok, log_scale, dark()))
    p
  })

  ######################
  #piecewise pareto
  ######################

  piecewise_input <- eventReactive(input$execute_piecewise_sev_analysis, claim_sizes(input$piecewise_pareto_var))
  piecewise_sev_data <- reactive(sort(piecewise_input()$claims))

  # one slider per threshold, from the second smallest to the second largest distinct claim;
  # a threshold already set is kept when it is still in range
  dynamic_sliders <- eventReactive(list(input$num_pareto_slices, input$execute_piecewise_sev_analysis), {
    sorted_claims <- piecewise_sev_data()
    distinct <- unique(sorted_claims)
    validate(need(length(distinct) >= 4, "The thresholds need at least four different claim sizes."))
    min_val <- distinct[2]
    max_val <- distinct[length(distinct) - 1]
    num_sliders <- or_default(input$num_pareto_slices, 1)
    step <- signif((max_val - min_val) / 1000, 1)
    tagList(lapply(seq_len(num_sliders), function(i) {
      id <- paste0("slider_", i)
      current <- isolate(input[[id]])
      # the 50th, 75th, 87.5th, ... percentiles: thresholds get closer together towards the tail
      default <- quantile(sorted_claims, 1 - 0.5^i, names = FALSE)
      value <- if (!is.null(current) && current >= min_val && current <= max_val) current else min(max(signif(default, 4), min_val), max_val)
      sliderInput(id, label = paste("Threshold", i), min = min_val, max = max_val, value = value,
                  step = if (step > 0) step else NULL)
    }))
  })

  output$pareto_slider_inputs <- renderUI({
    dynamic_sliders()
  })

  piecwise_pareto_mu <- reactive({
    # nothing to do until the analysis has been executed
    lowest_claim <- min(piecewise_sev_data())
    num_sliders <- req(input$num_pareto_slices)
    # sliders that are not rendered yet are NULL and are dropped by unlist()
    slider_values <- unlist(lapply(seq_len(num_sliders), function(i) input[[paste0("slider_", i)]]))
    sort(c(lowest_claim, slider_values))
  })

  piecwise_pareto_alpha <- reactive({
    piecewise_pareto_alpha(piecewise_sev_data(), piecwise_pareto_mu())
  })

  # the thresholds must be usable before the fit is shown
  piecewise_ready <- reactive({
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    validate(
      need(length(mu) == or_default(input$num_pareto_slices, 1) + 1, "Setting up the thresholds..."),
      need(!anyDuplicated(mu), "The thresholds must all be different and above the smallest claim."),
      need(max(mu) < max(x), "The largest threshold must be below the largest claim.")
    )
    TRUE
  })

  empirical_piecwise_cdf <- reactive(seq_along(piecewise_sev_data()) / length(piecewise_sev_data()))
  predicted_piecwise_cdf <- reactive(piecewise_pareto_cdf(piecewise_sev_data(), piecwise_pareto_mu(), piecwise_pareto_alpha()))

  output$fitted_sliced_pareto <- renderUI({
    need_run("execute_piecewise_sev_analysis", "claim size")
    piecewise_ready()
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    upper <- c(mu[-1], Inf)
    rows <- lapply(seq_along(mu), function(i) {
      list(paste("Layer", i), dft_fmt(mu[i]), if (is.finite(upper[i])) dft_fmt(upper[i]) else "and above",
           format(sum(x >= mu[i] & x < upper[i]), big.mark = ","), dft_fmt(alpha[i]))
    })
    dft_html_table(c("Layer", "From", "To", "Claims", "Alpha"), rows,
                   note = "Each alpha is the maximum likelihood estimate for the claims in its layer.")
  })

  output$piecewise_pareto_ks_test <- renderUI({
    need_run("execute_piecewise_sev_analysis", "claim size")
    piecewise_ready()
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    div(
      class = "dft-stats",
      dft_stat_tile("Claims", format(length(x), big.mark = ","), note = rows_left_out_note(piecewise_input()$dropped), icon_name = "hashtag"),
      dft_stat_tile("Layers", length(mu), icon_name = "layer-group"),
      dft_stat_tile("K-S distance", dft_fmt(ks_distance(x, function(q) piecewise_pareto_cdf(q, mu, alpha)), 3),
                    note = "Largest gap between the empirical and fitted cdf", icon_name = "ruler", accent = TRUE)
    )
  })

  # the summary tiles are hidden while empty, and shiny does not render hidden outputs,
  # so they must render even when hidden or they would never appear
  for (id in c("data_overview", "freq_stats", "sev_stats", "piecewise_pareto_ks_test")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  output$piecewise_pareto_cdf_plot <- renderPlotly({
    need_run("execute_piecewise_sev_analysis", "claim size")
    piecewise_ready()
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    log_scale <- isTRUE(input$piecewise_pareto_fit_log_scale)
    grid <- sort(unique(c(dft_severity_grid(x, log_scale), mu)))
    p <- add_empirical_cdf(plot_ly(), x)
    p <- add_model_line(p, grid, piecewise_pareto_cdf(grid, mu, alpha), "Piecewise Pareto", 1)
    p <- dft_plot_layout(p, "Claim size", "Cumulative probability", dark(), x_log = log_scale, y_range = c(0, 1.02))
    layout(p, shapes = dft_vlines(mu, log_scale, dark()))
  })

}
