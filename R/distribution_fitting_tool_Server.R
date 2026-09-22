#' Server function for the Distribution Fitting tool application
#'
#' @description Reads the uploaded file, fits the frequency, severity, sliced
#'   and piecewise Pareto models the user asks for and renders their tables and
#'   charts; paired with \code{distribution_fitting_tool_UI} by
#'   \code{\link{run_shiny_distribution_fitting_tool}}.
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @keywords internal
#' @import shiny

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
    # with one or two claims the sum of squares is nearly flat: the optimiser stops at
    # whatever alpha it reaches, an arbitrary number that would be shown as the fit
    if (length(w) < 3) stop("at least three claims must lie between the slicing points")
    dat <- data.frame(severity = w, empirical = rank(w) / (length(z) + 1))
    SumOfSquares <- function(data, par) {
      sum((1 - (slic_pont_lft / data$severity)^par[1] - data$empirical)^2)
    }
    result <- optim(par = 1, SumOfSquares, data = dat, lower = 0.0001, method = "L-BFGS-B")
    if (result$convergence != 0) stop("the optimisation did not converge")
    return(result$par)
  }

  # a chart, drawn as a PNG image on a transparent background; each chart reads dark(),
  # so it is redrawn in the colours of the other theme when the theme changes, and it is
  # redrawn when resized, so that its legend is laid out for the new width
  chart <- function(draw, alt) netsimr_render_plot(draw(), bg = "transparent", res = 96, alt = alt)

  # the fitted cdfs of claim size models that have one, named, with the colour of each model
  model_cdfs <- function(models) {
    has_cdf <- !vapply(models, function(m) is.null(m$cdf), logical(1))
    list(cdfs = stats::setNames(lapply(models[has_cdf], function(m) m$cdf), vapply(models[has_cdf], function(m) m$name, "")),
         colours = dft_model_palette[which(has_cdf)])
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

  # the columns of the last read (names and which are numeric), the guess made for each
  # select and the column each select was given
  column_choices <- reactiveVal(list(layout = NULL, guesses = list(), selected = list()))

  # offer the columns of the file in every analysis tab, guessing from the column names, then
  # from the values. A column the user chose is kept while the file still has it; a guess is
  # made again when the columns or their types change (a file first read with the wrong
  # separator or decimal mark gives other columns, or other numeric columns, once corrected)
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
    defaults <- list(counts_var = counts_default, counts_weights_var = weights_default, severity_var = severity_default,
                     sliced_sev_var = severity_default, piecewise_pareto_var = severity_default)
    previous <- column_choices()
    layout <- list(columns = columns, numbers = numbers)
    changed <- !identical(layout, previous$layout)
    guesses <- previous$guesses
    selected <- list()
    for (id in names(defaults)) {
      current <- input[[id]]
      # still the guess (the user has not chosen another column), made from other columns
      stale_guess <- changed && identical(current, guesses[[id]])
      if (is.null(current) || !(current %in% columns) || stale_guess) {
        selected[[id]] <- defaults[[id]]
        guesses[[id]] <- defaults[[id]]
      } else {
        selected[[id]] <- current
      }
      updateSelectizeInput(session, id, choices = columns, selected = selected[[id]])
    }
    column_choices(list(layout = layout, guesses = guesses, selected = selected))
  })

  # the preview shows the first rows; the analyses use every row
  preview_rows <- 100

  output$data_overview <- renderUI({
    req(input$file1)
    df <- data()
    truncated <- nrow(df) > preview_rows
    # a file without a header read with one: its first row became the column names
    numeric_names <- isTRUE(input$data_includes_header) && !anyNA(dft_as_numeric(names(df), or_default(input$dec, ".")))
    columns_note <- if (numeric_names) {
      "The column names look like numbers: is the first row a header?"
    } else if (ncol(df) == 1) {
      "Only one column: check the separator"
    } else {
      paste(length(numeric_columns()), "numeric")
    }
    div(
      class = "dft-stats",
      dft_stat_tile("File", div(class = "dft-file-name", input$file1$name), icon_name = "file-lines"),
      dft_stat_tile("Rows", format(nrow(df), big.mark = ","), icon_name = "bars",
                    note = if (truncated) paste("The preview shows the first", format(preview_rows, big.mark = ","))),
      dft_stat_tile("Columns", format(ncol(df), big.mark = ","), icon_name = "table-columns", note = columns_note)
    )
  })

  output$data_table <- renderUI(dft_data_preview(data(), preview_rows))

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
      # the Poisson mean would be 0 and there is no Negative Binomial to fit
      need(any(counts > 0), "All the claim counts are zero, so no distribution can be fitted."),
      need(all(counts == round(counts)),
           "Claim counts must be whole numbers. For claim amounts, use the Severity tab."),
      need(!weighted || all(weights == round(weights)),
           "Weights must be whole numbers: each row counts as that many observations.")
    )
    list(counts = counts, weights = if (weighted) weights else NULL, weighted = weighted, dropped = sum(!keep))
  })

  counts_data <- reactive(freq_input()$counts)
  weights_data <- reactive(freq_input()$weights)

  fit_counts <- function(fit, label) {
    x <- freq_input()
    tryCatch(fit(x$counts, x$weights), error = function(e) failed_fit(label, e))
  }
  freq_po_fit <- reactive(fit_counts(fit_poisson_mle, "Poisson fit"))
  freq_nb_fit <- reactive(fit_counts(fit_nbinom_mle, "Negative Binomial fit"))

  # the fitted cdfs for the charts, named, with the colour of each model; a model that failed is left out
  count_cdfs <- reactive({
    po <- freq_po_fit()
    nb <- freq_nb_fit()
    cdfs <- list()
    if (!is.null(po)) cdfs$Poisson <- function(q) ppois(q, fit_value(po, "lambda"))
    if (!is.null(nb)) cdfs[["Negative Binomial"]] <- function(q) pnbinom(q, size = fit_value(nb, "size"), mu = fit_value(nb, "mu"))
    list(cdfs = cdfs, colours = dft_model_palette[match(names(cdfs), c("Poisson", "Negative Binomial"))])
  })

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
    # the suggestion follows the fits: the Negative Binomial when it has a size and the lower AIC
    nb <- freq_nb_fit()
    nb_better <- !is.null(nb) && !isTRUE(nb$capped) && isTRUE(nb$aic < fit_stat(freq_po_fit(), "aic"))
    note <- if (nb_better) {
      "Lower AIC: the counts are overdispersed"
    } else if (isTRUE(m$variance > m$mean)) {
      "Too little overdispersion for the Negative Binomial"
    } else {
      "The variance is not above the mean"
    }
    div(
      class = "dft-stats",
      dft_stat_tile(if (x$weighted) "Weighted observations" else "Observations", dft_fmt_count(m$n),
                    note = rows_left_out_note(x$dropped), icon_name = "hashtag"),
      dft_stat_tile("Mean", dft_fmt(m$mean), icon_name = "bullseye"),
      dft_stat_tile("Variance", dft_fmt(m$variance), icon_name = "arrows-left-right",
                    note = if (m$mean > 0) paste("Variance / mean:", dft_fmt(m$variance / m$mean, 3))),
      dft_stat_tile("Suggested model", if (nb_better) "Negative Binomial" else "Poisson", note = note,
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
      note = paste0(
        "Negative Binomial with mean r x Beta and variance r x Beta x (1 + Beta), as in the NetSimR simulator.",
        if (isTRUE(nb$capped)) paste0(" The variance is not above the mean, so the Negative Binomial tends to the Poisson: ",
                                      "r is set to ", dft_fmt_count(dft_nbinom_max_size), ".")
      )
    )
  })

  output$freq_summary <- renderPrint({
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    print(summary(x$counts))
    if (x$weighted) cat("\nTotal weight:", dft_fmt_count(sum(x$weights)), "\n")
  })

  output$selected_distribution_summary <- renderPrint({
    need_run("execute_freq_analysis", "claim counts")
    fit <- if (identical(input$FreqDistri, "NegativeBinomial")) freq_nb_fit() else freq_po_fit()
    validate(need(!is.null(fit), "This distribution could not be fitted to the data."))
    dft_print_count_fit(fit)
  })

  # observed share of observations in each bin of counts, against the probability of each fitted model
  output$count_hist <- chart(function() {
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    models <- count_cdfs()
    d <- dft_count_hist_data(x$counts, x$weights, or_default(input$count_hist_bins, 20), models$cdfs)
    dft_category_chart(
      d$labels,
      bars = list(name = "Observed", values = d$observed, colour = "bar", border = "bar_border"),
      lines = unname(Map(function(name, y, colour) list(name = name, values = y, colour = colour), names(d$fitted), d$fitted, models$colours)),
      x_title = "Number of claims", y_title = "Share of observations", dark = dark(), y_percent = TRUE
    )
  }, alt = "Histogram of the claim counts, with the probabilities of the fitted Poisson and Negative Binomial distributions")

  output$freq_fit_plot <- chart(function() {
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    models <- count_cdfs()
    d <- dft_count_cdf_data(x$counts, x$weights, models$cdfs)
    dft_line_chart(dft_cdf_series(d, models$colours, step = TRUE), "Number of claims", "Cumulative probability",
                   dark(), ylim = c(0, 1.02))
  }, alt = "Empirical and fitted cumulative distributions of the claim counts")

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
    sorted <- sort(x)
    ks <- vapply(models, function(m) if (is.null(m$cdf)) NA_real_ else ks_distance(sorted, m$cdf, sorted = TRUE), numeric(1))
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

  output$sev_hist <- chart(function() {
    need_run("execute_sev_analysis", "claim size")
    d <- dft_severity_hist_data(severity_data(), or_default(input$severity_hist_bins, 20))
    dft_histogram_chart(d$breaks, d$counts, "Claim size", "Number of claims", dark())
  }, alt = "Histogram of the claim sizes")

  output$sev_summary <- renderPrint({
    need_run("execute_sev_analysis", "claim size")
    x <- severity_data()
    print(summary(x))
    cat("\nPercentiles\n")
    print(quantile(x, c(0.75, 0.9, 0.95, 0.99, 0.995)))
  })

  output$sev_fit_plot <- chart(function() {
    need_run("execute_sev_analysis", "claim size")
    log_scale <- isTRUE(input$sev_fit_log_scale)
    models <- model_cdfs(sev_models())
    d <- dft_severity_cdf_data(severity_data(), models$cdfs, log_scale)
    dft_line_chart(dft_cdf_series(d, models$colours), "Claim size", "Cumulative probability", dark(),
                   x_log = log_scale, ylim = c(0, 1.02))
  }, alt = "Empirical and fitted cumulative distributions of the claim sizes")

  ######################
  #Sliced Sev analysis
  ######################

  sliced_input <- eventReactive(input$execute_sliced_sev_analysis, claim_sizes(input$sliced_sev_var))
  sliced_sev_data <- reactive(sliced_input()$claims)

  # the range of the sliders: from the smallest claim to the 99.5th percentile (dft_slider_top()),
  # widened to hold a typed point beyond it
  slicing_range <- reactiveVal(NULL)

  # the sliders cover the claims up to the 99.5th percentile, not the largest claim, which on a
  # heavy tail left the body of the distribution in less than a pixel of the bar; they start at
  # the 75th and 95th percentiles
  observeEvent(sliced_input(), {
    x <- sliced_sev_data()
    lowest <- dft_nice_bound(min(x))
    highest <- dft_slider_top(x)
    # (nearly every claim the smallest: the percentile is the smallest claim too)
    if (highest <= lowest) highest <- dft_nice_bound(max(x), up = TRUE)
    left <- signif(quantile(x, 0.75, names = FALSE), 3)
    right <- signif(quantile(x, 0.95, names = FALSE), 3)
    if (right <= left) right <- signif((left + highest) / 2, 3)
    # a fine step, so that claims below 1 can still be sliced precisely
    step <- signif((highest - lowest) / 1000, 1)
    slicing_range(c(lowest, highest))
    updateSliderInput(session, "slicing_point_left", min = lowest, max = highest, value = left, step = step)
    updateSliderInput(session, "slicing_point_right", min = lowest, max = highest, value = right, step = step)
  })

  # a slicing point can also be typed, in the box beside its slider: the slider follows the
  # typed value, and the range of both sliders grows to hold a value beyond it, so that the
  # point is fitted wherever it lies (a slider clamps a value beyond its range); the box
  # follows the slider. Neither update comes back: the browser sends a value only when it changes
  for (id in c("slicing_point_left", "slicing_point_right")) local({
    slider <- id
    typed <- paste0(id, "_typed")
    observeEvent(input[[slider]], updateNumericInput(session, typed, value = input[[slider]]))
    observeEvent(input[[typed]], {
      value <- input[[typed]]
      if (!isTRUE(is.finite(value)) || isTRUE(value == input[[slider]])) return()
      range <- slicing_range()
      if (!is.null(range) && (value < range[1] || value > range[2])) {
        range <- c(min(range[1], dft_nice_bound(value)), max(range[2], dft_nice_bound(value, up = TRUE)))
        slicing_range(range)
        for (other in setdiff(c("slicing_point_left", "slicing_point_right"), slider)) {
          updateSliderInput(session, other, min = range[1], max = range[2])
        }
      }
      updateSliderInput(session, slider, value = value, min = range[1], max = range[2])
    })
  })

  # moving the first point past the second moves the second above it
  observeEvent(input$slicing_point_left, {
    range <- req(slicing_range())
    if (isTRUE(input$slicing_point_right <= input$slicing_point_left)) {
      updateSliderInput(session, "slicing_point_right", value = signif((input$slicing_point_left + range[2]) / 2, 3))
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
      need(sum(x > x1 & x <= x2) >= 3, "Move the slicing points apart: at least three claims must lie between them."),
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
    # (the points are checked first: a message about them is not a failed fit)
    points <- slicing_points()
    tryCatch(
      fit_slice_pareto(sliced_sev_data(), points[1], points[2]),
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

  output$sliced_sev_cdf_plot <- chart(function() {
    need_run("execute_sliced_sev_analysis", "claim size")
    points <- slicing_points()
    log_scale <- isTRUE(input$sev_cens_fit_log_scale)
    whole <- unname(slc_sev_lnorm_fit()$estimate)
    cdfs <- sliced_cdfs()
    models <- list(LogNormal = function(q) plnorm(q, whole[1], whole[2]),
                   "LogNormal - Pareto" = cdfs$one, "LogNormal - Pareto - Pareto" = cdfs$two)
    d <- dft_severity_cdf_data(sliced_sev_data(), models, log_scale, extra_points = points)
    dft_line_chart(dft_cdf_series(d), "Claim size", "Cumulative probability", dark(),
                   x_log = log_scale, ylim = c(0, 1.02), vlines = points)
  }, alt = "Empirical and fitted cumulative distributions of the claim sizes, with the slicing points")

  output$mean_excess_func_plot <- chart(function() {
    need_run("execute_sliced_sev_analysis", "claim size")
    d <- dft_mean_excess_data(sliced_sev_data())
    # the largest claim has no mean excess, so two different claims leave a single point
    validate(need(length(d$x) >= 2, "The mean excess chart needs at least three different claim sizes."))
    points_ok <- tryCatch(slicing_points(), error = function(e) NULL)
    dft_line_chart(list(list(name = "Mean excess", x = d$x, y = d$y, colour = "bar")), "Threshold",
                   "Mean excess over the threshold", dark(), x_log = isTRUE(input$sev_cens_fit_log_scale), vlines = points_ok)
  }, alt = "Empirical mean excess function of the claim sizes, with the slicing points")

  ######################
  #piecewise pareto
  ######################

  piecewise_input <- eventReactive(input$execute_piecewise_sev_analysis, claim_sizes(input$piecewise_pareto_var))
  piecewise_sev_data <- reactive(sort(piecewise_input()$claims))

  # one slider per threshold, from the second smallest distinct claim to the 99.5th percentile
  # (dft_slider_top(); at least the third distinct claim, and at most the second largest); a
  # threshold already set is kept when it is still in range
  dynamic_sliders <- eventReactive(list(input$num_pareto_slices, input$execute_piecewise_sev_analysis), {
    sorted_claims <- piecewise_sev_data()
    distinct <- unique(sorted_claims)
    validate(need(length(distinct) >= 4, "The thresholds need at least four different claim sizes."))
    min_val <- distinct[2]
    max_val <- min(distinct[length(distinct) - 1], max(dft_slider_top(sorted_claims), distinct[3]))
    num_sliders <- or_default(input$num_pareto_slices, 1)
    step <- signif((max_val - min_val) / 1000, 1)
    tagList(lapply(seq_len(num_sliders), function(i) {
      id <- paste0("slider_", i)
      current <- isolate(input[[id]])
      # the 50th, 75th, 87.5th, ... percentiles: thresholds get closer together towards the tail
      default <- quantile(sorted_claims, 1 - 0.5^i, names = FALSE)
      value <- if (!is.null(current) && current >= min_val && current <= max_val) current else min(max(signif(default, 4), min_val), max_val)
      # without tick labels: at the width of the settings card, the labels of a range from a
      # small claim to a large one overlap
      sliderInput(id, label = paste("Threshold", i), min = min_val, max = max_val, value = value,
                  step = if (step > 0) step else NULL, ticks = FALSE)
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
                   note = paste("Each layer holds the claims from its threshold up to the next one; layer 1 starts at,",
                                "and includes, the smallest claim. Each alpha is the maximum likelihood estimate for the claims in its layer."))
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

  output$piecewise_pareto_cdf_plot <- chart(function() {
    need_run("execute_piecewise_sev_analysis", "claim size")
    piecewise_ready()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    log_scale <- isTRUE(input$piecewise_pareto_fit_log_scale)
    d <- dft_severity_cdf_data(piecewise_sev_data(), list("Piecewise Pareto" = function(q) piecewise_pareto_cdf(q, mu, alpha)),
                               log_scale, extra_points = mu)
    dft_line_chart(dft_cdf_series(d), "Claim size", "Cumulative probability", dark(),
                   x_log = log_scale, ylim = c(0, 1.02), vlines = mu)
  }, alt = "Empirical and fitted piecewise Pareto cumulative distributions of the claim sizes, with the thresholds")

}
