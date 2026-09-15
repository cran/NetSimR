#the data behind the charts of the fitting tools, the chart helpers and the data preview

test_that("frequency histogram data bins the counts and gives each model's bin probabilities", {
  counts <- c(0, 1, 1, 2, 5, 9)
  weights <- c(1, 2, 1, 1, 3, 2)
  cdfs <- list(Poisson = function(q) ppois(q, 2))
  d <- dft_count_hist_data(counts, NULL, bins = 20, cdfs)
  expect_equal(d$labels, as.character(0:9))
  expect_equal(d$observed, as.numeric(table(factor(counts, levels = 0:9))) / 6)
  expect_equal(d$fitted$Poisson, dpois(0:9, 2))
  #fewer bins than counts: bins of two counts, labelled by their range; with weights, shares of the weight
  d <- dft_count_hist_data(counts, weights, bins = 5, cdfs)
  expect_equal(d$labels, c("0-1", "2-3", "4-5", "6-7", "8-9"))
  expect_equal(d$observed, c(4, 1, 3, 0, 2) / 10)
  expect_equal(d$fitted$Poisson, ppois(c(1, 3, 5, 7, 9), 2) - ppois(c(-1, 1, 3, 5, 7), 2))
  #large counts keep their thousands separators
  expect_equal(dft_count_hist_data(c(1000, 1500, 2999), bins = 2)$labels, c("1,000-1,999", "2,000-2,999"))
  #counts from 2^31 up are labelled in full (formatC(format = "d") gave "0-NA" with a warning)
  expect_no_warning(d <- dft_count_hist_data(c(0, 3e9, 5, 7), bins = 1))
  expect_equal(d$labels, "0-3,000,000,000")
  #the last bin ends at the largest count (it read "9-11"), and its probability with it
  d <- dft_count_hist_data(0:10, NULL, bins = 5, cdfs)
  expect_equal(d$labels, c("0-2", "3-5", "6-8", "9-10"))
  expect_equal(d$fitted$Poisson[4], ppois(10, 2) - ppois(8, 2))
  expect_equal(sum(d$fitted$Poisson), ppois(10, 2))
  expect_equal(dft_count_hist_data(0:9, NULL, bins = 4)$labels, c("0-2", "3-5", "6-8", "9"))
})

test_that("frequency cdf data is the weighted empirical cdf and the fitted cdfs at every count", {
  counts <- c(0, 1, 1, 3)
  weights <- c(1, 1, 2, 4)
  d <- dft_count_cdf_data(counts, weights, list(Poisson = function(q) ppois(q, 1)))
  expect_equal(d$x, 0:3)
  expect_equal(d$empirical, c(1, 4, 4, 8) / 8)
  expect_equal(d$fitted$Poisson, ppois(0:3, 1))
  #at most 5,000 points
  expect_lte(length(dft_count_cdf_data(c(0, 1e6))$x), 5000)
})

test_that("claim size chart data covers the claims and the slicing points", {
  set.seed(1)
  claims <- rlnorm(300, 7, 1.2)
  d <- dft_severity_cdf_data(claims, list(LogNormal = function(q) plnorm(q, 7, 1.2)), log_scale = TRUE, extra_points = c(2000, 5000))
  expect_equal(d$empirical, empirical_cdf_at(d$x, claims))
  expect_true(all(c(2000, 5000) %in% d$grid))
  expect_equal(range(d$grid), range(claims))
  expect_equal(d$fitted$LogNormal, plnorm(d$grid, 7, 1.2))
  #linear grid from zero
  expect_equal(dft_severity_cdf_data(claims)$grid[1], 0)
  h <- dft_severity_hist_data(claims, 20)
  expect_equal(sum(h$counts), 300)
  expect_true(min(h$breaks) <= min(claims) && max(h$breaks) >= max(claims))
  m <- dft_mean_excess_data(claims)
  expect_equal(m$y, mean_excess_at(m$x, claims))
  expect_false(max(claims) %in% m$x)
  #the empirical cdf is the first series, a step line; the fitted cdfs follow in their colours
  series <- dft_cdf_series(d, "#123456")
  expect_equal(vapply(series, function(s) s$name, ""), c("Empirical", "LogNormal"))
  expect_true(series[[1]]$step)
  expect_equal(series[[2]]$colour, "#123456")
  expect_identical(series[[2]]$x, d$grid)
})

test_that("chart axes have thousands separators and log ticks at round values", {
  expect_equal(dft_axis_labels(c(0, 2500, 1e6, 0.25, -1500)), c("0", "2,500", "1,000,000", "0.25", "-1,500"))
  expect_equal(dft_axis_labels(c(0, 0.25, 1), percent = TRUE), c("0%", "25%", "100%"))
  expect_equal(dft_axis_labels(c(1e12, 1e-7)), c("1e+12", "1e-07"))
  expect_equal(dft_log_ticks(c(50, 2e6)), 10^(2:6))
  expect_equal(dft_log_ticks(c(150, 3000)), c(200, 500, 1000, 2000))
  expect_equal(dft_zero_range(c(2, 5)), c(0, 5.4))
  expect_equal(dft_to_y2(c(0, 50, 100), c(0, 1), c(0, 100)), c(0, 0.5, 1))
  expect_equal(dft_short_label("a very long category label indeed", 10), paste0("a very lo", intToUtf8(8230)))
})

test_that("every chart draws in the light and dark themes, at card and phone widths", {
  set.seed(2)
  counts <- rnbinom(400, size = 1.2, mu = 5)
  claims <- rlnorm(500, 8, 1.3)
  cdfs <- list(Poisson = function(q) ppois(q, mean(counts)))
  hist_data <- dft_count_hist_data(counts, NULL, 20, cdfs)
  charts <- list(
    function(dark) dft_category_chart(hist_data$labels, bars = list(name = "Observed", values = hist_data$observed, colour = "bar", border = "bar_border"),
                                      lines = list(list(name = "Poisson", values = hist_data$fitted$Poisson, colour = dft_model_palette[1])),
                                      x_title = "Number of claims", y_title = "Share", dark = dark, y_percent = TRUE),
    function(dark) dft_line_chart(dft_cdf_series(dft_count_cdf_data(counts, NULL, cdfs), step = TRUE), "Number of claims", "Cumulative probability",
                                  dark, ylim = c(0, 1.02)),
    function(dark) dft_line_chart(dft_cdf_series(dft_severity_cdf_data(claims, list(LogNormal = function(q) plnorm(q, 8, 1.3)), TRUE, 5000)),
                                  "Claim size", "Cumulative probability", dark, x_log = TRUE, ylim = c(0, 1.02), vlines = 5000),
    function(dark) {
      h <- dft_severity_hist_data(claims, 50)
      dft_histogram_chart(h$breaks, h$counts, "Claim size", "Number of claims", dark)
    },
    function(dark) dft_category_chart(paste0("[", 1:30, ",", 2:31, ")"), bars = list(name = "Exposure (e)", values = 1:30, colour = "band_bar", y2 = "Exposure (e)"),
                                      lines = list(list(name = "Actual", values = sin(1:30) + 2, colour = "empirical")),
                                      x_title = "x", y_title = "y per unit of exposure", dark = dark)
  )
  for (width in c(800, 360)) {
    for (dark in c(FALSE, TRUE)) {
      for (draw in charts) {
        file <- tempfile(fileext = ".png")
        grDevices::png(file, width = width, height = 440, bg = "transparent")
        expect_no_error(draw(dark))
        grDevices::dev.off()
        expect_gt(file.size(file), 1000)
      }
    }
  }
})

test_that("the data preview shows the first rows in a scrolling table", {
  df <- data.frame(amount = c(1500.5, NA, seq_len(148)), region = c("North", "South", rep("East", 148)))
  html <- as.character(dft_data_preview(df, 100))
  expect_match(html, "Showing the first 100 of 150 rows.", fixed = TRUE)
  #a header row and 100 rows
  expect_equal(lengths(regmatches(html, gregexpr("<tr>", html, fixed = TRUE))), 101)
  expect_match(html, "dft-table-preview")
  expect_match(html, "1500.5", fixed = TRUE)
  #missing values are a dash, text columns are aligned left
  expect_match(html, paste0("<span class=\"dft-na\">", intToUtf8(8212)), fixed = TRUE)
  expect_match(html, "<th class=\"dft-left\">region</th>", fixed = TRUE)
  expect_match(html, "<th>amount</th>", fixed = TRUE)
  expect_match(as.character(dft_data_preview(df[1:3, ], 100)), "All 3 rows.", fixed = TRUE)
  #text and column names are escaped, missing text is a dash too
  expect_match(as.character(dft_data_preview(data.frame(x = "<b>"))), "&lt;b&gt;", fixed = TRUE)
  html <- as.character(dft_data_preview(data.frame(`<n>` = c("a<b & \"c\" 'd'", NA), check.names = FALSE)))
  expect_match(html, "<td class=\"dft-left\">a&lt;b &amp; &quot;c&quot; &#39;d&#39;</td>", fixed = TRUE)
  expect_match(html, "<th class=\"dft-left\">&lt;n&gt;</th>", fixed = TRUE)
  expect_match(html, paste0("<td class=\"dft-left\"><span class=\"dft-na\">", intToUtf8(8212), "</span></td>"), fixed = TRUE)
  expect_match(html, "All 2 rows.", fixed = TRUE)
  expect_match(as.character(dft_data_preview(df[1, ])), "1 row.", fixed = TRUE)
  #row numbers with thousands separators
  expect_match(as.character(dft_data_preview(data.frame(a = 1:1500), 1200)), "<td class=\"dft-left\">1,200</td>", fixed = TRUE)
})

test_that("the data preview of a wide file shows its first 50 columns, quickly", {
  set.seed(4)
  wide <- as.data.frame(matrix(round(runif(150 * 1000), 3), 150, 1000))
  #built as tags, 1,000 columns took 20 seconds, 5,000 columns five minutes
  elapsed <- system.time(html <- as.character(dft_data_preview(wide)))[["elapsed"]]
  expect_lt(elapsed, 5)
  #the # column and 50 columns, 100 rows
  expect_equal(lengths(regmatches(html, gregexpr("</th>", html, fixed = TRUE))), 51)
  expect_equal(lengths(regmatches(html, gregexpr("<tr>", html, fixed = TRUE))), 101)
  expect_match(html, "<th>V50</th>", fixed = TRUE)
  expect_false(grepl("<th>V51</th>", html, fixed = TRUE))
  expect_match(html, "Showing the first 100 of 150 rows and 50 of 1,000 columns.", fixed = TRUE)
  expect_match(as.character(dft_data_preview(wide[1:3, 1:60])), "All 3 rows and the first 50 of 60 columns.", fixed = TRUE)
})

test_that("with the bars on the right axis, the left axis follows the lines", {
  expect_equal(dft_line_range(c(999.3, 1003.5)), c(999.3 - 0.336, 1003.5 + 0.336))
  expect_equal(dft_line_range(c(-5, -4.8)), c(-5.016, -4.784))
  expect_equal(dft_line_range(c(2, 2)), c(1.984, 2.016))
  #zero is kept when it is near the values, or among them
  expect_equal(dft_line_range(c(0.2, 1.1)), dft_zero_range(c(0.2, 1.1)))
  expect_equal(dft_line_range(c(-1, 2)), dft_zero_range(c(-1, 2)))
  expect_equal(dft_line_range(c(-3, -1)), dft_zero_range(c(-3, -1)))
  expect_equal(dft_line_range(c(NA, Inf)), c(0, 1))
  left_axis <- function(bars, values) {
    file <- tempfile(fileext = ".png")
    grDevices::png(file, width = 600, height = 440)
    on.exit(grDevices::dev.off())
    dft_category_chart(c("a", "b"), bars = bars, lines = list(list(name = "Actual", values = values, colour = "empirical")),
                       x_title = "x", y_title = "y")
    graphics::par("usr")[3:4]
  }
  #a gaussian response around 1,000 drew as a flat line at the top of an axis from zero
  expect_equal(left_axis(list(name = "Rows", values = c(300, 300), colour = "band_bar", y2 = "Rows"), c(999.3, 1003.5)),
               dft_line_range(c(999.3, 1003.5)))
  #bars on the left axis still start at zero
  expect_equal(left_axis(list(name = "Observed", values = c(0.4, 0.6), colour = "bar"), c(0.5, 0.5))[1], 0)
})

test_that("GLM chart bands have readable labels and hold the rows cut() puts in them", {
  dash <- intToUtf8(8211)
  expect_equal(glm_band_labels(c(18.0006, 23.9723, 30.1769)), paste0(c("18", "24"), dash, c("24", "30")))
  #whole numbers: the values each band holds, the first band including its lower edge
  expect_equal(glm_band_labels(c(1, 25.75, 50.5, 75.25, 100), whole = TRUE),
               paste0(c("1", "26", "51", "76"), dash, c("25", "50", "75", "100")))
  expect_equal(glm_band_labels(c(0, 1500, 25000), whole = TRUE), paste0(c("0", "1,501"), dash, c("1,500", "25,000")))
  expect_equal(glm_band_labels(c(0, 1, 2, 5), whole = TRUE), c(paste0("0", dash, "1"), "2", paste0("3", dash, "5")))
  #" to " when a break is negative, where a dash would read as a minus sign
  expect_equal(glm_band_labels(c(-5, -2.5, 0, 2.5)), c("-5 to -2.5", "-2.5 to 0", "0 to 2.5"))
  #as many digits as keep the edges apart
  expect_equal(glm_band_labels(c(1, 1.01, 1.02)), paste0(c("1", "1.01"), dash, c("1.01", "1.02")))
  #labels that would repeat leave cut() its own
  expect_null(glm_band_labels(c(1, 1.5, 1.7), whole = TRUE))
  #equal widths from the smallest to the largest value (cut(breaks = n) moves the outer edges 0.1% out)
  expect_equal(glm_band_breaks(c(0, 10, NA), 5, "width"), seq(0, 10, by = 2))
  #quantiles that coincide give fewer bands
  expect_equal(glm_band_breaks(c(1, 1, 1, 2, 3, NA), 4), c(1, 2, 3))
  expect_equal(glm_value_labels(c(0.5, 1000, 25000)), c("0.5", "1,000", "25,000"))
  #every band holds the same rows as before, whatever the data
  set.seed(1)
  cases <- list(continuous = round(runif(3000, 18, 80), 4), whole = sample(18:80, 3000, TRUE),
                sum_insured = round(rlnorm(3000, 12, 1)), ties = c(rep(0, 1500), round(rexp(1500), 3)),
                tiny = runif(3000, 0, 1e-5), huge = rlnorm(3000, 38, 1), negative = rnorm(3000, -5, 3),
                few = sample(0:12, 3000, TRUE, prob = c(50, 20, 10, 5, 4, 3, 2, 2, 1, 1, 1, 0.5, 0.5)),
                missing = c(NA, round(runif(300, 0, 100), 2)))
  for (name in names(cases)) {
    x <- cases[[name]]
    values <- unique(x[!is.na(x)])
    for (method in c("quantile", "width")) {
      breaks <- glm_band_breaks(x, 10, method)
      labels <- glm_band_labels(breaks, all(values == round(values)))
      expect_false(is.null(labels), info = paste(name, method))
      new <- cut(x, breaks, labels = labels, include.lowest = TRUE, dig.lab = 6)
      old <- cut(x, if (method == "width") 10 else breaks, include.lowest = TRUE, dig.lab = 6)
      expect_identical(as.integer(new), as.integer(old), info = paste(name, method))
    }
  }
})
