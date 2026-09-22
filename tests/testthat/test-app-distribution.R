#Browser test of the distribution fitting tool (helpers in helper-app.R)

dft_wait_for_plot <- function(app, id) {
  app$wait_for_js(sprintf("(function () { var img = document.querySelector('#%s img'); return !!img && img.naturalWidth > 0; })()", id),
                  timeout = 60 * 1000)
  image <- app_plot_image(app, id)
  expect_identical(app_png_bytes(image$src)[1:8], app_png_signature)
  #something is drawn, and the output is not an error message
  expect_gt(image$opaque + image$partial, 1000)
  expect_false(app$get_js(sprintf("document.getElementById('%s').classList.contains('shiny-output-error')", id)))
  image
}

test_that("the distribution tool fits a Negative Binomial to overdispersed counts and draws the charts", {
  skip_if_no_app_browser()
  csv <- app_claims_csv()
  counts <- utils::read.csv(csv)$claim_count
  #the maximum likelihood Negative Binomial: its mean is the sample mean, and its size
  #maximises the likelihood
  mu <- mean(counts)
  nll <- function(log_size) -sum(stats::dnbinom(counts, size = exp(log_size), mu = mu, log = TRUE))
  size <- exp(stats::optimize(nll, c(-5, 10), tol = 1e-10)$minimum)
  expect_gt(stats::var(counts) / mu, 1.2)

  app <- start_netsimr_app("distribution", "distribution-counts")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_upload(app, "file1", csv)
  app$wait_for_js("/2,000/.test((document.getElementById('data_overview') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)

  app_goto(app, "frequency")
  app_set(app, counts_var = "claim_count")
  app_click(app, "execute_freq_analysis")
  #(the tiles' labels are in capitals, which innerText keeps)
  app$wait_for_js("/suggested model/i.test((document.getElementById('freq_stats') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)
  app_idle(app)

  #the suggested model is the Negative Binomial, fitted with its size (not capped at 1e8)
  expect_match(tolower(app_text(app, "#freq_stats")), "suggested model negative binomial", fixed = TRUE)
  params <- app_text(app, "#selected_freq_params")
  shown <- regmatches(params, regexec("Negative Binomial[^0-9]*? r ([0-9][0-9,]*[.]?[0-9]*)", params))[[1]]
  expect_length(shown, 2)
  expect_equal(as.numeric(gsub(",", "", shown[2])), size, tolerance = 0.005)
  expect_no_match(params, "r is set to", fixed = TRUE)

  #the charts are drawn as images
  dft_wait_for_plot(app, "freq_fit_plot")
  app$set_inputs(freq_results_tabs = "Histogram", wait_ = FALSE)
  dft_wait_for_plot(app, "count_hist")

  expect_app_logs_clean(app)
})
