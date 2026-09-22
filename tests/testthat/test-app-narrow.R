#Browser tests of the three tools at a phone's width (helpers in helper-app.R): the header
#fits, and no page scrolls sideways

expect_fits_narrow_window <- function(tool, tab, then = NULL) {
  skip_if_no_app_browser()
  app <- start_netsimr_app(tool, paste0(tool, "-narrow"), width = 360, height = 800)
  on.exit(app$stop(), add = TRUE)
  app_idle(app)

  #the navigation collapses behind the menu button, which is inside the window
  toggler <- app$get_js("(function () {
    var t = document.querySelector('.navbar-toggler, .navbar-toggle');
    if (!t || t.offsetParent === null) return {shown: false};
    var box = t.getBoundingClientRect();
    return {shown: true, left: box.left, right: box.right};
  })()")
  expect_true(toggler$shown)
  expect_gte(toggler$left, 0)
  expect_lte(toggler$right, 360)

  expect_no_horizontal_scroll(app, paste0("The welcome page of the ", tool))
  app_goto(app, tab)
  expect_no_horizontal_scroll(app, paste0("The ", tab, " tab of the ", tool))
  #further checks of the same app, e.g. of a tab that needs a run first
  if (!is.null(then)) then(app)
  expect_app_logs_clean(app)
}

#runs a small simulation and opens the Report tab: the report is shown in a frame that
#never scrolls sideways, so a report document wider than the frame (its two-column grids
#used to insist on 320 px tracks) had the right edge of every section cut off
expect_report_fits_frame <- function(app) {
  #an example first: a run with no distribution parameters produces nothing to report
  app_set(app, settingsIO_example = "Motor: excess of loss layer")
  app_click(app, "settingsIO_load_example")
  app_wait_for_notification(app, "Loaded example")
  app_wait_for_inputs(app, list(freqDistr = "Poisson", lamda = 5, mu = 9, sigma = 1.3),
                      "the values of the example")
  app_clear_notifications(app)
  app_set(app, numberOfSimulations = 1000)
  app_click(app, "RunSimulations")
  app$wait_for_js("document.getElementById('RunSimulations').disabled === false", timeout = 120 * 1000)
  app_idle(app)
  app_goto(app, "report")
  #the report, or the message that says why there is none, so a failure here is legible
  app$wait_for_js("(function () {
    var frame = document.querySelector('iframe.sim-report-frame');
    if (frame && frame.contentDocument && frame.contentDocument.body &&
        frame.contentDocument.body.innerText.length > 200) return true;
    var view = document.getElementById('report-report_view');
    return !!(view && /could not|No report yet/.test(view.innerText));
  })()", timeout = 120 * 1000)
  app_idle(app)
  expect_true(app$get_js("!!document.querySelector('iframe.sim-report-frame')"),
              info = app$get_js("(document.getElementById('report-report_view') || {}).innerText || ''"))
  size <- app$get_js("(function () {
    var frame = document.querySelector('iframe.sim-report-frame');
    var doc = frame.contentDocument;
    var wide = Array.prototype.filter.call(doc.querySelectorAll('section, header, nav'), function (e) {
      return e.getBoundingClientRect().right > frame.clientWidth + 0.5;
    });
    return {frame: frame.clientWidth, document: doc.documentElement.scrollWidth,
            wide: Array.prototype.map.call(wide, function (e) { return e.id || e.tagName; })};
  })()")
  expect_gt(size$frame, 200)
  testthat::expect(
    size$document <= size$frame && length(size$wide) == 0,
    sprintf("The report is %s px wide in a frame of %s px; these parts stick out: %s",
            size$document, size$frame, paste(unlist(size$wide), collapse = ", "))
  )
  expect_no_horizontal_scroll(app, "The report tab of the simulator")
}

test_that("the simulator and its report fit a 360 px window", {
  expect_fits_narrow_window("simulator", "simulator", then = expect_report_fits_frame)
})

test_that("the distribution fitting tool fits a 360 px window", {
  expect_fits_narrow_window("distribution", "data")
})

test_that("the GLM fitting tool fits a 360 px window", {
  expect_fits_narrow_window("glm", "model")
})
