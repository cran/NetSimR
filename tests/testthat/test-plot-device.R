#plot device used by the charts of the Shiny tools

test_that("the cairo type is asked for only when shiny would use the Windows png() device", {
  args <- plot_device_args()
  expect_type(args, "list")
  uses_other_device <- nzchar(system.file(package = "ragg")) || nzchar(system.file(package = "Cairo")) ||
    isTRUE(capabilities("aqua"))
  if (.Platform$OS.type == "windows" && !uses_other_device && isTRUE(capabilities("cairo"))) {
    expect_identical(args, list(type = "cairo", antialias = "gray"))
  } else {
    expect_identical(args, list())
  }
  #switching shiny's preferred packages off cannot make the arguments invalid for png()
  old <- options(shiny.useragg = FALSE, shiny.usecairo = FALSE)
  on.exit(options(old), add = TRUE)
  expect_true(identical(plot_device_args(), list()) ||
                identical(plot_device_args(), list(type = "cairo", antialias = "gray")))
})

test_that("a direct png() call asks Windows for the cairo device, and the report's charts use it", {
  args <- png_device_args()
  if (.Platform$OS.type == "windows" && isTRUE(capabilities("cairo"))) {
    expect_identical(args, list(type = "cairo", antialias = "gray"))
  } else {
    expect_identical(args, list())
  }
  #the arguments open a png() device
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  do.call(grDevices::png, c(list(file, width = 200, height = 200), args))
  graphics::plot(1:10, main = "A test chart")
  grDevices::dev.off()
  expect_gt(file.size(file), 0)
  #the report draws its charts with them; the Windows default device would not anti-alias
  expect_true(any(grepl("png_device_args", deparse(body(write_simulation_report)), fixed = TRUE)))
  #shiny's renderPlot gets the same arguments where it would draw with png()
  old <- options(shiny.useragg = FALSE, shiny.usecairo = FALSE)
  on.exit(options(old), add = TRUE)
  if (!isTRUE(capabilities("aqua"))) expect_identical(plot_device_args(), args)
})

test_that("netsimr_render_plot draws the chart lazily, redraws on resize and passes its arguments on", {
  drawn <- 0
  server <- function(input, output, session) {
    output$chart <- netsimr_render_plot({
      drawn <<- drawn + 1
      graphics::plot(1:10, col = grDevices::adjustcolor("steelblue", 0.4), pch = 19)
    }, bg = "transparent", alt = "A test chart")
  }
  #the chart code must not run when the render function is created
  expect_equal(drawn, 0)
  shiny::testServer(server, {
    session$setInputs(.clientdata_output_chart_width = 400, .clientdata_output_chart_height = 300)
    img <- output$chart
    expect_match(img$src, "^data:image/png;base64,")
    expect_identical(img$alt, "A test chart")
    expect_gte(drawn, 1)
  })
})
