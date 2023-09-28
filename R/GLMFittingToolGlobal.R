#settings
options(shiny.maxRequestSize=30*1024^2)

#' A function to run the glm fitting tool application
#'
#' @return Opens the glm fitting tool application
#' @export
run_shiny_glm_fitting_tool = function(){
  shinyjs::useShinyjs()
  shinyApp(ui = GLMFittingToolUI, server = GLMFittingToolServer)
}
