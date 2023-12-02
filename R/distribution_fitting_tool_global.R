#settings
options(shiny.maxRequestSize=64*1024^3)

#' A function to run the glm fitting tool application
#'
#' @return Opens the glm fitting tool application
#' @export
run_shiny_distribution_fitting_tool = function(){
  shinyApp(ui = distribution_fitting_tool_UI, server = distribution_fitting_tool_Server)
}
