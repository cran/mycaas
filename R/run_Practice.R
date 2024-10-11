#' run Practice Assessment shinyApp
#'
#' This function run a shinyApp in which the following pages are available:
#' i) a brief introduction of the assessment tools; ii) the GUI to create "assessment" files from a "csv" of items;
#' iii) the GUI implementation of the adaptive assessment; and iv) the GUI implementation of the outcome.
#' @return The results of the assessment
#' @export
#' @examples
#' # Try to build your test
#' if(interactive()){
#' run_Practice()}
run_Practice <- function() {
  if(system.file(package='shiny')=="")
  {
    stop("The shiny package is required to run this function.")
  }
  appDir <- system.file("App","App.R", package = "mycaas")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mycaas`.", call. = FALSE)
  }
  shiny::runApp(appDir = appDir)
}
