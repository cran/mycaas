#' run Assessment shinyApp
#'
#' This function run a shinyApp in which the is available:
#' iii) The GUI implementation of the adaptive assessment
#' @param file Logical value.
#' @return The results of the assessment
#' @export
#' @examples
#' # Try the test example yourself
#' data(AA_knowledge_test)
#' if(interactive()){
#' run_Assessment(AA_knowledge_test)}
run_Assessment <- function(file = NULL) {
  if(!methods::is(file,'assessment'))
  {
    stop("The variable 'file' need to be an 'assessment' type object." )
  }
  # Path to the ui.R and server.R files within the package
  ui_file <- system.file("Single", "ui.R", package = "mycaas")
  server_file <- system.file("Single", "server.R", package = "mycaas")

  # Create a local environment to source the files into
  local_env <- new.env()

  # Source the files into the local environment
  source(ui_file, local = local_env)
  source(server_file, local = local_env)

  # Retrieve the `ui` and `server` from the local environment
  ui <- local_env$ui
  server <- local_env$server

  # Run the Shiny app
  shiny::shinyApp(ui = ui, server = function(input, output) server(input, output, file))
}

