#' run_app
#'
#' @description Runs the Data harmonization app
#'
#' @importFrom shiny runApp
#'
#' @export
#'
#'
run_app <- function() {

  appDir <- system.file("app", package = "DataHarmonization")
  shiny::runApp(appDir)
}
