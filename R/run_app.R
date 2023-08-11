#' run_app
#'
#' @description Runs the Data harmonization app
#'
#' @importFrom shiny incProgress
#'
#' @export
#'
#' @examples
#' run_app()
#'

run_app <- function() {
  shiny::runApp("inst/app")
}
