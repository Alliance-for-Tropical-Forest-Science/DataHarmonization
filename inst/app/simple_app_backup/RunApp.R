#' RunApp
#'
#' @details This function launches the application located in inst/ directory
#'
#' @return Opens an interactive ShinnyApp window
#'
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples
#'\dontrun{
#' RunApp()
#'                }

RunApp <- function() {
  # appDir <- system.file("app", package = "DataHarmonization")
  appDir <- "inst/app"
  if (appDir == "") {
    stop("Could not find the Shiny app. Try re-installing the `DataHarmonization` package.", call. = FALSE)
  }

  runApp(appDir, display.mode = "normal")
}
