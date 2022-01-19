#' launch_app
#' Run the graphical interface to the game in a web browser
#' @export
launch_app <- function() {
  appDir <- system.file("shiny", "app.R", package = "shksprwrdl")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shksprwordl`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
