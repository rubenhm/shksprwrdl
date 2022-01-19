#' Run the graphical interface to the game in a web browser
#' @export
launch <- function() {
  appDir <- system.file("shiny", package = "shksprwordl")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",
                launch.browser = TRUE)
}
