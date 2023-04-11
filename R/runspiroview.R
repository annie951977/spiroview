#' Launch Shiny App for spiroview
#'
#' A function that launches the Shiny app for spiroview.
#' The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#' spiroview::runspiroview()
#' }
#'
#' @references
#  Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/
#'
#' Silva, A. (2022) TestingPackage: An Example R Package For BCB410H.
#' Unpublished. URL https://github.com/anjalisilva/TestingPackage.
#'
#' @export
#' @importFrom shiny runApp

runspiroview <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "spiroview")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")
  return(actionShiny)
}

# [END]
