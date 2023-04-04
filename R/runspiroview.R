#' Launch Shiny App for spiroview
#'
#'@return No return value but open up a Shiny page.
#'
#'@references
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/
#'
#' Silva, A. (2022) TestingPackage: An Example R Package For BCB410H.
#' Unpublished. URL https://github.com/anjalisilva/TestingPackage.
#'
#' @importFrom shiny runApp

runspiroview <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "spiroview")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")
  return(actionShiny)
}

# [END]
