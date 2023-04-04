# Adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/
#
# Silva, A. (2022) TestingPackage: An Example R Package For BCB410H. Unpublished. URL https://github.com/anjalisilva/TestingPackage.

library(shiny)

ui <- fluidPage(

)

server <- function(input, output){

}

# Create Shiny app
shiny::shinyApp(ui = ui, server = server)

# [END]
