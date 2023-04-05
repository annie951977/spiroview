# Adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/
#
# Silva, A. (2022) TestingPackage: An Example R Package For BCB410H. Unpublished. URL https://github.com/anjalisilva/TestingPackage.

library(shiny)

ui <- navbarPage("spiroview",
                 tabPanel("About",
                          titlePanel("Welcome to spiroview"),
                          sidebarLayout(
                            sidebarPanel(
                              h2("Installation"),
                              p("spiroview is available on GitHub and can be installed on your R console with the following lines of code:"),
                              code('require("devtools")'),
                              br(),
                              code('devtools::install_github("annie951977/spiroview")'),
                              br(),
                              br(),
                              br(),
                            ),
                            mainPanel(
                              h1("spiroview"),
                              h3("spiroview: An R Package for the Exploration and Visualization of Demographic Spirometry Data"),
                              p("spiroview is an R package that provides the tools for visualizing and exploring demographic spirometry data without the need for an extensive code background."),
                              br(),
                              h2("Features"),
                              p("- Segregate datasets based off of demographic variable of interest"),
                              p("- Visualize correlation between demographc variables and spirometric measurements"),
                              p("- Calculate predicted spirometric values based off of inputted metrics"),
                              br(),
                              h2("To get started, use one of our example datasets or upload your file:"),
                              # input
                              br(),


                              h4("Use simulated databases"),
                              selectInput(inputId ="inputDatabase", label = h3("Pick database"),
                                          choices = list("GLI"= "GLI", "NHANES3" = "NHANES3", "Upload"= "Upload"),
                                          selected = "GLI"),


                              fileInput(inputId ="file1",
                                        label= "Select a file to explore. File should be in .csv or .tsv format and contains demographic and spirometric values",
                                        accept=c(".csv", ".tsv")),

                              actionButton(inputId = "selectDB",
                                           label = "select dataset"),

                              tableOutput("workingDB"),

                            )
                          )


                 ),tabPanel("Calculate",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Pick reference equation"),
                                radioButtons(inputId ="references", label = h3("Reference Equations"),
                                             choices = list("Global Lung Function Initiative (GLI)"= "GLI", "National Health and Nutrition Examination Survey III" = "NHANES3" ),
                                             selected = "GLI"),
                                br(),


                                selectInput(inputId = "calculateOptions", label = h3("Pick what you would like to calculate"),
                                            choices = list("Lower Limit of Normal" = "calculateLLNPret", "Percent Predicted" = "calculatePctPret", "Predicted Mean" = "calculateMeanPret"),
                                            selected = "calculateLLNPret"),

                                br(),
                                selectInput(inputId ="spirometryValues", label = h3("Pick the spirometry value you would like to calculate"),
                                            choices = list("FEV1" = "FEV1", "FVC" = "FVC", "FEV1FVC" = "FEV1FVC", "PEF" = "PEF", "FEF2575" = "FEF2575", "FEV6"= "FEV6", "FEV1FEV6"= "FEV1FEV6"),
                                            selected = "FEV1"),

                                br(),

                                selectInput(inputId ="addToDataset", label = h3("Add calculated result to your dataset"),
                                            choices = list("No" = FALSE, "Yes" = TRUE),
                                            selected = FALSE),

                                br(),
                                br(),

                                # calculate
                                actionButton(inputId = "calculateButton",
                                             label = "Calculate"),

                                br(),
                                br(),


                                h3("Download your calculated dataset"),
                                downloadButton("downloadCalcData", "Download")

                              ),
                              mainPanel(
                                h2("Please select a dataset in the About tab"),
                                tableOutput("calculatedTable"),

                              ),

                            )

                 ),
                 tabPanel("Segregate",
                          sidebarLayout(
                            sidebarPanel(

                              h2("Segregate dataset"),
                              textInput(inputId ="segDem", label = h3("Enter your demographic variable exactly how it appears in dataset"), value = "gender"),


                              textInput(inputId ="segBy", label = h3("Enter the value you want to filter your demographic variable by"), value = "1"),


                              br(),
                              selectInput(inputId ="segIsNumeric", label = h3("Are you filtering by a numeric value?"),
                                          choices = list("No" = FALSE, "Yes" = TRUE),
                                          selected = FALSE),

                              # calculate
                              actionButton(inputId = "segregateButton",
                                           label = "Segregate"),


                            ),

                            mainPanel(
                              h2("Please select a dataset in the About tab"),

                              tableOutput("containsTable"),

                              tableOutput("otherTable"),

                            )

                          )

                 ),
                 tabPanel("Summarize",
                          sidebarLayout(
                            sidebarPanel(

                              h2("Create summary statistics for a certain demographic variable value"),
                              textInput(inputId ="summaryDem", label = h3("Input a demographic variable. Enter your demographic variable exactly how it appears in dataset"), value = "gender"),


                              textInput(inputId ="summaryDelim", label = h3("Enter the value you want to filter your demographic variable by"), value = "1"),
                              selectInput(inputId ="summarySpiro", label = h3("Pick the spirometry value you would like to calculate"),
                                          choices = list("FEV1" = "FEV1", "FVC" = "FVC", "FEV1FVC" = "FEV1FVC", "PEF" = "PEF", "FEF2575" = "FEF2575", "FEV6"= "FEV6", "FEV1FEV6"= "FEV1FEV6"),
                                          selected = "FEV1"),

                              br(),
                              selectInput(inputId ="summaryDemIsNumeric", label = h3("Are you filtering by a numeric value?"),
                                          choices = list("No" = FALSE, "Yes" = TRUE),
                                          selected = FALSE),

                              # summarize
                              actionButton(inputId = "summarizeButton",
                                           label = "Summarize"),


                            ),

                            mainPanel(
                              h2("Please select a dataset in the About tab"),

                              tableOutput("summaryTable"),


                            )

                          )

                 ),tabPanel("Plot a numeric variable",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Plot numeric variable"),

                                textInput(inputId ="viewNumDem", label = h3("Input a demographic variable,Enter your demographic variable exactly how it appears in dataset"), value = "height"),

                                textInput(inputId ="viewNumSpiro", label = h3("Input a spirometric variable, Enter your spirometric variable exactly how it appears in dataset"), value = "FEV1"),

                                textInput(inputId ="viewNumTitle", label = h3("Input a title"), value = ""),
                                selectInput(inputId ="numIncludeBestFit", label = h3("Include a line of best fit?"),
                                            choices = list("No" = FALSE, "Yes" = TRUE),
                                            selected = FALSE),
                                actionButton(inputId = "viewNumericButton",
                                             label = "Plot"),

                                br(),
                                br(),
                                br(),

                                h2("Plot an annotated graph of a numeric variable"),

                                textInput(inputId ="compareNumDem", label = h3("Input a numeric demographic variable, Enter your demographic variable exactly how it appears in dataset"), value = "height"),
                                textInput(inputId ="compareNumDelim", label = h3("Input a filter for your numeric demographic variable. Enter the value you want to filter your demographic variable by"), value = ">1.60"),
                                textInput(inputId ="compareNumSpiro", label = h3("Input a spirometric variable. Enter your spirometric variable exactly how it appears in dataset"), value = "FEV1"),

                                textInput(inputId ="compareNumTitle", label = h3("Input a title"), value = ""),
                                selectInput("numCompareIncludeBestFit", label = h3("Include a line of best fit?"),
                                            choices = list("No" = FALSE, "Yes" = TRUE),
                                            selected = FALSE),

                                br(),


                                actionButton(inputId = "viewNumComparePlot",
                                             label = "Plot"),
                                br(),


                              ),

                              mainPanel(
                                h2("Please select a dataset in the About tab"),

                                plotOutput("viewNumericPlot"),
                                plotOutput("viewNumComparePlot"),
                              )
                            ),

                 ),tabPanel("Plot a categorical variable",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Plot categorical variable"),

                                textInput(inputId ="viewCatDem", label = h3("Input a demographic variable. Enter your demographic variable exactly how it appears in dataset"), value = "ethnicity"),

                                textInput(inputId ="viewCatSpiro", label = h3("Input a spirometric variable. Enter your spirometric variable exactly how it appears in dataset"), value = "FEV1"),

                                textInput(inputId ="viewCatTitle", label = h3("Input a title"), value = ""),
                                selectInput(inputId ="viewCatType", label = h3("Select a graph type"),
                                            choices = list("box" = "box", "strip" = "strip", "violin"= "violin"),
                                            selected = "box"),
                                actionButton(inputId = "viewCatButton",
                                             label = "Plot"),

                                br(),
                                br(),
                                br(),

                                h2("Plot categorical counts"),

                                textInput(inputId ="viewCatCountsDem", label = h3("Input a demographic variable. Enter your demographic variable exactly how it appears in dataset"), value = "gender"),

                                textInput(inputId ="viewCatCountsTitle", label = h3("Input a title"), value = ""),
                                selectInput(inputId ="viewCatCountsType", label = h3("Select a graph type"),
                                            choices = list("pie" = "pie", "bar" = "bar"),
                                            selected = "pie"),
                                actionButton(inputId = "viewCatCountsButton",
                                             label = "Plot"),

                                br(),


                              ),

                              mainPanel(
                                h2("Please select a dataset in the About tab"),

                                plotOutput("viewCatPlot"),
                                plotOutput("viewCatCountPlot"),

                              )
                            ),

                 ),

)


server <- function(input, output) {

  ## About page
  currentDB <- eventReactive(input$selectDB, {
    if(!is.null(input$file1)) {
      readInDatabase <- spiroview::formatData(path = input$file1$datapath)
    }
    switch(input$inputDatabase,
           "GLI" = spiroview::GLIData,
           "NHANES3"= spiroview::NHANES3Data,
           "Upload" = readInDatabase)

    if(input$inputDatabase == "GLI"){
      currentDB <- spiroview::GLIData
    } else if (input$inputDatabase == "NHANES3") {
      currentDB <- spiroview::NHANES3Data
    } else {
      if(!is.null(input$file1)) {
        currentDB <- spiroview::formatData(path = input$file1$datapath)
      }
    }
  })

  output$workingDB <- renderTable({
    currentDB()
  })


  ## Calculate Tab

  calculateTable <- eventReactive(input$calculateButton, {
    if(input$calculateOptions == "calculateLLNPret") {
      spiroview::calculateLLNPret(df=currentDB(),
                                  param =input$spirometryValues,
                                  ref=input$references)
    } else if(input$calculateOptions == "calculatePctPret") {
      spiroview::calculatePctPret(df=currentDB(),
                                  param =input$spirometryValues,
                                  ref=input$references)

    } else if(input$calculateOptions == "calculateMeanPret") {
      spiroview::calculateMeanPret(df=currentDB(),
                                  param =input$spirometryValues,
                                  ref=input$references)
    }
  })

  output$calculatedTable <- renderTable({
    calculateTable()
  })

  output$downloadCalcData <- downloadHandler(
    filename = function() {
      paste(input$inputDatabase, "_augmented.csv", sep = "")
    },
    content = function(file) {
      write.csv(cbind(currentDB(), calculateTable()), file, row.names = FALSE)
    }
  )

  # Segregate Tab


  containsResult<- eventReactive(input$segregateButton, {
    l <- spiroview::segregateBy(df=currentDB(),
                                demParam = input$segDem,
                                segBy = input$segBy,
                                segIsNumeric = input$segIsNumeric)
    return(l$contains)
  })

  otherResult <- eventReactive(input$segregateButton, {
    l <- spiroview::segregateBy(df=currentDB(),
                                demParam = input$segDem,
                                segBy = input$segBy,
                                segIsNumeric = input$segIsNumeric)
    return(l$other)
  })

  output$containsTable <- renderTable({
    containsResult()
  })

  output$otherTable <- renderTable({
    otherResult()
  })

  # Summarize Tab

  summaryResults <- eventReactive(input$summarizeButton, {
    results <- spiroview::summarizeAllByCategory(df=currentDB(),
                                                demParam = input$summaryDem,
                                                delim= input$summaryDelim,
                                                delimIsNumeric = input$summaryDemIsNumeric,
                                                spiroParam = input$summarySpiro)

    return(results)

  })

  output$summaryTable <- renderTable({
    summaryResults()
  })






  # Plot numeric tab

  observeEvent(input$viewNumericButton, {
    output$viewNumericPlot <- renderPlot({
      spiroview::viewNumerical(df=currentDB(),
                               demParam=input$viewNumDem,
                               spiroParam = input$viewNumSpiro,
                               includeBestFit = input$numIncludeBestFit,
                               title=input$viewNumTitle)
    })
  })

  observeEvent(input$viewNumComparePlot, {
    output$viewNumComparePlot <-renderPlot({
                spiroview::compareNumerical(df=currentDB(),
                                         demParam=input$compareNumDem,
                                         spiroParam=input$compareNumSpiro,
                                         delim=input$compareNumDelim,
                                         title=input$compareNumTitle,
                                         includeBestFit = input$numCompareIncludeBestFit)
    })
  })

  # Plot categorical tab

  observeEvent(input$viewCatButton, {
    output$viewCatPlot <- renderPlot({
      spiroview::viewCategorical(df=currentDB(),
                               demParam=input$viewCatDem,
                               spiroParam = input$viewCatSpiro,
                               type=input$viewCatType,
                               titl=input$viewCatTitle)
    })
  })

  observeEvent(input$viewCatCountsButton, {
    output$viewCatCountPlot <- renderPlot({
      spiroview::viewCategoricalCounts(df=currentDB(),
                                 demParam=input$viewCatCountsDem,
                                 type=input$viewCatCountsType,
                                 titl=input$viewCatCountsTitle)
    })
  })

}
# Create Shiny app
shiny::shinyApp(ui = ui, server = server)

# [END]
