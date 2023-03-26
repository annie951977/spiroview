#' Produces a number of categorical plots based on the categorical demographic inputted
#' @param
#' @return
#' @examples
viewCategorical <- function(df,
                            demParam,
                            spiroParam,
                            type="box") {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for compareNumerical")
  }


  # takes the data frames with the categorical data and the value that you want to see
  plotData <- df %>%
    dpylr::select(demParam, spiroParam)

  outputGraph <- ggplot(plotData, aes(x=demParam, y=spiroParam))

  # makes ggplot graph based on the type of distribution graph you're looking for

  if (type == "box") {
    outputGraph <- outputGraph + geom_boxplot()
  } else if (type == "violin") {
    outputGraph <- outputGraph + geom_violin()
  } else if (type == "strip") {
    outputGraph <- outputGraph + geom_jitter()
  }

  return(outputGraph)
}



#' Produces a number of categorical plots based on the categorical demographic inputted and a certain delination
#' @param
#' @return
#' @examples

compareCategorical <- function(df,
                               demParam,
                               spiroParam,
                               delim = NULL,
                               type = "box") {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for compareCategorical")
  }


  # takes the data frames with the categorical data and the value that you want to see
  plotData <- df %>%
    dpylr::select(demParam, spiroParam)

  outputGraph <- ggplot(plotData, aes(x=demParam, y=spiroParam))

  # makes ggplot graph based on the type of distribution graph you're looking for

  if (type == "box") {
    outputGraph <- outputGraph + geom_boxplot()
  } else if (type == "violin") {
    outputGraph <- outputGraph + geom_violin()
  } else if (type == "strip") {
    outputGraph <- outputGraph + geom_jitter()
  }

  if (delim) {

  }

  return(outputGraph)

}

#' Produces graphs that show the number of samples per demographic category
#' @param df Data that is being analyzed
#' @param demParam A categorical demographic variable
#' @param secondParam A second categorical demographic variable
#' @param type The type of graph desired, options include: pie, doughnut, bar.
#'             Default is pie
#' @return an output graph
#' @examples

viewCategoricalCounts <- function(df,
                                  demParam,
                                  secondParam=NULL,
                                  type="pie") {
  if (!is.data.frame(df) || !is.character(demParam)) {
    stop("Please provide the proper parameters for viewCaategoricalCounts")
  }

  outputGraph <- ggplot(plotData)

  if(type == "bar") {
    outputGraph <- outputGraph + geom_bar(aes(x=demParam))

  } else if(type == "pie") {
    outputGraph <- outputGraph +
      geom_col(aes(x = 1, y = n, fill = demParam), position = "fill") +
      coord_polar("y", start=0)

  } else if(type == "doughnut") {
    outputGraph <- outputGraph +
      geom_bar(aes(x=demParam)) +
      coord_polar("y", start=0) + xlim(0, 1.5)
  }

  if(!is.null(secondParam)) {
    outGraph <- outputGraph + facet_wrap(~ secondParam)
  }

  return(outputGraph)

}


