#' Produces a number of categorical plots based on the categorical demographic inputted
#' @param
#' @return
#' @examples
viewCategorical <- function(df,
                            demParam,
                            spiroParam,
                            type="box") {

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
                               delim,
                               type = "box") {

  # takes the data frames with the categorical data and the value that you want to see
  sel <- df %>%
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

viewCategoricalCounts <- function(df,
                                  demParam,
                                  type="pie") {

}


