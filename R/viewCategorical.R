#' Produces a categorical plot based off of parameters given
#'
#' A function a basic ggplot plot visualizing the relationship between a
#' categorical variable to a spirometric value of interest.
#'
#' @param df Data that is being analyzed
#' @param demParam A categorical demographic variable
#' @param spiroParam The spirometric parameter of interest as a string
#' @param type The type of graph desired, options include: box, strip, violin.
#'             Default is box.
#' @return A basic ggplot plot that can be built upon as needed
#' @examples
#' @import ggplot2
#' @import dplyr

viewCategorical <- function(df,
                            demParam,
                            spiroParam,
                            type="box") {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for compareNumerical")
  }

  graphTypes <- c("box", "strip", "violin")

  if(!(type %in% graphTypes)) {
    stop("Requested graph type not supported")
  }

  # takes the data frames with the categorical data and the value that you want to see
  plotData <- df %>%
    dplyr::select(demParam, spiroParam)

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


#' Produces graphs that show the number of samples per demographic category.
#'
#' A function that produces graphs visualizing the number of samples per a
#' categorical demographic variable.
#'
#' @param df Data that is being analyzed
#' @param demParam A categorical demographic variable
#' @param secondParam A second categorical demographic variable to further
#' stratify the plot
#' @param type The type of graph desired, options include: pie, doughnut, bar.
#'             Default is pie
#' @return an output graph
#' @examples
#' @import ggplot2

viewCategoricalCounts <- function(df,
                                  demParam,
                                  secondParam=NULL,
                                  type="pie") {
  if (!is.data.frame(df) || !is.character(demParam)) {
    stop("Please provide the proper parameters for viewCategoricalCounts")
  }

  graphTypes <- c("pie", "bar", "doughnut")

  if(!(type %in% graphTypes)) {
    stop("Requested graph type not supported")
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


