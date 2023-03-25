#' Produces a number of numerical plots based on the numerical demographic inputted
#' @param
#' @return
#' @examples

viewNumerical <- function(df, demParam, spiroParam, export=FALSE) {

  # takes the data frames with the categorical data and the value that you want to see
  plotData <- df %>%
    dpylr::select(demParam, spiroParam)

  # makes ggplot graph (mostly a scatterplot or a line graph)

  # export the graph

}

#' Produces numerical plots that are segregated based off of certain delinations of a
#' parameter
#'
#'
#' @param
#' @return
#' @examples
compareNumerical <- function() {

}
