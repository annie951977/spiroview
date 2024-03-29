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
#' @param title Title for the plot as a character string. Default is NULL
#' @return A basic ggplot plot that can be built upon as needed
#' @examples
#' plotData <- GLIData
#' # Example 1: Basic categorical plot
#' \dontrun{
#' plot <- viewCategorical(df=plotdata,
#'                         demParam="ethnicity",
#'                         spiroParam="FEV1",
#'                         type="box")
#' }
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' https://ggplot2.tidyverse.org.
#' @export
#' @import ggplot2
viewCategorical <- function(df,
                            demParam,
                            spiroParam,
                            type="box",
                            title=NULL) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for compareNumerical")
  }

  graphTypes <- c("box", "strip", "violin")

  if (!(type %in% graphTypes)) {
    stop("Requested graph type not supported")
  }

  # takes the data frames with the categorical data and the value that you want to see

  outputGraph <- ggplot2::ggplot(df, ggplot2::aes(x=sapply(.data[[demParam]], as.character), y=.data[[spiroParam]])) +
    ggplot2::labs(y=spiroParam, x=demParam)

  # makes ggplot graph based on the type of distribution graph you're looking for

  if (type == "box") {
    outputGraph <- outputGraph + ggplot2::geom_boxplot()
  } else if (type == "violin") {
    outputGraph <- outputGraph + ggplot2::geom_violin()
  } else if (type == "strip") {
    outputGraph <- outputGraph + ggplot2::geom_jitter()
  }


  if (!is.null(title)) {
    if (!is.character(title)) {
      warning("Inputted title is a not a character string, title not added")
    }
    outputGraph <- outputGraph + ggplot2::ggtitle(title)
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
#' @param type The type of graph desired, options include: pie, bar.
#'             Default is pie
#' @param title Title for the plot as a character string. Default is NULL
#' @return an output graph
#' @examples
#' plotData <- GLIData
#' # Example 1: One categorical variable
#' \dontrun{
#' plot <- viewCategoricalCounts(df=plotData,
#'                         demParam="ethnicity"
#'                         type="pie")
#' }

#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' https://ggplot2.tidyverse.org.
#' @export
#' @import ggplot2

viewCategoricalCounts <- function(df,
                                  demParam,
                                  type="pie",
                                  title=NULL) {
  if (!is.data.frame(df) || !is.character(demParam)) {
    stop("Please provide the proper parameters for viewCategoricalCounts")
  }

  graphTypes <- c("pie", "bar")

  if (!(type %in% graphTypes)) {
    stop("Requested graph type not supported")
  }

  outputGraph <- ggplot2::ggplot(df, ggplot2::aes(x=sapply(.data[[demParam]], as.character))) +
    ggplot2::labs(x=demParam)

  if (type == "bar") {
    outputGraph <- outputGraph +
      ggplot2::geom_bar()

  } else if (type == "pie") {
    countDF <- data.frame()
    cat <- unique(df[[demParam]])
    for(i in seq_along(cat)) {
      total <- sum(df[[demParam]] == cat[i])
      countDF <- rbind(countDF, list(cat[i], total))
    }
    colnames(countDF) <-c("demParam", "n")
    outputGraph <- ggplot2::ggplot(countDF, ggplot2::aes(x="", y=n, fill=demParam)) +
      ggplot2::geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0)
  }

  if (!is.null(title)) {
    if (!is.character(title)) {
      warning("Inputted title is a not a character string, title not added")
    }
    outputGraph <- outputGraph + ggplot2::ggtitle(title)
  }

  return(outputGraph)

}

# [END]

