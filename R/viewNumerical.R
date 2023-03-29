#' Produces a scatterplot visualizing a numeric variable to spirometric variable
#'
#' A function a basic ggplot scatter plot visualizing the relationship between a
#' numeric variable to a spirometric value of interest.
#'
#' @param df A dataframe containing each patient demographic data and at least
#'  one mean spirometric value
#' @param demParam Demographic category of interest
#' @param spiroParam The spirometric parameter of interest as a string
#' @param includeBestFit A boolean stating if a the line of best fit should be
#' included in the plot
#' @return A basic ggplot scatterplot that can be built upon as needed
#' @examples
#' @import ggplot2
#' @import dplyr
viewNumerical <- function(df,
                          demParam,
                          spiroParam,
                          includeBestFit=FALSE) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for viewNumerical")
  }

  # takes the data frames with the numerical data and the value that you want to see
  plotData <- df %>%
    dplyr::select(demParam, spiroParam)

  # makes ggplot graph

  outputGraph <- ggplot(plotData, aes(x=demParam, y=spiroParam)) + geom_point()

  if (includeBestFit) {
    outputGraph = outputGraph + geom_smooth(method = "lm")
  }

  return(outputGraph)
}

#' Produces scatterplot with values meeting certain thresholds marked
#'
#' A function that produces numerical plot that is segregated based off of certain
#' delineations of a numerical or categorical parameter. Up to two delineations
#' can be added.
#'
#' Assumption: delim exists within the domain of demParam and secondDelim
#' exists within the domain of secondParam
#'
#' @param df A dataframe containing each patient demographic data and at least
#'  one mean spirometric value
#' @param demParam Demographic category of interest
#' @param spiroParam The spirometric parameter of interest as a string
#' @param delim An delineation of demParam, must follow
#'  the convention of "operator numeric" for example "<10".
#'  Default is NULL
#' @param delimColor The color to color the points meeting delim on the graph.
#'  Default is "red"
#' @param secondParam An additional demographic variable of interest that can be
#'  either a numeric or categorical value.
#' @param secondDelim A delineation of secondParam. If it is a numerical variable
#'  it must follow the convention of "operator numeric" for example "<10".
#'  Default is NULL
#' @param secondColor The color to color the points meeting secondDelim
#'  on the graph. Default is "purple"
#' @param includeBestFit A boolean stating if a the line of best fit should be
#' included in the plot
#' @return A basic ggplot scatterplot that can be built upon as needed
#' @examples
#' @import ggplot2
#' @import dplyr
compareNumerical <- function(df,
                             demParam,
                             spiroParam,
                             delim=NULL,
                             delimColor= "red",
                             secondParm = NULL,
                             secondDelim= NULL,
                             secondColor= "purple",
                             includeBestFit=FALSE) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for compareNumerical")
  }

  # makes ggplot graph (mostly a scatterplot)

  outputGraph <- ggplot(df, aes(x=df$demParam, y=df$spiroParam)) + geom_point()

  if(!is.null(delim)) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- str_locate(delim, operators)

    # get the ending index
    opIndex <- opIndices[1,2]
    # get the numeric value
    op <- substring(delim, 1, opIndex)
    numBy <- as.numeric(substring(delim, opIndex + 1, length(delim)))

    # filter for the param
    if (op == "<") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam < numBy),
                                            color=delimColor)
    } else if(op == "<=") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam <= numBy),
                                              color=delimColor)
    } else if(op == ">") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam > numBy),
                                              color=delimColor)

    } else if(op == ">=") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam >= numBy),
                                              color=delimColor)

    } else if(op == "==") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam == numBy),
                                              color=delimColor)

    } else if(op == "!="){
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam != numBy),
                                              color=delimColor)
    }

  }


  # defensive programming for second dem

  if (!is.null(secondDem) & is.character(secondDem)) {
    outputGraph <- outputGraph + geom_point(aes(shape = factor(df$secondDem)))
  }

  if(!is.null(secondDem) & !is.character(secondDem)) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- str_locate(secondDelim, operators)

    # get the ending index
    opIndex <- opIndices[1,2]
    # get the numeric value
    op <- substring(delim, 1, opIndex)
    numBy <- as.numeric(substring(delim, opIndex + 1, length(secondDelim)))

    # filter for the param
    if (op == "<") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$secondParam < numBy),
                                              color=secondColor)

    } else if(op == "<=") {

      outputGraph <- outputGraph + geom_point(df %>% filter(df$secondParam <= numBy),
                                              color=secondColor)

    } else if(op == ">") {

      outputGraph <- outputGraph + geom_point(df %>% filter(df$secondParam > numBy),
                                              color=secondColor)
    } else if(op == ">=") {

      outputGraph <- outputGraph + geom_point(df %>% filter(df$secondParam >= numBy),
                                              color=secondColor)
    } else if(op == "==") {

      outputGraph <- outputGraph + geom_point(df %>% filter(df$secondParam == numBy),
                                              color=secondColor)

    } else if(op == "!="){

      outputGraph <- outputGraph + geom_point(df %>% filter(df$secondParam != numBy),
                                              color=secondColor)
    }

  }

  if (includeBestFit) {
    outputGraph = outputGraph + geom_smooth(method = "lm")
  }

  return(outputGraph)
}
