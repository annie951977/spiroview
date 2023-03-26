#' Produces a number of numerical plots based on the numerical demographic inputted
#' @param
#' @return
#' @examples
#' @imports ggplot
viewNumerical <- function(df,
                          demParam,
                          spiroParam,
                          includeBestFit=FALSE) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for viewNumerical")
  }

  # takes the data frames with the numerical data and the value that you want to see
  plotData <- df %>%
    dpylr::select(demParam, spiroParam)

  # makes ggplot graph

  outputGraph <- ggplot(plotData, aes(x=demParam, y=spiroParam)) + geom_point()

  if (includeBestFit) {
    outputGraph = outputGraph + geom_smooth(method = "lm")
  }

  return(outputGraph)
}

#' Produces numerical plot that is segregated based off of certain delinations of a
#' numerical or categorical parameter
#'
#'
#' @param
#' @return
#' @examples
#' @imports ggplot
compareNumerical <- function(df,
                             demParam,
                             spiroParam,
                             delim=NULL,
                             demColor= "red",
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
                                            color=demColor)
    } else if(op == "<=") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam <= numBy),
                                              color=demColor)
    } else if(op == ">") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam > numBy),
                                              color=demColor)

    } else if(op == ">=") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam >= numBy),
                                              color=demColor)

    } else if(op == "==") {
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam == numBy),
                                              color=demColor)

    } else if(op == "!="){
      outputGraph <- outputGraph + geom_point(df %>% filter(df$demParam != numBy),
                                              color=demColor)
    }

  }

  if (!is.null(secondDem) & is.character(secondDem)) {
    outputGraph <- outputGraph + geom_point(aes(shape = factor(df$secondDem)))
  }

  if(!is.null(secondDem) & !is.character(secondDem)) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- str_locate(delim, operators)

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
