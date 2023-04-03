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
#' # Example 1: Basic numeric plot with viewNumerical
#' \dontrun{
#' plotData <- GLIData
#' plot <- viewNumerical(df=plotdata,
#'                       demParam="height",
#'                       spiroParam="FEV1",
#'                       includeBestFit=FALSE)
#' }
#' @import ggplot2

viewNumerical <- function(df,
                          demParam,
                          spiroParam,
                          includeBestFit=FALSE) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for viewNumerical")
  }

  # makes ggplot graph

  outputGraph <- ggplot(df, aes(x=demParam, y=spiroParam)) + geom_point()

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
#' @param demParam Demographic numeric category of interest
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
#' @param secondParamIsNumeric States whether secondParam is a numeric or
#'  categorical variable
#' @param secondColor The color to color the points meeting secondDelim
#'  on the graph. Default is "purple"
#' @param includeBestFit A boolean stating if a the line of best fit should be
#' included in the plot
#' @return A basic ggplot scatterplot that can be built upon as needed
#' @examples
#' plotData <- GLIData
#' # Example 1: compareNumerical with one numeric variable
#' \dontrun{
#' plot <- compareNumerical(df=plotdata,
#'                         demParam="height",
#'                         delim=">1.40",
#'                         spiroParam="FEV1",
#'                         includeBestFit=FALSE)
#' plot
#' }
#' # Example 2: compareNumerical with a categorical variable
#' \dontrun{
#' plot <- compareNumerical(df=plotdata,
#'                         demParam="height",
#'                         delim=">1.40",
#'                         secondParam= "smoking",
#'                         secondDlim="1",
#'                         secondParamIsNumeric=FALSE,
#'                         spiroParam="FEV1",
#'                         includeBestFit=FALSE)
#' plot
#' }
#' @import ggplot2
#' @import dplyr
compareNumerical <- function(df,
                             demParam,
                             spiroParam,
                             delim=NULL,
                             delimColor= "red",
                             secondParam = NULL,
                             secondDelim= NULL,
                             secondParamIsNumeric=FALSE,
                             secondColor= "purple",
                             includeBestFit=FALSE) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)){
    stop("Please provide the proper parameters for compareNumerical")
  }

  if(!is.null(delim)){
    if(!(grepl("^[<>]{1}\\d", delim) || grepl("^[><=]{1}[=]{1}\\d", delim))){
      stop("delim parameter in compareNumerical is expected to be numeric")
    }
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
  if (!is.null(secondParam) & !is.null(secondDelim) & !secondParamIsNumeric) {
    outputGraph <- outputGraph + geom_point(aes(shape = factor(df$secondParam)))
  }

  # if there is a secondDelim but its category doesn't match secondParamIsNumeric
  if(!is.null(secondDelim)) {
    if((grepl("^[<>]{1}\\d", secondDelim) || grepl("^[><=]{1}[=]{1}\\d", secondDelim)) & !secondParamIsNumeric){
      stop("secondDelim parameter and secondParamIsNumeric
         parameter in compareNumerical does not match up")

    } else if(!(grepl("^[<>]{1}\\d", secondDelim) || grepl("^[><=]{1}[=]{1}\\d", secondDelim)) & secondParamIsNumeric){
      stop("secondDelim parameter and secondParamIsNumeric
         parameter in compareNumerical does not match up
         or invalid formatting of secondDelim")
    }
  }

  if(!is.null(secondParam) & !is.null(secondDelim) & secondParamIsNumeric) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- as.data.frame(stringr::str_locate(secondDelim, operators))

    # get the ending index
    opRow <- dplyr::filter(opIndices, .data$start == 1)
    opIndex <- opRow[,"end"]
    # get the numeric value
    op <- substring(secondDelim, 1, opIndex)
    numBy <- as.numeric(substring(secondDelim, opIndex + 1, nchar(secondDelim)))

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

# [END]
