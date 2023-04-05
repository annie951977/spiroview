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
#' @param title Title for the plot as a character string. Default is NULL
#' @return A basic ggplot scatterplot that can be built upon as needed
#' @examples
#' # Example 1: Basic numeric plot with viewNumerical
#' \dontrun{
#' plotData <- GLIData
#' plot <- viewNumerical(df=plotData,
#'                       demParam="height",
#'                       spiroParam="FEV1",
#'                       includeBestFit=FALSE)
#' }
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' https://ggplot2.tidyverse.org.
#' @export
#' @import ggplot2

viewNumerical <- function(df,
                          demParam,
                          spiroParam,
                          includeBestFit=FALSE,
                          title=NULL) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)) {
    stop("Please provide the proper parameters for viewNumerical")
  }

  # makes ggplot graph

  outputGraph <- ggplot(df, aes(x=.data[[demParam]], y=.data[[spiroParam]]))+ geom_point()

  if (includeBestFit) {
    outputGraph = outputGraph + geom_smooth(method = "lm")
  }

  if(!is.null(title)){
    if(!is.character(title)){
      warning("Inputted title is a not a character string, title not added")
    }
    outputGraph <- outputGraph + ggtitle(title)
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
#' @param secondParam An additional demographic variable of interest that can be
#'  either a numeric or categorical value.
#' @param secondDelim A delineation of secondParam. If it is a numerical variable
#'  it must follow the convention of "operator numeric" for example "<10".
#'  Default is NULL
#' @param secondParamIsNumeric States whether secondParam is a numeric or
#'  categorical variable
#' @param includeBestFit A boolean stating if a the line of best fit should be
#' included in the plot
#' @param title Title for the plot as a character string. Default is NULL
#' @return A basic ggplot scatterplot that can be built upon as needed
#' @examples
#' plotData <- GLIData
#' # Example 1: compareNumerical with one numeric variable
#' \dontrun{
#' plot <- compareNumerical(df=plotData,
#'                         demParam="height",
#'                         delim=">1.40",
#'                         spiroParam="FEV1",
#'                         includeBestFit=FALSE)
#' plot
#' }
#' # Example 2: compareNumerical with a categorical variable
#' \dontrun{
#' plot <- compareNumerical(df=plotData,
#'                         demParam="height",
#'                         delim=">1.40",
#'                         secondParam= "smoking",
#'                         secondDlim="1",
#'                         secondParamIsNumeric=FALSE,
#'                         spiroParam="FEV1",
#'                         includeBestFit=FALSE)
#' plot
#' }
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' https://ggplot2.tidyverse.org.
#'
#' Wickham H, François R, Henry L, Müller K, Vaughan D (2023).
#' dplyr: A Grammar of Data Manipulation. https://dplyr.tidyverse.org,
#' https://github.com/tidyverse/dplyr.
#'
#' Wickham H (2022). stringr: Simple, Consistent Wrappers for
#' Common String Operations. https://stringr.tidyverse.org,
#' https://github.com/tidyverse/stringr.
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @import stringr
compareNumerical <- function(df,
                             demParam,
                             spiroParam,
                             delim=NULL,
                             secondParam = NULL,
                             secondDelim= NULL,
                             secondParamIsNumeric=FALSE,
                             includeBestFit=FALSE,
                             title=NULL) {

  if (!is.data.frame(df) || !is.character(demParam) || !is.character(spiroParam)){
    stop("Please provide the proper parameters for compareNumerical")
  }

  if(!is.null(delim)){
    if(!(grepl("^[<>]{1}\\d", delim) || grepl("^[><=]{1}[=]{1}\\d", delim))){
      stop("delim parameter in compareNumerical is expected to be numeric")
    }
  }


  # makes ggplot graph (mostly a scatterplot)

  outputGraph <- ggplot(df, aes(x=.data[[demParam]], y=.data[[spiroParam]])) + geom_point()

  if(!is.null(delim)) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- as.data.frame(stringr::str_locate(delim, operators))

    # get the ending index
    opRow <- dplyr::filter(opIndices, .data$start == 1)
    opIndex <- opRow[,"end"]
    # get the numeric value
    op <- substring(delim, 1, opIndex)
    numBy <- as.numeric(substring(delim, opIndex + 1, nchar(delim)))

    # filter for the param
    if (op == "<") {
      highLight <- dplyr::filter(df, df[[demParam]] < numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  color=.data[[demParam]]))
    } else if(op == "<=") {
      highLight <- dplyr::filter(df, df[[demParam]] <= numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  color=.data[[demParam]]))
    } else if(op == ">") {
      highLight <- dplyr::filter(df, df[[demParam]] > numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  color=.data[[demParam]]))

    } else if(op == ">=") {
      highLight <- dplyr::filter(df, df[[demParam]] >= numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  color=.data[[demParam]]))

    } else if(op == "==") {
      highLight <- dplyr::filter(df, df[[demParam]] == numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  color=.data[[demParam]]))

    } else if(op == "!="){
      highLight <- dplyr::filter(df, df[[demParam]] != numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  color=.data[[demParam]]))
    }

  }

  # if there is a secondDelim but its category doesn't match secondParamIsNumeric
  if(!is.null(secondDelim)) {
    if((grepl("^[<>]{1}\\d", secondDelim) || grepl("^[><=]{1}[=]{1}\\d",
                                                   secondDelim)) &
                                                  !secondParamIsNumeric){
      stop("secondDelim parameter and secondParamIsNumeric
         parameter in compareNumerical does not match up")

    } else if(!(grepl("^[<>]{1}\\d", secondDelim) || grepl("^[><=]{1}[=]{1}\\d",
                                                           secondDelim)) &
                                                           secondParamIsNumeric){
      stop("secondDelim parameter and secondParamIsNumeric
         parameter in compareNumerical does not match up
         or invalid formatting of secondDelim")
    }
  }

  # defensive programming for second dem
  if(!is.null(secondParam) & !is.null(secondDelim) & !secondParamIsNumeric) {
    outputGraph <- outputGraph + geom_point(data=df,
                                            aes(x=.data[[demParam]],
                                                y=.data[[spiroParam]],
                                                shape=factor(.data[[secondParam]])))

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
      highLight <- dplyr::filter(df, df[[secondParam]] < numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  shape=25))

    } else if(op == "<=") {
      highLight <- dplyr::filter(df, df[[secondParam]] <= numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  shape=25))

    } else if(op == ">") {
      highLight <- dplyr::filter(df, df[[secondParam]] > numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]]),
                                                  shape=25)


    } else if(op == ">=") {
      highLight <- dplyr::filter(df, df[[secondParam]] >= numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  shape=25))

    } else if(op == "==") {
      highLight <- dplyr::filter(df, df[[secondParam]] == numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  shape=25))


    } else if(op == "!="){
      highLight <- dplyr::filter(df, df[[secondParam]] != numBy)
      outputGraph <- outputGraph + geom_point(data=highLight,
                                              aes(x=.data[[demParam]],
                                                  y=.data[[spiroParam]],
                                                  shape=25))
    }

  }

  if (includeBestFit) {
    outputGraph = outputGraph + geom_smooth(method = "lm")
  }

  if(!is.null(title)){
    if(!is.character(title)){
      warning("Inputted title is a not a character string, title not added")
    }
    outputGraph <- outputGraph + ggtitle(title)
  }

  return(outputGraph)
}

# [END]
