#' Provides summary statistics of spirometry data for a specific demographic value
#'
#' A function that provides a table of summary statistics for a specific
#' demographic value for the spirometry parameters inputted. This function
#' expects a dataframe
#'
#' Assumption: the delimiter is a value that exists within the the domain of
#' demParam.
#'
#'@param df A dataframe containing each patient demographic data and at least
#'  one mean spirometric value
#'@param demParam Demographic category of interest
#'@param delim A value to segregate the data by. If the delimiter is numeric,
#'that is, it contains a number, it must follow the convention of
#'an inequality operator followed by a number, for example "<10"
#'@param delimIsNumeric Boolean stating if the delimiter contains a numeric
#'numeric value
#'@param spiroParam Vector containing the spirometric parameter of interest
#'as a string
#'@return Returns a dataframe that contains summary statistics as columns and
#'spirometry parameters of interest as rows
#'@examples
#' # Example dataset
#' exampleDF <- data.frame(id=c(0000, 1111, 2222),
#'                         gender=c(1, 2, 1),
#'                         age=c(22, 75, 74),
#'                         height=c(1.52, 1.83, 1.96),
#'                         ethnicity=c(1, 2, 1),
#'                         FEV1=c(2.581, 2.2768045, 0.4895280 ),
#'                         FVC=c(2.924, 3.0208665, 0.6688253))
#'
#' # Example 1: Summary statistics for a categorical variable
#'
#' catResults <- summarizeAllByCategory(df=exampleDF,
#'                                      demParam="gender",
#'                                      delim="1",
#'                                      delimIsNumeric = FALSE,
#'                                      spiroParam="FEV1")
#' catResults
#'
#' # Example 2: Summary statistics for a numeric variable
#'
#' numResults <- summarizeAllByCategory(df=exampleDF,
#'                                      demParam="age",
#'                                      delim=">70",
#'                                      delimIsNumeric = TRUE,
#'                                      spiroParam="FEV1")
#' numResults
#'
#'@references
#'Wickham H, François R, Henry L, Müller K, Vaughan D (2023).
#'dplyr: A Grammar of Data Manipulation. https://dplyr.tidyverse.org,
#'https://github.com/tidyverse/dplyr.
#'
#'Wickham H (2022). stringr: Simple, Consistent Wrappers for
#'Common String Operations. https://stringr.tidyverse.org,
#'https://github.com/tidyverse/stringr.
#'
#'@export
#'@import stringr
#'@import dplyr


summarizeAllByCategory <- function(df,
                                   demParam,
                                   delim,
                                   delimIsNumeric=FALSE,
                                   spiroParam) {

  if(!is.data.frame(df) || !is.character(demParam) ||
     !is.character(delim) || !is.vector(spiroParam)){
    stop("Missing inputs for summarizeAllByCategory")
  }

  if((grepl("^[<>]{1}\\d", delim) || grepl("^[><=]{1}[=]{1}\\d", delim)) & !delimIsNumeric){
    stop("delim parameter and delimIsNumeric parameter in summarizeAllByCategory does not match up")

  } else if(!(grepl("^[<>]{1}\\d", delim) || grepl("^[><=]{1}[=]{1}\\d", delim)) & delimIsNumeric){
    stop("delim parameter and delimIsNumeric parameter in summarizeAllByCategory does not match up")
  }

  results <- data.frame()

  if(delimIsNumeric == TRUE) {

    # obtain the operator value

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

      sel <- df[df[demParam] < numBy,]

      # count
      count <- nrow(sel[spiroParam])

      # calculate mean
      spiroMean <- mean(sel[,spiroParam])

      # standard deviation
      spiroSD <- stats::sd(sel[,spiroParam])

      # min
      spiroMin <- min(sel[,spiroParam])

      # max
      spiroMax <- max(sel[,spiroParam])

      results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))

    } else if(op == "<=") {

      sel <- df[df[demParam] <= numBy,]

      # count
      count <- nrow(sel[spiroParam])

      # calculate mean
      spiroMean <- mean(sel[,spiroParam])

      # standard deviation
      spiroSD <- stats::sd(sel[,spiroParam])

      # min
      spiroMin <- min(sel[,spiroParam])

      # max
      spiroMax <- max(sel[,spiroParam])

      results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))

    } else if(op == ">") {

      sel <- df[df[demParam] > numBy,]

      # count
      count <- nrow(sel[spiroParam])

      # calculate mean
      spiroMean <- mean(sel[,spiroParam])

      # standard deviation
      spiroSD <- stats::sd(sel[,spiroParam])

      # min
      spiroMin <- min(sel[,spiroParam])

      # max
      spiroMax <- max(sel[,spiroParam])

      results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))

    } else if(op == ">=") {


      sel <- df[df[demParam] >= numBy,]

      # count
      count <- nrow(sel[spiroParam])

      # calculate mean
      spiroMean <- mean(sel[,spiroParam])

      # standard deviation
      spiroSD <- stats::sd(sel[,spiroParam])

      # min
      spiroMin <- min(sel[,spiroParam])

      # max
      spiroMax <- max(sel[,spiroParam])

      results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))


    } else if(op == "==") {

      sel <- df[df[demParam] == numBy,]

      # count
      count <- nrow(sel[spiroParam])

      # calculate mean
      spiroMean <- mean(sel[,spiroParam])

      # standard deviation
      spiroSD <- stats::sd(sel[,spiroParam])

      # min
      spiroMin <- min(sel[,spiroParam])

      # max
      spiroMax <- max(sel[,spiroParam])

      results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))

    } else if(op == "!="){


      sel <- df[df[demParam] != numBy,]

      # count
      count <- nrow(sel[spiroParam])

      # calculate mean
      spiroMean <- mean(sel[,spiroParam])

      # standard deviation
      spiroSD <- stats::sd(sel[,spiroParam])

      # min
      spiroMin <- min(sel[,spiroParam])

      # max
      spiroMax <- max(sel[,spiroParam])

      results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))
    }

  } else {

    sel <- df[df[demParam] == delim,]

    # count
    count <- nrow(sel[spiroParam])

    # calculate mean
    spiroMean <- mean(sel[,spiroParam])

    # standard deviation
    spiroSD <- stats::sd(sel[,spiroParam])

    # min
    spiroMin <- min(sel[,spiroParam])

    # max
    spiroMax <- max(sel[,spiroParam])

    results <- rbind(results, list(count, spiroMean, spiroSD, spiroMin, spiroMax))

  }

  colnames(results) <- c("Count", "Mean", "Standard Deviation", "Min", "Max")

  return(results)
}

# [END]

