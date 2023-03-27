#' Provides summary statistics of spirometry data for a specific demographic value
#'
#' A function that provides a table of summary statistics for a specific
#' demographic value for the spirometry parameters inputted. This function
#' expects a dataframe
#'
#' Assumption: the delimiter is a value that exists within the the domain of
#' demParam
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
#'@example
#'@import stringr


summarizeAllByCategory <- function(df,
                                   demParam,
                                   delim,
                                   delimIsNumeric=FALSE,
                                   spiroParam) {

  if(!is.data.frame(df) || !is.character(demParam) ||
     !is.character(delim) || !is.vector(spiroParam)){
    stop("Missing inputs for summarizeAllByCategory")
  }

  if(grepl("\\d", delim) & !delimIsNumeric){
    stop("delim parameter and delimIsNumeric parameter in summarizeAllByCategory does not match up")

  } else if(!grepl("\\d", delim) & delimIsNumeric){
    stop("delim parameter and delimIsNumeric parameter in summarizeAllByCategory does not match up")
  }

  results <- data.frame()

  if(delimIsNumeric) {

    # obtain the <= value

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

      # filter for the specific parameter using dplyr

      # count

      # calculate mean

      # standard deviation

      # mode

      # min

      # max

    } else if(op == "<=") {

      # filter for the specific parameter using dplyr

      # count

      # calculate mean

      # standard deviation

      # mode

      # min

      # max

    } else if(op == ">") {

      # filter for the specific parameter using dplyr

      # count

      # calculate mean

      # standard deviation

      # mode

      # min

      # max

    } else if(op == ">=") {

      # filter for the specific parameter using dplyr

      # count

      # calculate mean

      # standard deviation

      # mode

      # min

      # max


    } else if(op == "==") {

      # filter for the specific parameter using dplyr

      # count

      # calculate mean

      # standard deviation

      # mode

      # min

      # max

    } else if(op == "!="){

      # filter for the specific parameter using dplyr

      # count

      # calculate mean

      # standard deviation

      # mode

      # min

      # max

    }

  } else {

    # filter for the specific parameter using dplyr

    # count

    # calculate mean

    # standard deviation

    # mode

    # min

    # max

  }

  return(results)

}

