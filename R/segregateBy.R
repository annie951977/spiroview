#' Segregates dataframes by a demographic delimiter
#'
#' A function that takes a demographic parameter and divides the dataframe based
#' off of the relationship to said parameter. For example, if demParam is sex
#' and segBy is Male, a data frame with only male samples and a data frame with
#' only female samples will be created.
#'
#' Assumptions:
#' - the provided parameters and delimiters exist in your dataset.
#' - the segBy deliminter exists in your dataset under the inputted demParam
#'
#'
#' @param df The dataframe containing the data you want to clean
#' @param demParam The demographic factor in question
#' @param segBy The demographic delimiter in question. For categorical variables,
#' simply some categorical term or a vector of terms is expected. For numerical variables,
#' a singlestring consisting of a numeric value preceded
#' by an inequality operator is expected. For example ">16".
#'
#' @param segIsNumeric Declaring if segBy is a numeric value. Default is FALSE.
#' @import stringr
#' @return a list of resulting dataset:
#' - dataset meeting segBy condition
#' - dataset that does not meet segBy condition


segregateBy <- function(df,
                        demParam,
                        segBy,
                        segIsNumeric = FALSE) {

  if(!is.data.frame(df) || !is.character(demParam) || !is.character(segBy)){
    stop("Missing necessary parameters in segregateBy function")
  }

  if(segIsNumeric == TRUE) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- str_locate(segBy, operators)

    # get the ending index
    opIndex <- opIndices[1,2]
    # get the numeric value
    op <- substring(segBy, 1, opIndex)
    numBy <- as.numeric(substring(segBy, opIndex + 1, length(segBy)))


    # filter for the param
    if (op == "<") {

      containsDF <- (df[which(df$demParam < numBy)])
      otherDF <- (df[which(df$demParam >= numBy)])


    } else if(op == "<=") {

      containsDF <- (df[which(df$demParam < numBy)])
      otherDF <- (df[which(df$demParam >= numBy)])

    } else if(op == ">") {

      containsDF <- (df[which(df$demParam > numBy)])
      otherDF <- (df[which(df$demParam <= numBy)])

    } else if(op == ">=") {

      containsDF <- (df[which(df$demParam >= numBy)])
      otherDF <- (df[which(df$demParam < numBy)])

    } else if(op == "==") {

      containsDF <- (df[which(df$demParam == numBy)])
      otherDF <- (df[which(df$demParam != numBy)])

    } else if(op == "!="){

      containsDF <- (df[which(df$demParam != numBy)])
      otherDF <- (df[which(df$demParam == numBy)])

    }
    return(list(
      contains = containsDF,
      other = otherDF
    ))

  } else {
    # filter based on param
    if (!is.vector(segBy)) {
      segBy = c(segBy)
    }
    containsDF <- (df[df$demParm %in% segBy])
    otherDF <- (df[!(df$demParam %in% segBy)])

    return(list(
      contains = containsDF,
      other = otherDF
    ))
  }
}

# [END]
