#' Segregates dataframes by a demographic delimiter
#'
#' A function that takes a demographic parameter and divides the dataframe based
#' off of the relationship to said parameter. For example, if demParam is sex
#' and segBy is Male, a data frame with only male samples and a data frame with
#' only female samples will be created.
#'
#' Assumptions:
#' - the provided parameters and delimiters exist in your dataset.
#' - the segBy delimiter exists in your dataset under the inputted demParam
#'
#' @examples
#' exampleDF <- data.frame(id=c(0000, 1111, 2222),
#'                         gender=c(1, 2, 1),
#'                         height=c(1.52, 1.83, 1.96),
#'                         ethnicity=c(1, 2, 1),
#'                         FEV1=c(2.581, 2.2768045, 0.4895280 ),
#'                         FVC=c(2.924, 3.0208665, 0.6688253))
#'
#' # Example 1: Using a categorical delimiter
#' \dontrun{
#' results <- segregateBy(df=exampleDF,
#'             demParam = "gender",
#'             segBy = "1",
#'             segIsNumeric = FALSE)
#' }
#'
#' # Example 2: Using a numeric delimiter
#' \dontrun{
#' results <- segregateBy(df=exampleDF,
#'             demParam = "height",
#'             segBy = ">1.80",
#'             segIsNumeric = FALSE)
#' }
#'
#' @param df The dataframe containing the data you want to clean
#' @param demParam The demographic factor in question
#' @param segBy The demographic delimiter in question. For categorical variables,
#' simply some categorical term or a vector of terms is expected.
#' For numerical variables,a singlestring consisting of a numeric value preceded
#' by an inequality operator is expected. For example ">16".
#' @param segIsNumeric Declaring if segBy is a numeric value. Default is FALSE.
#' @return a list of resulting dataset:
#' - dataset meeting segBy condition
#' - dataset that does not meet segBy condition
#' @references
#' Wickham H (2022). _stringr:
#' Simple, Consistent Wrappers for Common String Operations_.
#' R package version 1.5.0, <https://CRAN.R-project.org/package=stringr>.
#'
#' @export
#' @import stringr
segregateBy <- function(df,
                        demParam,
                        segBy,
                        segIsNumeric = FALSE) {

  if(!is.data.frame(df) || !is.character(demParam) || !is.character(segBy)){
    stop("Missing necessary parameters in segregateBy function")
  }

  if((grepl("^[<>]{1}\\d", segBy) || grepl("^[><=]{1}[=]{1}\\d", segBy)) & !segIsNumeric){
    stop("segBy parameter and delimIsNumeric
         parameter in segregateBy does not match up")

  } else if(!(grepl("^[<>]{1}\\d", segBy) || grepl("^[><=]{1}[=]{1}\\d", segBy)) & segIsNumeric){
    stop("segBy parameter and delimIsNumeric
         parameter in segregateBy does not match up
         or invalid formatting of segBy")
  }


  if(segIsNumeric == TRUE) {
    # grep for inequality operator
    operators <- c("<", "<=", ">", ">=", "==", "!=")

    opIndices <- as.data.frame(stringr::str_locate(segBy, operators))

    # get the ending index
    opRow <- dplyr::filter(opIndices, .data$start == 1)
    opIndex <- opRow[,"end"]
    # get the numeric value
    op <- substring(segBy, 1, opIndex)
    numBy <- as.numeric(substring(segBy, opIndex + 1, nchar(segBy)))


    # filter for the param
    if (op == "<") {

      containsDF <- (df[which(sapply(df[demParam], as.numeric) < numBy),])
      otherDF <- (df[which(sapply(df[demParam], as.numeric) >= numBy),])


    } else if(op == "<=") {

      containsDF <- (df[which(sapply(df[demParam], as.numeric) < numBy),])
      otherDF <- (df[which(sapply(df[demParam], as.numeric) >= numBy),])

    } else if(op == ">") {

      containsDF <- (df[which(sapply(df[demParam], as.numeric) > numBy),])
      otherDF <- (df[which(sapply(df[demParam], as.numeric) <= numBy),])

    } else if(op == ">=") {

      containsDF <- (df[which(sapply(df[demParam], as.numeric) >= numBy,)])
      otherDF <- (df[which(sapply(df[demParam], as.numeric) < numBy),])

    } else if(op == "==") {

      containsDF <- (df[which(sapply(df[demParam], as.numeric) == numBy),])
      otherDF <- (df[which(sapply(df[demParam], as.numeric) != numBy),])

    } else if(op == "!="){

      containsDF <- (df[which(sapply(df[demParam], as.numeric) != numBy),])
      otherDF <- (df[which(sapply(df[demParam], as.numeric) == numBy),])

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
    containsDF <- (df[which(sapply(df[demParam], as.character) %in% segBy),])
    otherDF <- (df[which(!(sapply(df[demParam], as.character) %in% segBy)),])

    return(list(
      contains = containsDF,
      other = otherDF
    ))
  }
}

# [END]
