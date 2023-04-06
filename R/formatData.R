#' Reads a file in as a dataframe and formats the columns of commonly used demographic parameters
#'
#' A helper function that reads in a csv or tsv and reformats it to be easily
#' used with the other functions in this package.
#'
#' Assumption: sex is either denoted as a binary (1, 2 for a MALE column)
#' where 1 is male or in descriptive terms ("female", "male")
#'
#' @param path The character path to the dataframe. Expects a csv or tsv file
#' @param heightLabel A character string that represents the height column name
#'  in the dataset. Default is NULL
#' @param genderLabel A character string that represents the gender column name
#'  in the dataset. Default is NULL
#' @param ageLabel A character string that represents the age column name
#'  in the dataset. Default is NULL
#' @param ethLabel A character string that represents the ethnicity column name
#'  in the dataset. Default is NULL
#' @return dataframe containing reformated data
#' @examples
#' \dontrun{
#' # Example 1: Read in a file
#' examplePath <- system.file("extdata",
#'                             "example_dataset_1.csv",
#'                             package="spiroview")
#' resultDB <- formatData(path=examplePath)
#'
#' resultDB
#' }
#'
#' # Example 2: Read in a file with a different label
#' \dontrun{
#' examplePath <- system.file("extdata",
#'                             "example_dataset_4.csv",
#'                             package="spiroview")
#' resultDB <- formatData(path=examplePath,
#'                        ageLabel= "A")
#' resultDB
#' }
#'
#' @export
#' @import readr
#' @import utils
#' @references
#' Wickham H, Hester J, Bryan J (2023). readr: Read Rectangular Text Data.
#' https://readr.tidyverse.org, https://github.com/tidyverse/readr.

formatData <- function(path,
                       heightLabel=NULL,
                       genderLabel=NULL,
                       ageLabel=NULL,
                       ethLabel=NULL) {

  # check if path exists
  if (!file.exists(path)) {
    stop("Path passed into formatData does not exist")
  }

  # check if the file is tsv or csv

  if (grepl("\\.csv$", path)) {
      df <- read.csv(path, header= TRUE)
    } else if (grepl("\\.tsv$", path)) {
     df <- as.data.frame(readr::read_tsv(path, col_names = TRUE))
    } else {
    stop("Input file type not supported")
   }

  # for each parameter, grep for the thing in the colnames with a regex
  # then format using a rename

  # height, HT
  if (!is.null(heightLabel)) {
    colnames(df)[colnames(df) == heightLabel] <- "height"

  } else if (any(grepl("HEIGHT|height|HT", colnames(df)))) {
    colnames(df)[grep("HEIGHT|height|HT", colnames(df))] <- "height"
  } else {
    warning("Height data not present. Some features may not be usable")
  }

  # sex or gender, female or male


  if (!is.null(genderLabel)) {
    colnames(df)[colnames(df) == genderLabel] <- "gender"

  } else if (any(grepl("sex|SEX|gender|GENDER", colnames(df)))) {

    # if all the values in gender are numeric values then we just need to rename,
    # suppress warnings because if the values aren't numeric, then we know this
    # check wasn't passed
    if (suppressWarnings(any(!is.na(sapply(df[[grep("sex|SEX|gender|GENDER",
                                              colnames(df))]], as.numeric))))) {
      colnames(df)[grep("sex|SEX|gender|GENDER", colnames(df))] <- "gender"
    } else {
      sex_options <- unique(df[,grep("sex|SEX|gender|GENDER", colnames(df))])
      colnames(df)[grep("sex|SEX|gender|GENDER", colnames(df))] <- "gender"
      if (any(grepl("^m|^M|^male|^MALE", sex_options))) {
        maleTag <- sex_options[grep("^m|^M|^male|^MALE", sex_options)][1]
        df$gender[which(!(df$gender == maleTag))] <- "2"
        df$gender[which(df$gender == maleTag)] <- "1"
      } else {
        warning("Sex data not recognized")
      }
    }

  } else if (any(grepl("^m|^M|^male|^MALE", colnames(df)))) {
    # rename column to male
    colnames(df)[grep("^m|^M|^male|^MALE", colnames(df))] <- "gender"
    df$gender[which(df$gender == "1")] <- "1"
    df$gender[which(df$gender == "0") ] <- "2"
  } else if (any(grepl("^f|^F|^female|^FEMALE", colnames(df)))) {
    # rename the column to male and flip the binary
    colnames(df)[grep("^f|^F|^female|^FEMALE", colnames(df))] <- "gender"
    df$gender[which(df$gender == "1")] <- "2"
    df$gender[which(df$gender == "0")] <- "1"
  } else {
    warning("Sex data not found")
  }

  # age
  if (!is.null(ageLabel)) {
    colnames(df)[colnames(df) == ageLabel] <- "age"

  } else if (any(grepl("age|AGE", colnames(df)))) {
    colnames(df)[grep("age|AGE", colnames(df))] <- "age"
  } else {
    warning("Age data not found")
  }

  # ethnicity

  if (!is.null(ethLabel)) {
    colnames(df)[colnames(df) == ethLabel] <- "ethnicity"

  } else if (any(grepl("ethnicity|ETHNICITY", colnames(df)))) {
    colnames(df)[grep("ethnicity|ETHNICITY", colnames(df))] <- "ethnicity"
  } else {
    warning("Ethnicity data not found")
  }
  return(df)

}

# [END]

