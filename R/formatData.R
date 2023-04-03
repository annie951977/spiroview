#' Reads a file in as a dataframe and formats the columns of commonly used demographic parameters
#'
#' A helper function that
#'
#' @param path path to the dataframe
#' @param heightLabel height label
#' @param genderLabel gender label
#' @param ageLabel age label
#' @param ethLabel ethnicity label
#' @return dataframe containing reformated data
#' @examples
#' # Example 1: Read in a file
#'
#'
#'
#' # Example 2: Read in a file with a different label
#' @export
#' @import readr

formatData <- function(path,
                       heightLabel=NULL,
                       genderLabel=NULL,
                       ageLabel=NULL,
                       ethLabel=NULL) {

  # check if path exists
  if(!file.exists(path)){
    stop("Path passed into formatData does not exist")
  }

  # check if the file is tsv or csv

  if(grepl("\\.csv$", path)) {
      df <- readr::read_csv2(path, col_names = TRUE)
    } else if(grepl("\\.tsv$", path)) {
     df <- readr::read_tsv(path, col_names = TRUE)
    } else {
    stop("Input file type not supported")
   }

  # for each parameter, grep for the thing in the colnames with a regex
  # then format using a rename

  # height, HT
  if (!is.null(heightLabel)){
    colnames(df)[colnames(df) == heightLabel] <- "height"

  } else if (grepl("HEIGHT | height | HT", colnames(df))) {
    colnames(df)[grep("HEIGHT | height | HT", colnames(df))] <- "height"
  } else {
    warning("Height data not present. Some features may not be usable")
  }

  # sex or gender, female or male

  ## Assumption: sex is either denoted as a binary (1, 2 for a MALE column) where 1 is male
  # or in descriptive terms ("female", "male")
  if(!is.null(genderLabel)) {
    colnames(df)[colnames(df) == genderLabel] <- "gender"

  } else if(grepl("m | M | male | MALE", colnames(df))) {
    # rename column to male
    colnames(df)[grepl("m | M | male | MALE", colnames(df))] <- "gender"
    df[which(df$gender) == "1"] <- "1"
    df[which(df$gender) == "0"] <- "2"
    colnames(df)
  } else if (grepl("f | F| female | FEMALE", colnames(df))) {
    # rename the column to male and flip the binary
    colnames(df)[grep("f | F| female | FEMALE", colnames(df))] <- "gender"
    df[which(df$gender) == "1"] <- "2"
    df[which(df$gender) == "0"] <- "1"
    colnames(df)
  } else if (grepl("sex | SEX | gender | GENDER", colnames(df))) {
    sex_options <- unique(df[,grep("sex | SEX | gender | GENDER", colnames(df))])
    if(grep("m | M | male | MALE", sex_options)){
      sex_options[which(grep("m | M | male | MALE", sex_options))] <- "1"
      sex_options[which(grep("f | F | female | FEMALE", sex_options))] <- "2"
      colnames(df)[grep("sex | SEX | gender | GENDER", colnames(df))] <- "gender"
    } else {
      warning("Sex data not recognized")
    }
  } else {
    warning("Sex data not found")
  }

  # age
  if (!is.null(ageLabel)){
    colnames(df)[colnames(df) == ageLabel] <- "age"

  } else if (grepl("age | AGE ", colnames(df))) {
    colnames(df)[grep("age | AGE ", colnames(df))] <- "age"
  } else {
    warning("Age data not found")
  }

  # ethnicity

  if (!is.null(ethLabel)){
    colnames(df)[colnames(df) == ethLabel] <- "ethnicity"

  } else if (grepl("ethnicity | ETHNICITY ", colnames(df))) {
    colnames(df)[grep("ethnicity | ETHNICITY", colnames(df))] <- "ethnicity"
  } else {
    warning("Ethnicity data not found")
  }

  return(df)

}

# [END]

