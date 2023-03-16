#' Reads a file in as a dataframe and formats the columns of commonly used demographic parameters
#' @param path
#' @return dataframe containing reformated data
#' @examples
#' @import readr

formatData <- function(path) {

  results <- data.frame()
  # check if the file is tsv or csv

  if(grep("\.csv$", path)) {
      df <- readr::read.csv2(path, col_names = TRUE)
    } else if(grep("\.tsv$", path)) {
     df <- readr::read_tsv(path, col_names = TRUE)
    } else {
    stop("Input file type not supported")
   }

  # for each parameter, grep for the thing in the colnames with a regex
  # then format using a rename

  # height, HT
  if(grep("HEIGHT | height | HT", colnames(df))) {
    colnames(df)[grep("HEIGHT | height | HT", colnames(df))] <- "height"
  } else {
    warning("Height data not present. Some features may not be usable")
  }

  # sex or gender, female or male

  ## Assumption: sex is either denoted as a binary (1, 2 for a MALE column) or in descriptive terms ("female", "male")
  if(grep("m | M | male | MALE", colnames(df))) {
    # rename column to male
    colnames(df)[grep("m | M | male | MALE", colnames(df))] <- "gender"
    df[which(df$gender) == "1"] <- "1"
    df[which(df$gender) == "0"] <- "2"
    colnames(df)
  } else if (grep("f | F| female | FEMALE", colnames(df))) {
    # rename the column to male and flip the binary
    colnames(df)[grep("f | F| female | FEMALE", colnames(df))] <- "gender"
    df[which(df$gender) == "1"] <- "2"
    df[which(df$gender) == "0"] <- "1"
    colnames(df)
  } else if (grep("sex | SEX | gender | GENDER", colnames(df))) {
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
  if(grep("age | AGE ", colnames(df))) {
    colnames(df)[grep("age | AGE ", colnames(df))] <- "age"
  } else {
    warning("Age data not found")
  }

  # ethnicity

  if(grep("ethnicity | ETHNICITY ", colnames(df))) {
    colnames(df)[grep("ethnicity | ETHNICITY", colnames(df))] <- "ethnicity"
  } else {
    warning("Ethnicity data not found")
  }

}
