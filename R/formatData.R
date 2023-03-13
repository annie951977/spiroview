#' Reads a file in as a dataframe and formats the columns of commonly used demographic parameters
#' @param
#' @return
#' @examples
#' @import readr

formatData <- function(path) {

  results <- data.frame()
  # check if the file is tsv or csv

 # if(grep("\.csv$", path)) {
 #   raw_df <- readr::read_csv2(path, col_names = TRUE)
  #} else if(grep("\.tsv$", path)) {
  #  raw_df <- readr::read_tsv(path, col_names = TRUE)
  #} else {

 # }

  # for each parameter, grep for the thing in the colnames with a regex
  # then format using a rename

  # weight, WT

  # height, HT

  # sex or gender, female or male

  # age

  # height

  # bmi



}
