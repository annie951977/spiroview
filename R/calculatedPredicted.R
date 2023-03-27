#' Calculates the predicted lower limit of normal based off of inputted parameters
#'
#' Assumes that df is a dataframe row that contains at least the age and the height
#' of the patient
#' @param df A dataframe containing colnames age and height at the minimum
#' @return
#' @examples
#' @import rspiro
calculateLLNPret <- function(df,
                             param = "FEV1",
                             ref = "GLI") {
  # if the df has age, height, gender
  if(is.null(df$age) || is.null(df$height)){
    stop("Not enough demographic parameters to calculate mean predicted")
  }

  age <- df$age
  height <- df$height

  if(!is.null(df$gender)){
    gender <- df$gender
  } else {
    gender <- 1
  }

  if(!is.null(df$ethnicity)){
    ethnicity <- df$ethnicity
  } else {
    ethinicity <- 1
  }
  if (ref == "NHANESS3") {
    return(rspiro::LLN_NHANES3(age, height, gender, ethinicity, param))
  } else {
    return(rspiro::LLN_GLI(age, height, gender, param))
  }

}

#' Calculates the predicted lower limit of normal based off of inputted parameters
#' @param df A dataframe containing colnames age and height at the minimum
#' @return
#' @examples
#' @import rspiro
calculatePctPret <- function(df, param = "FEV1", ref = "GLI") {
  # if the df has age, height, gender
  if(is.null(df$age) || is.null(df$height)){
    stop("Not enough demographic parameters to calculate percent predicted")
  }

  age <- df$age
  height <- df$height

  if(!is.null(df$gender)){
    gender <- df$gender
  } else {
    gender <- 1
  }
  if (ref == "NHANESS3") {
    return(rspiro::pctpred_NHANES3(age, height, gender))
  } else {
    return(rspiro::LLN_GLI(age, height, gender, param))
  }
}

#' Calculates the predicted mean predicted based on inputed paramters
#' @param df A dataframe containing colnames age and height at the minimum
#' @return
#' @examples
#' @import rspiro
calculateMeanPret <- function(df,
                              param = "FEV1",
                              ref = "GLI") {
  # if the df has age, height, gender
  if(is.null(df$age) || is.null(df$height)){
    stop("Not enough demographic parameters to calculate mean predicted")
  }

  age <- df$age
  height <- df$height

  if(!is.null(df$gender)){
    gender <- df$gender
  } else {
    gender <- 1
  }
  if (ref == "NHANESS3") {
    return(rspiro::pred_NHANES3(age, height, gender))
  } else {
    return(rspiro::pred_GLI(age, height, gender, param))
  }
}

# [END]
