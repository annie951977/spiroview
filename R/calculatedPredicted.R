#' Calculates the predicted lower limit of normal based off of inputted parameters
#'
#' A function that calculates the predicted lower limit of normal (LLN) for an
#' inputted spirometric value and a dataframe.
#'
#' Assumptions:
#' - df is a dataframe row that contains at least the age and the height
#' of the patient
#' - gender is denoted as 1 or 2 for male or female respectively
#' - ethnicity is denoted as a value between 1 to 5
#'
#' @param df A dataframe containing colnames age and height at the minimum
#' @param param The spirometric metric of interest: "FEV1", "FVC", "FEV1FVC",
#' "PEF", "FEF2575", "FEV6", "FEV1FEV6. Default is "FEV1"
#' @param ref The reference equation to be used
#' @return A numeric vector or data.frame containing length(param) columns with
#'  rows equal to the number of rows in df
#' @examples
#' @import rspiro
calculateLLNPret <- function(df,
                             param = "FEV1",
                             ref = "GLI") {

  # check that there's a dataframe
  if(!is.data.frame(df)) {
    stop("Please input a dataframe into calculateLLNPret")
  }

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
    if (ethnicity > 5) {
      ethnicity <- 1
      warning("Ethnicity value not valid for spriometry equations, setting ethnicity to default of 1")
    }
  } else {
    ethinicity <- 1
  }
  if (ref == "NHANES3") {
    if (ethnicity > 3) {
      ethnicity <- 1
      warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
    }
    return(rspiro::LLN_NHANES3(age, height, gender, ethinicity, param))
  } else if (ref == "GLI"){

    return(rspiro::LLN_GLI(age, height, gender, param))
  } else {
    stop("Please select NHANES3 or GLI")
  }
}

#' Calculates the percent predicted based off of inputted parameters
#' Assumptions:
#' - df is a dataframe row that contains at least the age and the height
#' of the patient
#' - gender is denoted as 1 or 2 for male or female respectively
#' - ethnicity is denoted as a value between 1 to 5
#'
#' @param df A dataframe containing colnames age and height and a spirometric column
#' matching param at the minimum
#' @param param The spirometric metric of interest: "FEV1", "FVC", "FEV1FVC",
#' "PEF", "FEF2575", "FEV6", "FEV1FEV6. Default is "FEV1"
#' @param ref The reference equation to be used
#' @return A numeric vector or data.frame containing length(param) columns with
#'  rows equal to the number of rows in df
#' @examples
#' @import rspiro

calculatePctPret <- function(df, param = "FEV1", ref = "GLI") {
  # check that there's a dataframe
  if(!is.data.frame(df)) {
    stop("Please input a dataframe into calculatePctPret")
  }


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

  if(!is.null(df$ethnicity)){
    ethnicity <- df$ethnicity
    if (ethnicity > 5) {
      ethnicity <- 1
      warning("Ethnicity value not valid for spriometry equations, setting ethnicity to default of 1")
    }
  } else {
    ethinicity <- 1
  }

  if(param == "FEV1") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, FEV1=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, FEV1=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FVC") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, FVC=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, FVC=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEV1FVC") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, FEV1FVC=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, FEV1FVC=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "PEF") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, PEF=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, PEF=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEF2575") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, FEF2575=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, FEF2575=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEV6") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, FEV6=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, FEV6=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEV1FEV6") {

    if (ref == "NHANESS3") {
      if (ethnicity > 3) {
        ethnicity <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, FEV1FEV6=df$param))
    } else if (ref == "GLI") {
      return(rspiro::LLN_GLI(age, height, gender, ethnicity, FEV1FEV6=df$param))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else {
    stop("Please enter a valid spirometry metric")
  }




}

#' Calculates the predicted mean predicted based on inputted parameters.
#'
#' A function that takes a dataframe and calculates the predicted value of an
#'  inputted spirometric value.
#'
#' Assumptions:
#' - df is a dataframe row that contains at least the age and the height
#' of the patient
#' - gender is denoted as 1 or 2 for male or female respectively
#' - ethnicity is denoted as a value between 1 to 5
#'
#' @param df A dataframe containing colnames age and height at the minimum
#' @param param The spirometric metric of interest: "FEV1", "FVC", "FEV1FVC",
#'  "PEF", "FEF2575", "FEV6", "FEV1FEV6. Default is "FEV1"
#' @param ref The desired reference equation. Can be NHANES3 or GLI.
#' Default is GLI
#' @return A numeric vector or data.frame containing length(param) columns
#' @examples
#' @import rspiro
calculateMeanPret <- function(df,
                              param = "FEV1",
                              ref = "GLI") {
  # check that there's a dataframe
  if(!is.data.frame(df)) {
    stop("Please input a dataframe into calculateMeanPret")
  }

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
    if (ethnicity > 5) {
      ethnicity <- 1
      warning("Ethnicity value not valid for spriometry equations, setting ethnicity to default of 1")
    }
  } else {
    ethinicity <- 1
  }

  if (ref == "NHANES3") {
    if (ethnicity > 3) {
      ethnicity <- 1
      warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
    }
    return(rspiro::pred_NHANES3(age, height, gender, ethnicity, param))
  } else if (ref == "GLI") {
    return(rspiro::pred_GLI(age, height, gender, ethnicity, param))
  } else {
    stop("Please select NHANES3 or GLI")
  }
}

# [END]
