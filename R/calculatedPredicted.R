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
#'
#' # Example 1: Calculate LLN of FEV1 from the GLI equations
#'  example_df <- GLIData
#'  output <- calculateLLNPret(
#'                df=example_df,
#'                param="FEV1",
#'                ref="GLI")
#'  output
#'
#' # Example 2: Calculate LLN of FEV1 from NHANES3
#'  example_df <- NHANES3Data
#'
#'  output <- calculateLLNPret(
#'                 df=example_df,
#'                 param="FEV1",
#'                 ref="NHANES3")
#' output
#'
#' @references
#' Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
#' Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel, G.,
#' Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).
#' The Global Lung Function Initiative (GLI) Network: bringing the world's
#' respiratory reference values together. Breathe (Sheffield, England), 13(3),
#' e56–e64. https://doi.org/10.1183/20734735.012717

#' Hankinson, J. L., Odencrantz, J. R., & Fedan, K. B. (1999).
#' Spirometric reference values from a sample of the general U.S. population.
#' American journal of respiratory and critical care medicine, 159(1), 179–187.
#' https://doi.org/10.1164/ajrccm.159.1.9712108
#'
#' Lytras T (2020). _rspiro: Implementation of Spirometry Equations_.
#' R package version 0.2, <https://CRAN.R-project.org/package=rspiro>.
#' @export
#' @import rspiro
calculateLLNPret <- function(df,
                             param = "FEV1",
                             ref = "GLI") {

  # check that there's a dataframe
  if(!is.data.frame(df)) {
    stop("Please input a dataframe into calculateLLNPret")
  }

  spiroOptions <- c("FEV1", "FVC", "FEV1FVC", "PEF", "FEF2575", "FEV6", "FEV1FEV6")

  if(!(param %in% spiroOptions)){
    stop("Please enter a valid spirometry metric in calculatePctPret")
  }

  # if the df has age, height, gender
  if(is.null(df$age) || is.null(df$height)){
    stop("Not enough demographic parameters to calculate mean predicted")
  }

  age <- sapply(df$age, as.numeric)
  height <- sapply(df$height, as.numeric)

  if(!is.null(df$gender)){
    gender <- sapply(df$gender, as.numeric)
  } else {
    gender <- rep(1, length(df$age))
  }

  if(!is.null(df$ethnicity)){
    ethnicity <- sapply(df$ethnicity, as.numeric)
    if (any(ethnicity>5)) {
      ethnicity[which(ethnicity>5)] <- 1
      warning("Ethnicity value not valid for spriometry equations, setting some ethnicity values to default of 1")
    }
  } else {
    ethinicity <- rep(1, length(df$age))
  }
  if (ref == "NHANES3") {
    ethnicity <- sapply(df$ethnicity, as.numeric)
    if (any(ethnicity>3)) {
      ethnicity[which(ethnicity>3)] <- 1
      warning("Ethnicity value not valid for NHANES3 equations, setting some ethnicity value to default of 1")
    }
    return(rspiro::LLN_NHANES3(age=age,
                               height=height,
                               gender=gender,
                               ethnicity=ethnicity,
                               param=param))
  } else if (ref == "GLI"){

    return(rspiro::LLN_GLI(age=age,
                           height=height,
                           gender=gender,
                           ethnicity=ethnicity,
                           param=param))
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
#' "PEF", "FEF2575", "FEV6", "FEV1FEV6".
#' @param ref The reference equation to be used
#' @return A numeric vector or data.frame containing length(param) columns with
#'  rows equal to the number of rows in df
#' @examples
#'
#' # Example 1: Calculate LLN of FEV1 from the GLI equations
#'  example_df <- GLIData
#'  output <- calculatePctPret(
#'                df=example_df,
#'                param="FEV1",
#'                ref="GLI")
#'  output
#'
#' # Example 2: Calculate LLN of FEV1 from NHANES3
#'  example_df <- NHANES3Data
#'
#'  output <- calculatePctPret(
#'                 df=example_df,
#'                 param="FEV1",
#'                 ref="NHANES3")
#' output
#'
#' @references
#' Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
#' Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel, G.,
#' Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).
#' The Global Lung Function Initiative (GLI) Network: bringing the world's
#' respiratory reference values together. Breathe (Sheffield, England), 13(3),
#' e56–e64. https://doi.org/10.1183/20734735.012717

#' Hankinson, J. L., Odencrantz, J. R., & Fedan, K. B. (1999).
#' Spirometric reference values from a sample of the general U.S. population.
#' American journal of respiratory and critical care medicine, 159(1), 179–187.
#' https://doi.org/10.1164/ajrccm.159.1.9712108
#'
#' Lytras T (2020). _rspiro: Implementation of Spirometry Equations_.
#' R package version 0.2, <https://CRAN.R-project.org/package=rspiro>.
#' @export
#' @import rspiro
calculatePctPret <- function(df,
                             param = "FEV1",
                             ref = "GLI") {
  # check that there's a dataframe
  if(!is.data.frame(df)) {
    stop("Please input a dataframe into calculatePctPret")
  }

  spiroOptions <- c("FEV1", "FVC", "FEV1FVC", "PEF", "FEF2575", "FEV6", "FEV1FEV6")

  if(!(param %in% spiroOptions)){
    stop("Please enter a valid spirometry metric in calculatePctPret")
  }

  if(is.null(df[[param]])){
    stop("Dataframe must contain spirometry parameter in calculatePctPret")
  }

  # if the df has age, height, gender
  if(is.null(df$age) || is.null(df$height)){
    stop("Not enough demographic parameters to calculate percent predicted")
  }

  age <- as.numeric(df$age)
  height <- as.numeric(df$height)

  if(!is.null(df$gender)){
    gender <- sapply(df$gender, as.numeric)
  } else {
    gender <- rep(1, length(df$age))
  }

  if(!is.null(df$ethnicity)){
    ethnicity <- as.numeric(df$ethnicity)
    if (any(ethnicity>5)) {
      ethnicity[which(ethnicity>5)] <- 1
      warning("Ethnicity value not valid for spriometry equations, setting some
              ethnicity values to default of 1")
    }
  } else {
    ethinicity <- rep(1, length(df$age))
  }

  if(param == "FEV1") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations,
                setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age=age,
                                     height=height,
                                     gender=gender,
                                     ethnicity=ethnicity,
                                     FEV1=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_GLI(age=age,
                                 height=height,
                                 gender=gender,
                                 ethnicity=ethnicity,
                                 FEV1=df[[param]]))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FVC") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations,
                setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age=age,
                                     height=height,
                                     gender=gender,
                                     ethnicity=ethnicity,
                                     FVC=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_GLI(age=age,
                                 height=height,
                                 gender=gender,
                                 ethnicity=ethnicity,
                                 FVC=df[[param]]))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEV1FVC") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations,
                setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age=age,
                                     height=height,
                                     gender=gender,
                                     ethnicity=ethnicity,
                                     FEV1FVC=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_GLI(age=age,
                                 height=height,
                                 gender=gender,
                                 ethnicity=ethnicity,
                                 FEV1FVC=df[[param]]))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "PEF") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age, height, gender, ethnicity, PEF=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_GLI(age=age,
                                 height=height,
                                 gender=gender,
                                 ethnicity=ethnicity,
                                 PEF=df[[param]]))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEF2575") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age=age,
                                     height=height,
                                     gender=gender,
                                     ethnicity=ethnicity,
                                     FEF2575=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_GLI(age=age,
                                 height=height,
                                 gender=gender,
                                 ethnicity=ethnicity,
                                 FEF2575=df[[param]]))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEV6") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age=age,
                                     height=height,
                                     gender=gender,
                                     ethnicity=ethnicity,
                                     FEV6=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_GLI(age=age,
                                 height=height,
                                 gender=gender,
                                 ethnicity=ethnicity,
                                 FEV6=df[[param]]))
    } else {
      stop("Please select NHANES3 or GLI")
    }

  } else if(param == "FEV1FEV6") {

    if (ref == "NHANES3") {
      if (any(ethnicity>3)) {
        ethnicity[which(ethnicity>3)] <- 1
        warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
      }
      return(rspiro::pctpred_NHANES3(age=age,
                                     height=height,
                                     gender=gender,
                                     ethnicity=ethnicity,
                                     FEV1FEV6=df[[param]]))
    } else if (ref == "GLI") {
      return(rspiro::pctpred_NHANES3(age=age,
                              height=height,
                              gender=gender,
                              ethnicity=ethnicity,
                              FEV1FEV6=df[[param]]))
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
#'
#' # Example 1: Calculate LLN of FEV1 from the GLI equations
#'  example_df <- GLIData
#'  output <- calculateMeanPret(
#'                df=example_df,
#'                param="FEV1",
#'                ref="GLI")
#'
#' # Example 2: Calculate LLN of FEV1 from NHANES3
#'  example_df <- NHANES3Data
#'
#'  output <- calculateMeanPret(
#'                 df=example_df,
#'                 param="FEV1",
#'                 ref="NHANES3")
#'
#' @references
#' Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
#' Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel, G.,
#' Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).
#' The Global Lung Function Initiative (GLI) Network: bringing the world's
#' respiratory reference values together. Breathe (Sheffield, England), 13(3),
#' e56–e64. https://doi.org/10.1183/20734735.012717

#' Hankinson, J. L., Odencrantz, J. R., & Fedan, K. B. (1999).
#' Spirometric reference values from a sample of the general U.S. population.
#' American journal of respiratory and critical care medicine, 159(1), 179–187.
#' https://doi.org/10.1164/ajrccm.159.1.9712108
#'
#' Lytras T (2020). _rspiro: Implementation of Spirometry Equations_.
#' R package version 0.2, <https://CRAN.R-project.org/package=rspiro>.
#' @export
#' @import rspiro
calculateMeanPret <- function(df,
                              param = "FEV1",
                              ref = "GLI") {
  # check that there's a dataframe
  if(!is.data.frame(df)) {
    stop("Please input a dataframe into calculateMeanPret")
  }

  spiroOptions <- c("FEV1", "FVC", "FEV1FVC", "PEF", "FEF2575", "FEV6", "FEV1FEV6")

  if(!(param %in% spiroOptions)){
    stop("Please enter a valid spirometry metric in calculatePctPret")
  }

  # if the df has age, height, gender
  if(is.null(df$age) || is.null(df$height)){
    stop("Not enough demographic parameters to calculate mean predicted")
  }

  age <- as.numeric(df$age)
  height <- as.numeric(df$height)

  if(!is.null(df$gender)){
    gender <- df$gender
  } else {
    gender <- rep(1, length(df$age))
  }

  if(!is.null(df$ethnicity)){
    ethnicity <- df$ethnicity
    if (any(ethnicity>5)) {
      ethnicity[which(ethnicity>5)] <- 1
      warning("Ethnicity value not valid for spriometry equations, setting some ethnicity values to default of 1")
    }
  } else {
    ethinicity <- rep(1, length(df$age))
  }

  if (ref == "NHANES3") {
    if (any(ethnicity>3)) {
      ethnicity[which(ethnicity>3)] <- 1
      warning("Ethnicity value not valid for NHANES3 equations, setting ethnicity to default of 1")
    }
    return(rspiro::pred_NHANES3(age=age,
                                height=height,
                                gender=gender,
                                ethnicity=ethnicity,
                                param=param))
  } else if (ref == "GLI") {
    return(rspiro::pred_GLI(age=age,
                            height=height,
                            gender=gender,
                            ethnicity=ethnicity,
                            param=param))
  } else {
    stop("Please select NHANES3 or GLI")
  }
}


# [END]
