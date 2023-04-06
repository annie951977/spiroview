#' Simulated demographic spirometry data using Global Lung Function Initiative
#' reference equations
#'
#' A dataset containing simulated patient demographic and spirometry data using
#' Global Lung Initiative Reference equations.
#'
#'@source Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
#'Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel, G.,
#'Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).
#'The Global Lung Function Initiative (GLI) Network: bringing the world's
#'respiratory reference values together. Breathe (Sheffield, England), 13(3),
#' e56–e64. https://doi.org/10.1183/20734735.012717
#'
#'@format A CSV file with 50 observation of 8 variables
#'\describe{
#'  \item{id}{Simulated patient id using 4 digits}
#'  \item{gender}{Gender denoted by 1 for male, 2 for female}
#'  \item{age}{Simulated age of patient}
#'  \item{height}{Simulated height of patient}
#'  \item{ethnicity}{Ethnicity of patient, denoted by 1 = Caucasian,
#'   2 = African-American, 3 = NE Asian, 4 = SE Asian, 5 = Other/mixed}
#'  \item{smoking}{Whether or not patient is a smoker, denoted by 1 for
#'   yes and 0 for no}
#'  \item{FEV1}{Simulated FEV1 value using GLI equations multiplied by
#'   a random permutation between 0.2 and 1.2}
#'  \item{FVC}{Simulated FEV1 value using GLI equations multiplied by
#'   a random permutation between 0.2 and 1.2}
#' }
#'@examples
#'\dontrun{
#' GLIData
#' }
"GLIData"

#' Simulated demographic spirometry data using the third National Health and
#' Nutrition Examination Survey (NHANES III) equations
#'
#' A dataset containing simulated patient demographic and spirometry data using
#' Nutrition Examination Survey (NHANES III) equations
#'
#'@source Hankinson, J. L., Odencrantz, J. R., & Fedan, K. B. (1999).
#' Spirometric reference values from a sample of the general U.S. population.
#' American journal of respiratory and critical care medicine, 159(1), 179–187.
#' https://doi.org/10.1164/ajrccm.159.1.9712108
#'
#'@format A CSV file with 50 observation of 8 variables
#'\describe{
#'  \item{id}{Simulated patient id using 4 digits}
#'  \item{gender}{Gender denoted by 1 for male, 2 for female}
#'  \item{age}{Simulated age of patient}
#'  \item{height}{Simulated height of patient}
#'  \item{ethnicity}{Ethnicity of patient, denoted by 1 = Caucasian,
#'   2 = African-American, 3 = Mexican-American}
#'  \item{smoking}{Whether or not patient is a smoker, denoted by 1 for yes and 0 for no}
#'  \item{FEV1}{Simulated FEV1 value using NHANES3 equations multiplied by
#'   a random permutation between 0.2 and 1.2}
#'  \item{FVC}{Simulated FEV1 value using NHANES3 equations multiplied by
#'   a random permutation between 0.2 and 1.2}
#' }
#'@examples
#'\dontrun{
#' NHANES3Data
#' }
"NHANES3Data"

# [END]
