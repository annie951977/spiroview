% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculatedPredicted.R
\name{calculatePctPret}
\alias{calculatePctPret}
\title{Calculates the percent predicted based off of inputted parameters
Assumptions:
\itemize{
\item df is a dataframe row that contains at least the age and the height
of the patient
\item gender is denoted as 1 or 2 for male or female respectively
\item ethnicity is denoted as a value between 1 to 5
\item height is assumed to be in meters
}}
\usage{
calculatePctPret(df, param = "FEV1", ref = "GLI")
}
\arguments{
\item{df}{A dataframe containing colnames age and height and a spirometric column
matching param at the minimum}

\item{param}{The spirometric metric of interest: "FEV1", "FVC", "FEV1FVC",
"PEF", "FEF2575", "FEV6", "FEV1FEV6".}

\item{ref}{The reference equation to be used}
}
\value{
A numeric vector or data.frame containing length(param) columns with
rows equal to the number of rows in df
}
\description{
Calculates the percent predicted based off of inputted parameters
Assumptions:
\itemize{
\item df is a dataframe row that contains at least the age and the height
of the patient
\item gender is denoted as 1 or 2 for male or female respectively
\item ethnicity is denoted as a value between 1 to 5
\item height is assumed to be in meters
}
}
\examples{
# Example 1: Calculate LLN of FEV1 from the GLI equations
\dontrun{
 exampleDF <- GLIData
 output <- calculatePctPret(
               df=exampleDF,
               param="FEV1",
               ref="GLI")
 output
}
# Example 2: Calculate LLN of FEV1 from NHANES3
\dontrun{
 exampleDF <- NHANES3Data

 output <- calculatePctPret(
                df=exampleDF,
                param="FEV1",
                ref="NHANES3")
output
}

}
\references{
Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel, G.,
Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).
The Global Lung Function Initiative (GLI) Network: bringing the world's
respiratory reference values together. Breathe (Sheffield, England), 13(3),
e56–e64. https://doi.org/10.1183/20734735.012717
Hankinson, J. L., Odencrantz, J. R., & Fedan, K. B. (1999).
Spirometric reference values from a sample of the general U.S. population.
American journal of respiratory and critical care medicine, 159(1), 179–187.
https://doi.org/10.1164/ajrccm.159.1.9712108

Lytras T (2020). \emph{rspiro: Implementation of Spirometry Equations}.
R package version 0.2, \url{https://CRAN.R-project.org/package=rspiro}.
}
