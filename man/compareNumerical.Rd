% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viewNumerical.R
\name{compareNumerical}
\alias{compareNumerical}
\title{Produces scatterplot with values meeting certain thresholds marked}
\usage{
compareNumerical(
  df,
  demParam,
  spiroParam,
  delim = NULL,
  secondParam = NULL,
  secondDelim = NULL,
  secondParamIsNumeric = FALSE,
  includeBestFit = FALSE,
  title = NULL
)
}
\arguments{
\item{df}{A dataframe containing each patient demographic data and at least
one mean spirometric value}

\item{demParam}{Demographic numeric category of interest}

\item{spiroParam}{The spirometric parameter of interest as a string}

\item{delim}{An delineation of demParam, must follow
the convention of "operator numeric" for example "<10".
Default is NULL}

\item{secondParam}{An additional demographic variable of interest that can be
either a numeric or categorical value.}

\item{secondDelim}{A delineation of secondParam. If it is a numerical variable
it must follow the convention of "operator numeric" for example "<10".
Default is NULL}

\item{secondParamIsNumeric}{States whether secondParam is a numeric or
categorical variable}

\item{includeBestFit}{A boolean stating if a the line of best fit should be
included in the plot}

\item{title}{Title for the plot as a character string. Default is NULL}
}
\value{
A basic ggplot scatterplot that can be built upon as needed
}
\description{
A function that produces numerical plot that is segregated based off of certain
delineations of a numerical or categorical parameter. Up to two delineations
can be added.
}
\details{
Assumption: delim exists within the domain of demParam and secondDelim
exists within the domain of secondParam
}
\examples{
plotData <- GLIData
# Example 1: compareNumerical with one numeric variable
\dontrun{
plot <- compareNumerical(df=plotData,
                        demParam="height",
                        delim=">1.40",
                        spiroParam="FEV1",
                        includeBestFit=FALSE)
plot
}
# Example 2: compareNumerical with a categorical variable
\dontrun{
plot <- compareNumerical(df=plotData,
                        demParam="height",
                        delim=">1.40",
                        secondParam= "smoking",
                        secondDlim="1",
                        secondParamIsNumeric=FALSE,
                        spiroParam="FEV1",
                        includeBestFit=FALSE)
plot
}
}
\references{
Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
Springer-Verlag New York. ISBN 978-3-319-24277-4,
https://ggplot2.tidyverse.org.

Wickham H, François R, Henry L, Müller K, Vaughan D (2023).
dplyr: A Grammar of Data Manipulation. https://dplyr.tidyverse.org,
https://github.com/tidyverse/dplyr.

Wickham H (2022). stringr: Simple, Consistent Wrappers for
Common String Operations. https://stringr.tidyverse.org,
https://github.com/tidyverse/stringr.
}
