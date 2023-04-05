
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spiroview

<!-- badges: start -->
<!-- badges: end -->

# Description

`spiroview` is an R package that provides the tools to visualize and
explore spirometry data in demographic studies. This package is targeted
for the healthcare focused researcher who may not have an extensive
coding background but wishes to use R for their research study. The
`spiroview` package was developed using `R version 4.2.1 (2022-06-23)`,
`Platform: x86_64-apple-darwin17.0 (64-bit)`, and
`Running under: macOS Big Sur 11.7.4`.

## Installation

To install the latest version of this package:

``` r
require("devtools")
devtools::install_github("annie951977/spiroview")
```

To run the Shiny app

``` r
spiroview::runspiroview()
```

# Overview

spiroview at a glance:

``` r
ls("package:spiroview")
data(package = "spiroview")
browseVignettes("spiroview")
```

`spiroview` contains 10 functions:

`formatData` is a helper function that reads a csv or tsv datafile and
reformats the dataframe so that the dataframe can be easily read into
other functions in this dataset.

`segregateBy` splits a dataframe by a demographic value to allow for the
extraction of data relating to a subset of interest.

`calculateLLNPret` calculates the lower limit of normal for a
spirometric measurement based off of inputted demographic variables such
as height, age, and gender and the spirometry reference of interest.

`calculatePctPret` calculates

`calculateMeanPret` calculates the mean predicted value for a
spirometric measurement based off of inputted demographic variables such
as height, age, and gender.

`summarizeAllByCategory` extracts datainputs matching a certain
demographic threshold and outputs summary statistics on a spirometric
measurement in the dataset.

`viewCategorical` produces plots correlating a categorical variable to a
spirometric measurement.

`viewCategoricalCounts` produces plots that showcase the number of
datapoints that exist in the dataset for a categorical variable of
interest

`viewNumerical` produces plots correlating a numerical variable to a
spirometric measurement.

`compareNumerical` produces plots correlating a numerical variable to a
spirometric measurement annotated by other categorical or numerical
demographic variables of interest.

# Contributions

- `formatData` uses packages `utils` and `readr` to handle file reading

- `segregateBy` uses package `stringr` for string matching

- `calculateLLNPret`, `calculatePctPret`, `calculateMeanPret` use
  package `rspiro` for spirometric equations and calculations

- `summarizeAllByCategory` uses package `stringr` for string matching
  and package `dplyr` for filtering

- `viewCategorical`,`viewCategoricalCounts`, and `viewNumerical` use
  package `ggplot2` for generating plots

- `compareNumerical` uses package `ggplot2` for generating plots. This
  function also uses `stringr` for string matching and `dplyr` for
  dataset filtering

# References

- Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
  McPherson J, Dipert A, Borges B (2023). shiny: Web Application
  Framework for R. R package version 1.7.4.9002,
  <https://shiny.rstudio.com/>.

- Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
  Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel,
  G., Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).The
  Global Lung Function Initiative (GLI) Network: bringing the world’s
  respiratory reference values together. Breathe (Sheffield, England),
  13(3), e56–e64. <https://doi.org/10.1183/20734735.012717>

- Grolemund, G. (2015). Learn Shiny - Video Tutorials.
  URL:<https://shiny.rstudio.com/tutorial/>

- Hankinson, J. L., Odencrantz, J. R., & Fedan, K. B. (1999).
  Spirometric reference values from a sample of the general U.S.
  population. American journal of respiratory and critical care
  medicine, 159(1), 179–187.https://doi.org/10.1164/ajrccm.159.1.9712108

- Lytras T (2020). *rspiro: Implementation of Spirometry Equations*. R
  package version 0.2, <https://CRAN.R-project.org/package=rspiro>.

- R Core Team (2021). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL <https://www.R-project.org/>.

- Silva, A. (2022) TestingPackage: An Example R Package For BCB410H.
  Unpublished. URL <https://github.com/anjalisilva/TestingPackage>.”

- Wickham, H. and Bryan, J. (2019). *R Packages* (2nd edition). Newton,
  Massachusetts: O’Reilly Media. <https://r-pkgs.org/>

- Wickham H, François R, Henry L, Müller K, Vaughan D (2023). dplyr: A
  Grammar of Data Manipulation. <https://dplyr.tidyverse.org>,
  <https://github.com/tidyverse/dplyr>.

- Wickham H, Hester J, Bryan J (2023). readr: Read Rectangular Text
  Data. <https://readr.tidyverse.org>,
  <https://github.com/tidyverse/readr>.

- Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
  Springer-Verlag New York. ISBN 978-3-319-24277-4,
  <https://ggplot2.tidyverse.org>.

- Wickham H (2022). *stringr: Simple, Consistent Wrappers for Common
  String Operations*. R package version 1.5.0,
  <https://CRAN.R-project.org/package=stringr>.

# Acknowledgements

This package was developed as part of an assessment for 2022 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. `spiroview` welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues.

# Package Structure

The package tree structure is provided below.

``` r
- spiroview
  ├── DESCRIPTION
  ├── LICENSE
  ├── LICENSE.md
  ├── NAMESPACE
  ├── R
  │   ├── calculatedPredicted.R
  │   ├── data.R
  │   ├── formatData.R
  │   ├── runspiroview.R    
  │   ├── segregateBy.R
  │   ├── summarize.R
  │   ├── viewCategorical.R
  │   └── viewNumerical.R
  ├── README.Rmd
  ├── README.md
  ├── data
  │   ├── GLIData.rda
  │   └── NHANES3Data.rda
  ├── inst
  │   ├── CITATION
  │   ├── extdata
  │   │   ├── GLI_simulated_DB.csv
  │   │   ├── NHANES3_simulated_DB.csv
  │   │   ├── example_dataset_1.csv
  │   │   ├── example_dataset_2.csv
  │   │   ├── example_dataset_3.csv
  │   │   └── example_dataset_4.csv
  │   └── shiny-scripts
  │       └── app.R
  ├── man
  │   ├── GLIData.Rd
  │   ├── NHANES3Data.Rd
  │   ├── calculateLLNPret.Rd
  │   ├── calculateMeanPret.Rd
  │   ├── calculatePctPret.Rd
  │   ├── compareNumerical.Rd
  │   ├── figures
  │   ├── formatData.Rd
  │   ├── segregateBy.Rd
  │   ├── summarizeAllByCategory.Rd
  │   ├── viewCategorical.Rd
  │   ├── viewCategoricalCounts.Rd
  │   └── viewNumerical.Rd
  ├── spiroview.Rproj
  ├── tests
  │   ├── testthat
  │   │   ├── test-calculatedPredicted.R
  │   │   ├── test-formatData.R
  │   │   ├── test-segregateBy.R
  │   │   ├── test-summarize.R
  │   │   ├── test-viewCategorical.R
  │   │   └── test-viewNumerical.R
  │   └── testthat.R
  └── vignettes
      └── Introduction_spiroview.Rmd
```
