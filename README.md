
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

You can install the development version of spiroview from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("annie951977/spiroview")
```

Overview

This is a basic example which shows you how to solve a common problem:

``` r
# library(spiroview)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

# Contributions

# References

- Cooper, B. G., Stocks, J., Hall, G. L., Culver, B., Steenbruggen, I.,
  Carter, K. W., Thompson, B. R., Graham, B. L., Miller, M. R., Ruppel,
  G., Henderson, J., Vaz Fragoso, C. A., & Stanojevic, S. (2017).The
  Global Lung Function Initiative (GLI) Network: bringing the world’s
  respiratory reference values together. Breathe (Sheffield, England),
  13(3), e56–e64. <https://doi.org/10.1183/20734735.012717>

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
