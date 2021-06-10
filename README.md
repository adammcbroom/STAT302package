
# STAT302package

<!-- badges: start -->
[![R-CMD-check](https://github.com/adammcbroom/STAT302package/workflows/R-CMD-check/badge.svg)](https://github.com/adammcbroom/STAT302package/actions)
[![codecov](https://codecov.io/gh/adammcbroom/STAT302package/branch/master/graph/badge.svg?token=6YSJYBF7IT)](https://codecov.io/gh/adammcbroom/STAT302package)
<!-- badges: end -->

STAT302package includes an assortment of functions for statistical inference and prediction.

## Installation

You can install the package through GitHub using:

``` r
devtools::install_github("adammcbroom/STAT302package")
```

## Use

To view the vignette:

``` r
devtools::install_github("adammcbroom/STAT302package", build_vignette = TRUE, build_opts = c())
library(STAT302package)
# Use this to view the vignette in the STAT302package HTML help
help(package = "STAT302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302package")
```
