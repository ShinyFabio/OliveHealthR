
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OliveHealthR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of OliveHealthR is to analyze and map all the information
obtained from the [OliveHealth](https://olivehealth.it) project. The
planned activities will concern the mapping of the olive groves of the 5
Campania provinces (Italy) and the analysis of the health component (eg
polyphenols) on samples such as leaves, drupes and oil.

## Installation

Install devtools if not already installed.

``` r
install.packages("devtools")
```

You can install OliveHealthR from
[GitHub](https://github.com/ShinyFabio/OliveHealthR2) with:

``` r
devtools::install_github("ShinyFabio/OliveHealthR2")
```

Be sure that your R version is at least 4.0.0.

## Launch

To launch the app use this code

``` r
library(OliveHealthR2)
OliveHealthR2::run_app()
```

## Other

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
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.