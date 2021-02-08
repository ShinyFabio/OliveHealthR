
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OliveHealthR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of OliveHealthR is to …

## Installation

You can install the released version of OliveHealthR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("OliveHealthR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(OliveHealthR)
#> Carico il pacchetto richiesto: tmap
#> Carico il pacchetto richiesto: tmaptools
#> Carico il pacchetto richiesto: plotly
#> Carico il pacchetto richiesto: ggplot2
#> 
#> Attaching package: 'plotly'
#> The following object is masked from 'package:ggplot2':
#> 
#>     last_plot
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:graphics':
#> 
#>     layout
#> Carico il pacchetto richiesto: shinythemes
#> Carico il pacchetto richiesto: shinydashboard
#> 
#> Attaching package: 'shinydashboard'
#> The following object is masked from 'package:graphics':
#> 
#>     box
#> Carico il pacchetto richiesto: shinyWidgets
#> Carico il pacchetto richiesto: ggfortify
#> Carico il pacchetto richiesto: scales
#> Carico il pacchetto richiesto: htmltools
#> Carico il pacchetto richiesto: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Carico il pacchetto richiesto: tidyr
#> Carico il pacchetto richiesto: tibble
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
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
