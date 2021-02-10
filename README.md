
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
OliveHealthR2::run_OliveHealthR()
```

## How to use

This shiny app performs various analysis and plotting based on files
being uploaded. For now OliveHealthR accepts 4 different csv files, one
is mandatory and contains all the info about the companies (name, id,
coordinates…), the others 3 are different type of data.

### “File aziende (.csv)”

The most important file is “2\_Dati\_monitoraggio.csv”. In order to make
the app work, this csv file has some columns that have to follow some
writing rules. The following columns are mandatory:

  - **“Codice\_azienda”** filled with the code of the company (eg
    SA\_01, AV\_12 etc.)
  - **“Azienda”** filled with the name (eg. Az.Agr.Perretta\_Nicola).
    Don’t use “-” or spaces in the name
  - **“Provincia”** filled with the province (eg. SA, AV)
  - **“UTM\_33T\_E”** filled with the UTM 33T East coordinate
  - **“UTM\_33T\_N”** filled with the UTM 33T North coordinate
  - **“Cultivar\_principale”** filled with the main cultivar (use "\_"
    instead of space)

### “File descrizione .csv”

This is an optional csv file containing some info about companies. It
contains:

  - **“Codice\_azienda”** same
  - **“Azienda”** same
  - **“Descrizione”** filled with the description of the company
  - **“Contatti”** filled with the phone number, email and others infos

### “file CSV con dati drupe”

Here you can upload the phenological phase and ripening index extracted
from the record cards. It follows the following structure:

  - **“Codice\_azienda”** same
  - **“N\_campionamento”** filled with “R1” or “R2” where R1 refers to
    the first sampling (September/October) abd R2 refers to the last
    sampling (end of October/ beginning of November so when the olives
    are completely ripe)
  - **“Data\_campionamento”** filled with the sampling date (in the form
    of dd/mm/yyyy)
  - **“Fase\_fenologica”** (filled with a number)
  - **“Indice\_maturazione”** (filled with a number)
  - **“Note”** filled with possible notes

### “file CSV con dati polifenoli”

Here you can upload the polyphenols datas. The csv file follows this
structure:

  - **“Codice\_azienda”** same
  - **“N\_campionamento”** same
  - **“Polifenoli\_tot”** filled with the concentration of total
    polyphenols (mg/g drupes)
  - **“Presenza\_larve”** filled with the larvae presence indicator
    (from 0 to 2)
  - Various polyphenols (eg. “Acido\_Gallico”). unit of measure: ug/ml
    of oil

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
