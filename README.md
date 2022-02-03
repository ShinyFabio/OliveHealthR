
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OliveHealthR <img src="man/figures/OliveHealthRfavicon.png" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable-succes.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

The goal of OliveHealthR is to analyze and map all the information
obtained from the [OliveHealth](https://olivehealth.it) project. The
planned activities will concern the mapping of the olive groves of the 5
Campania provinces (Italy) and the analysis of the health component (eg
polyphenols) on samples such as leaves, drupes and oil.

## Installation

<strong>1. Install R. For Windows use this link:</strong>

    https://cran.r-project.org/bin/windows/base/

<strong>2. Install RStudio from this link. Choose your OS.</strong>

    https://www.rstudio.com/products/rstudio/download/#download

<strong>3. If you are on Windows, install Rtools from this link
otherwise skip this line.</strong>

    https://cran.r-project.org/bin/windows/Rtools

<strong>4. Now open RStudio and install devtools if not already
installed. Just copy this code in the Console (on the left) and click
enter. Click yes if R asks you something.</strong>

``` r
install.packages("devtools")
```

<strong>5. Now you can install OliveHealthR copying and launching this
code in the Console:</strong>

``` r
devtools::install_github("ShinyFabio/OliveHealthR")
```

Be sure that your R version is at least 4.0.0. If R asks you to Install
packages that requires compilation, click “No” otherwise the
installation will take a long time (especially if your computer is not
powerful).

<strong>6. When installation is completed, you can launch this code if
you want to download all photos (this step is optional):</strong>

``` r
OliveHealthR::download_photo()
```

### Ubuntu (tested on 18.04)

If your OS is Ubuntu you have to perform some additional steps. In
command line run these lines:

``` r
sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev   #for {devtools} library
sudo apt-get install libcairo2-dev                                            #for {Cairo} library
sudo apt-get install libxt-dev                                                #for {Cairo} library
sudo apt install libudunits2-dev                                              #for {units} library
sudo apt install libgdal-dev                                                  #for {sf} library
sudo apt install libmagick++-dev                                              #for {magick} library
sudo apt-get install libc6
```

### MacOS (should be tested again)

If you’re a MacOS user, you have to run this line in the Terminal:

``` r
brew install imagemagick@6
```

## Launch

To launch the app use this code

``` r
library(OliveHealthR)
OliveHealthR::run_OliveHealthR()
```

## How to use

This shiny app performs various analysis and plotting based on files
being uploaded. For now OliveHealthR accepts 4 different csv files, one
is mandatory and contains all the info about the companies (name, id,
coordinates…), the others 3 are different type of data.

## How to update the package

In order to update the package just run this code:

``` r
devtools::update_packages("OliveHealthR", upgrade = "always")
```

If R asks you to Install packages that requires compilation, click “No”.
After the updating process is completed, you should downlad photos again
if you need them (go to step 6 of installation).
