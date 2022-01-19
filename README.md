
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OliveHealthR <img src="man/figures/OliveHealthRfavicon.png" align="right" height="139"/>

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

Be sure that your R version is at least 4.0.0. Click yes if R asks you
something.

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

Se R ti chiede di installare i pacchetti che richiedono compilazione
clicca su “No”. Una volta completata l’installazione, dovrai riscaricare
le foto (vedi step 6 dell’installazione).

### “File aziende (.csv)”

The most important file is “2_Dati_monitoraggio.csv”. In order to make
the app work, this csv file has some columns that have to follow some
writing rules. The following columns are mandatory:

-   **“Codice_azienda”** filled with the code of the company (eg SA_01,
    AV_12 etc.)
-   **“Azienda”** filled with the name (eg. Az.Agr.Perretta_Nicola).
    Don’t use “-” or spaces in the name
-   **“Provincia”** filled with the province (eg. SA, AV)
-   **“UTM_33T_E”** filled with the UTM 33T East coordinate
-   **“UTM_33T_N”** filled with the UTM 33T North coordinate
-   **“Cultivar_principale”** filled with the main cultivar (use “\_”
    instead of space)

### “File descrizione .csv”

This is an optional csv file containing some info about companies. It
contains:

-   **“Codice_azienda”** same
-   **“Descrizione”** filled with the description of the company
-   **“Contatti”** filled with the phone number, email and others infos

### “file CSV con dati drupe”

Here you can upload the phenological phase and ripening index extracted
from the record cards. It follows the following structure:

-   **“Codice_azienda”** same
-   **“N_campionamento”** filled with “R1” or “R2” where R1 refers to
    the first sampling (September/October) and R2 refers to the last
    sampling (end of October/beginning of November i.e. when the olives
    are completely ripe)
-   **“Data_campionamento”** filled with the sampling date (in the form
    of dd/mm/yyyy)
-   **“Fase_fenologica”** (filled with a number)
-   **“Indice_maturazione”** (filled with a number)
-   **“Note”** filled with possible notes

### “file CSV con dati polifenoli”

Here you can upload the polyphenols datas. The csv file follows this
structure:

-   **“Codice_azienda”** same
-   **“N_campionamento”** same
-   **“Polifenoli_tot”** filled with the concentration of total
    polyphenols (mg/g drupes)
-   **“Presenza_larve”** filled with the larvae presence indicator (from
    0 to 2)
-   Various polyphenols (eg. “Acido_Gallico”). unit of measure: ug/ml of
    oil

## Other

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

# `{r cars} # summary(cars) #`

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

# `{r pressure, echo = FALSE} # plot(pressure) #`

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
