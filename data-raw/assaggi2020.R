## code to prepare `assaggi2020` dataset goes here
# Assaggi2020 è il file delle analisi sensoriali dell'olio della raccolta 2020

library(readr)
library(janitor)
library(usethis)

assaggi2020 = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/analisi_sensoriali.xlsx") %>%
  janitor::remove_empty("rows")
assaggi2020$Anno = factor(assaggi2020$Anno)

usethis::use_data(assaggi2020, overwrite = TRUE)
