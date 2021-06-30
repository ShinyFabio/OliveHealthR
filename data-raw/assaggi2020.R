## code to prepare `assaggi2020` dataset goes here
# Assaggi2020 è il file delle analisi sensoriali dell'olio della raccolta 2020

library(readr)
library(janitor)
library(usethis)

assaggi2020 = readr::read_delim("C:/Users/fabio/Desktop/file progetto/analisi_sensoriali.csv", na = "", delim = ";", col_names = TRUE, local = readr::locale(decimal_mark = ",", date_format = "%d/%m/%Y", encoding = "windows-1252")) %>%
  janitor::remove_empty("rows")
assaggi2020$Anno = as.character(assaggi2020$Anno)

usethis::use_data(assaggi2020, overwrite = TRUE)
