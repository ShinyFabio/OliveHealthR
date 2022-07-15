## code to prepare `drupecamp2020` dataset goes here
#"drupecamp2020" è il file delle schede campionamento di drupe e foglie della raccolta 2020.

library(readxl)
library(janitor)
library(usethis)
library(lubridate)

#datidrupe
drupecamp2020 = readxl::read_xlsx("C:/Users/fabio/Desktop/OliveHealthR/data-raw/dati_drupe_campionamenti.xlsx") %>% 
  janitor::remove_empty("rows")
drupecamp2020$Data_campionamento = lubridate::as_date(drupecamp2020$Data_campionamento)
drupecamp2020$Indice_maturazione = factor(drupecamp2020$Indice_maturazione, levels = c(0:8), ordered = TRUE)
drupecamp2020$Fase_fenologica = factor(drupecamp2020$Fase_fenologica, levels = c(51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89), ordered = TRUE)
drupecamp2020$Anno = factor(drupecamp2020$Anno)

usethis::use_data(drupecamp2020, overwrite = TRUE)
