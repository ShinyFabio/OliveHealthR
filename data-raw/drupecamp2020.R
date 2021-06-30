## code to prepare `drupecamp2020` dataset goes here
#"drupecamp2020" è il file delle schede campionamento di drupe e foglie della raccolta 2020.

library(readr)
library(janitor)
library(usethis)

#datidrupe
drupecamp2020 = readr::read_delim("C:/Users/fabio/Desktop/file progetto/dati_drupe_campionamenti.csv", delim = ";", col_names = TRUE, local = readr::locale(decimal_mark = ",",date_format = "%d/%m/%Y", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")
drupecamp2020$Indice_maturazione = factor(drupecamp2020$Indice_maturazione, levels = c(0:8), ordered = TRUE)
drupecamp2020$Fase_fenologica = factor(drupecamp2020$Fase_fenologica, levels = c(51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89), ordered = TRUE)


usethis::use_data(drupecamp2020, overwrite = TRUE)
