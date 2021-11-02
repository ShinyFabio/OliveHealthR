## code to prepare `data` dataset goes here
#"data" è il file delle aziende coinvolte.

library(readxl)
library(janitor)
library(usethis)

data_aziende = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/2_Dati_monitoraggio.xlsx") %>% 
  janitor::remove_empty("rows")

usethis::use_data(data_aziende, overwrite = TRUE)
