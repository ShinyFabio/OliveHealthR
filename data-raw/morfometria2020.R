## code to prepare `Morfometria2020` dataset goes here


library(dplyr)
library(readxl)
drupe = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/morfo/morfometria_drupe_aggiornato.xlsx") %>%
  janitor::remove_empty("rows")

foglie = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/morfo/morfometria_foglie_aggiornato.xlsx") %>%
  janitor::remove_empty("rows")

endocarpo = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/morfo/morfometria_endocarpo.xlsx") %>%
  janitor::remove_empty("rows")

rapporti = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/morfo/Rapporti_drupe_endocarpo.xlsx") %>%
  janitor::remove_empty("rows")

morfometria2020 = list("Foglie" = foglie, "Drupe" = drupe, "Endocarpo" = endocarpo, "Rapporti" = rapporti) #mancano endocarpo e rapporti che non sono aggiornati

usethis::use_data(morfometria2020, overwrite = TRUE)
