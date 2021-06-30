## code to prepare `Morfometria2020` dataset goes here



drupe = readr::read_delim("C:/Users/fabio/Desktop/file progetto/morfo/morfometria_drupe_aggiornato.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>%
  janitor::remove_empty("rows")

foglie = readr::read_delim("C:/Users/fabio/Desktop/file progetto/morfo/morfometria_foglie_aggiornato.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>%
  janitor::remove_empty("rows")

endocarpo = readr::read_delim("C:/Users/fabio/Desktop/file progetto/morfo/morfometria_endocarpo.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>%
  janitor::remove_empty("rows")

rapporti = readr::read_delim("C:/Users/fabio/Desktop/file progetto/morfo/Rapporti_drupe_endocarpo.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>%
  janitor::remove_empty("rows")

morfometria2020 = list("Foglie" = foglie, "Drupe" = drupe, "Endocarpo" = endocarpo, "Rapporti" = rapporti) #mancano endocarpo e rapporti che non sono aggiornati

usethis::use_data(morfometria2020, overwrite = TRUE)
