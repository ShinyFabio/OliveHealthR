## code to prepare `polifenoliLCxLC` dataset goes here
#polifenoliLCxLC è una lista contenente tutti e 5 i file dei polifenoli LCxLC. Per utilizzare ad esempio le foglie,
#basta usare polifenoliLCxLC$Foglie e si ha il vettore.
library(readr)
library(janitor)
library(usethis)



foglie = readr::read_delim("C:/Users/fabio/Desktop/OliveHealthR/data-raw/cromatogrammi_precisi_2020/Tabelle campioni foglie.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")


drupe = readr::read_delim("C:/Users/fabio/Desktop/OliveHealthR/data-raw/cromatogrammi_precisi_2020/Tabelle campioni drupe.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")

olio = readr::read_delim("C:/Users/fabio/Desktop/OliveHealthR/data-raw/cromatogrammi_precisi_2020/Tabelle campioni oli.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")

posa = readr::read_delim("C:/Users/fabio/Desktop/OliveHealthR/data-raw/cromatogrammi_precisi_2020/Tabelle campioni posa.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")

sansa = readr::read_delim("C:/Users/fabio/Desktop/OliveHealthR/data-raw/cromatogrammi_precisi_2020/Tabelle campioni sansa.csv", delim = ";", col_names = T, local = readr::locale(decimal_mark = ",", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")


polifenoliLCxLC2020 = list("Foglie" = foglie, "Drupe" = drupe, "Olio" = olio, "Posa" = posa, "Sansa" = sansa)

usethis::use_data(polifenoliLCxLC2020, overwrite = TRUE)
