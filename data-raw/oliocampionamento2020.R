## code to prepare `oliocampionamento2020` dataset goes here
#oliocampionamento2020 è il file delle schede campionamento olio della raccolta 2020. Il file csv è "schede_olio.csv".

library(readr)
library(janitor)
library(usethis)

oliocampionamento2020 = readr::read_delim("C:/Users/fabio/Desktop/file progetto/schede_olio.csv", delim = ";", col_names = TRUE, local = readr::locale(decimal_mark = ",", date_format = "%d/%m/%Y", encoding = "windows-1252")) %>% 
  janitor::remove_empty("rows")
oliocampionamento2020$Olio = factor(oliocampionamento2020$Olio, levels = c("SI", "NO"), ordered = FALSE)
oliocampionamento2020$Sansa  = factor(oliocampionamento2020$Sansa, levels = c("SI", "NO"), ordered = FALSE)


usethis::use_data(oliocampionamento2020, overwrite = TRUE)
