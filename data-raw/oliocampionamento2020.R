## code to prepare `oliocampionamento2020` dataset goes here
#oliocampionamento2020 è il file delle schede campionamento olio della raccolta 2020. Il file csv è "schede_olio.csv".

library(readxl)
library(janitor)
library(usethis)
library(lubridate)

oliocampionamento2020 = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/schede_olio.xlsx") %>% 
  janitor::remove_empty("rows")
oliocampionamento2020$Data_campionamento = lubridate::as_date(oliocampionamento2020$Data_campionamento)
oliocampionamento2020$Olio = factor(oliocampionamento2020$Olio, levels = c("SI", "NO"), ordered = FALSE)
oliocampionamento2020$Sansa  = factor(oliocampionamento2020$Sansa, levels = c("SI", "NO"), ordered = FALSE)
oliocampionamento2020$Anno = factor(oliocampionamento2020$Anno)

usethis::use_data(oliocampionamento2020, overwrite = TRUE)
