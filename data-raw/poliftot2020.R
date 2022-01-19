## code to prepare `poliftot2020` dataset goes here
#poliftot2020 è il file dei polifenoli totali di foglie, drupe, olio, posa e sansa della raccolta 2020.

library(readxl)
library(janitor)
library(usethis)
library(dplyr)
library(tidyr)

cv <- function(x) 100*(sd(x)/mean(x))

#### drupe ####
#header del file:
#Codice_azienda	| N_campionamento	| Anno | Estrazione | Replicato	| Polifenoli_replicati_mg.ml | ml_estrazione	| g_drupe_estrazione | Presenza_larve

drupe = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polif_tot_drupe.xlsx") %>%
  janitor::remove_empty("rows")

drupe = drupe %>% tidyr::unite(col = cod_rep, Codice_azienda , Estrazione , remove = FALSE) %>% 
  dplyr::group_by(dplyr::across(-c(Polifenoli_replicati_mg.ml, Replicato))) %>% 
  dplyr::summarise(dplyr::across(Polifenoli_replicati_mg.ml, list(media = mean, CV = cv))) %>% dplyr::ungroup()

#rimuovo i campioni con CV > 30
message("Ci sono ", sum(drupe$Polifenoli_replicati_mg.ml_CV > 30, na.rm = TRUE), " campioni con CV > 30")
drupe = drupe %>% dplyr::filter(Polifenoli_replicati_mg.ml_CV < 30)

#calcolare i mg/g di drupe
drupe = drupe %>% dplyr::mutate("Polifenoli (mg/g drupe)" = Polifenoli_replicati_mg.ml_media * ml_estrazione / g_drupe_estrazione) %>%
  dplyr::select("Codice_azienda", "N_campionamento", "Anno", "Estrazione", "Polifenoli (mg/g drupe)") %>% 
  dplyr::mutate(across(where(is.double), round,3))

# drupe$Presenza_larve = readr::parse_factor(as.character(drupe$Presenza_larve), levels = c("0","1","2"), ordered = TRUE)
# drupe = within(drupe, levels(Presenza_larve)[levels(Presenza_larve) == "0"] <- "Non individuabili")
# drupe = within(drupe, levels(Presenza_larve)[levels(Presenza_larve) == "1"] <- "Poche larve")
# drupe = within(drupe, levels(Presenza_larve)[levels(Presenza_larve) == "2"] <- "Molte larve")
drupe$Anno = factor(drupe$Anno)


#summarizare ancora
 drupe_summ =  drupe %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
   dplyr::summarise(dplyr::across("Polifenoli (mg/g drupe)", mean, na.rm = T)) %>% dplyr::ungroup()


#### foglie ####
#header del file:
#Codice_azienda	| N_campionamento	| Anno | Estrazione | Replicato	| Polifenoli_replicati_mg.ml | ml_estrazione	| g_foglie_estrazione
foglie = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polif_tot_foglie.xlsx") %>% 
  janitor::remove_empty("rows")

foglie = foglie %>% tidyr::unite(col = cod_rep, Codice_azienda , Estrazione , remove = FALSE) %>% 
  dplyr::group_by(dplyr::across(-c(Polifenoli_replicati_mg.ml, Replicato))) %>% 
  dplyr::summarise(dplyr::across(Polifenoli_replicati_mg.ml, list(media = mean, CV = cv))) %>% dplyr::ungroup()

#rimuovo i campioni con CV > 30
message("Ci sono ", sum(foglie$Polifenoli_replicati_mg.ml_CV > 30, na.rm = TRUE), " campioni con CV > 30")
foglie = foglie %>% dplyr::filter(Polifenoli_replicati_mg.ml_CV < 30)

#calcolare i mg/g di foglie
foglie = foglie %>% dplyr::mutate("Polifenoli (mg/g foglie)" = Polifenoli_replicati_mg.ml_media * ml_estrazione / g_foglie_estrazione) %>%
  dplyr::select("Codice_azienda", "N_campionamento", "Anno", "Estrazione", "Polifenoli (mg/g foglie)") %>% 
  dplyr::mutate(across(where(is.double), round,3))

foglie$Anno = factor(foglie$Anno)

#summarizare ancora
# foglie_summ = foglie %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
#  dplyr::summarise(dplyr::across("Polifenoli (mg/g foglie)", mean, na.rm = T)) %>% dplyr::ungroup()


#### olio ####
#header del file:
#Codice_azienda	| Tipo_olio | Anno |	Estrazione | Replicato | Polifenoli_replicati_mg.ml | ml_estrazione | g_olio_estrazione
olio = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polif_tot_olio.xlsx") %>% 
  janitor::remove_empty("rows")

olio = olio %>% tidyr::unite(col = cod_rep, Codice_azienda , Estrazione , remove = FALSE) %>% 
  dplyr::group_by(dplyr::across(-c(Polifenoli_replicati_mg.ml, Replicato))) %>% 
  dplyr::summarise(dplyr::across(Polifenoli_replicati_mg.ml, list(media = mean, CV = cv))) %>% dplyr::ungroup()

#rimuovo i campioni con CV > 30
message("Ci sono ", sum(olio$Polifenoli_replicati_mg.ml_CV > 30, na.rm = TRUE), " campioni con CV > 30")
olio = olio %>% dplyr::filter(Polifenoli_replicati_mg.ml_CV < 30)

#calcolare i mg/kg di olio
olio = olio %>% dplyr::mutate("Polifenoli (mg/kg olio)" = Polifenoli_replicati_mg.ml_media * 1000 * ml_estrazione / g_olio_estrazione) %>%
  dplyr::select("Codice_azienda", "N_campionamento", "Tipo_olio", "Anno", "Estrazione", "Polifenoli (mg/kg olio)") %>% 
  dplyr::mutate(across(where(is.double), round,3))

olio$Anno = factor(olio$Anno)

#summarizare ancora
# olio_summ = olio %>% dplyr::group_by(Codice_azienda, Tipo_olio, Anno, N_campionamento) %>% 
#  dplyr::summarise(dplyr::across("Polifenoli (mg/kg olio)", mean, na.rm = T)) %>% dplyr::ungroup()


#### posa ####
#header del file:
#Codice_azienda	| Tipo_olio | Anno |	Estrazione | Replicato | Polifenoli_replicati_mg.ml | ml_estrazione | g_posa_estrazione
posa = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polif_tot_posa.xlsx") %>% 
  janitor::remove_empty("rows")

posa = posa %>% tidyr::unite(col = cod_rep, Codice_azienda , Estrazione , remove = FALSE) %>% 
  dplyr::group_by(dplyr::across(-c(Polifenoli_replicati_mg.ml, Replicato))) %>% 
  dplyr::summarise(dplyr::across(Polifenoli_replicati_mg.ml, list(media = mean, CV = cv))) %>% dplyr::ungroup()

#rimuovo i campioni con CV > 30
message("Ci sono ", sum(posa$Polifenoli_replicati_mg.ml_CV > 30, na.rm = TRUE), " campioni con CV > 30")
posa = posa %>% dplyr::filter(Polifenoli_replicati_mg.ml_CV < 30)

#calcolare i mg/g di posa
posa = posa %>% dplyr::mutate("Polifenoli (mg/kg posa)" = Polifenoli_replicati_mg.ml_media * 1000* ml_estrazione / g_posa_estrazione) %>%
  dplyr::select("Codice_azienda", "N_campionamento", "Tipo_olio", "Anno", "Estrazione", "Polifenoli (mg/kg posa)") %>% 
  dplyr::mutate(across(where(is.double), round,3))

posa$Anno = factor(posa$Anno)

#summarizare ancora
# posa_summ <- posa %>% dplyr::group_by(Codice_azienda, Tipo_olio, Anno, N_campionamento) %>% 
#   dplyr::summarise(dplyr::across("Polifenoli (mg/kg posa)", mean, na.rm = T)) %>% dplyr::ungroup()


#### sansa ####
#header del file:
#Codice_azienda	| Tipo_olio | Anno |	Estrazione | Replicato | Polifenoli_replicati_mg.ml | ml_estrazione | g_sansa_estrazione
sansa = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polif_tot_sansa.xlsx") %>% 
  janitor::remove_empty("rows")

sansa = sansa %>% tidyr::unite(col = cod_rep, Codice_azienda , Estrazione , remove = FALSE) %>% 
  dplyr::group_by(dplyr::across(-c(Polifenoli_replicati_mg.ml, Replicato))) %>% 
  dplyr::summarise(dplyr::across(Polifenoli_replicati_mg.ml, list(media = mean, CV = cv))) %>% dplyr::ungroup()

#rimuovo i campioni con CV > 30
message("Ci sono ", sum(sansa$Polifenoli_replicati_mg.ml_CV > 30, na.rm = TRUE), " campioni con CV > 30")
sansa = sansa %>% dplyr::filter(Polifenoli_replicati_mg.ml_CV < 30)
#calcolare i mg/g di sansa
sansa = sansa %>% dplyr::mutate("Polifenoli (mg/kg sansa)" = Polifenoli_replicati_mg.ml_media * 1000 * ml_estrazione / g_sansa_estrazione) %>%
  dplyr::select("Codice_azienda", "N_campionamento", "Tipo_olio", "Anno", "Estrazione", "Polifenoli (mg/kg sansa)") %>% 
  dplyr::mutate(across(where(is.double), round,3))

sansa$Anno = factor(sansa$Anno)

#summarizare ancora
# sansa_summ <- sansa %>% dplyr::group_by(Codice_azienda, Tipo_olio, Anno, N_campionamento) %>% 
#   dplyr::summarise(dplyr::across("Polifenoli (mg/kg sansa)", mean, na.rm = T)) %>% dplyr::ungroup()

#### unione ####

poliftot2020 = list("Foglie" = foglie, "Drupe" = drupe, "Olio" = olio, "Posa" = posa, "Sansa" = sansa)

#poliftot2020_summ = list("Foglie" = foglie_summ, "Drupe" = drupe_summ, "Olio" = olio_summ, "Posa" = posa_summ, "Sansa" = sansa_summ)

usethis::use_data(poliftot2020, overwrite = TRUE)
#usethis::use_data(poliftot2020_summ, overwrite = TRUE)
