## code to prepare `polifind2020` dataset goes here
#polifind2020 è il file dei polifenoli individuali di foglie, drupe olio e posa della raccolta 2020.

library(readxl)
library(janitor)
library(usethis)
library(dplyr)
library(tidyr)


#### drupe ####
#header del file:
#Codice_azienda, N_campionamento, Anno, Estrazione, Fattore_diluizione, ml_estrazione, g_drupe_estrazione,Acido_Gallico,
#Idrossitirosolo, Acido_siringico, Acido_p.Cumarico, Acido_trans.Ferulico, Oleuropeina, Quercetina

drupe_ind = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polifenoli_HPLC_drupe.xlsx") %>% 
  janitor::remove_empty("rows")

drupe_ind = drupe_ind %>% dplyr::mutate(dplyr::across(8:14, ~.x * Fattore_diluizione * ml_estrazione / g_drupe_estrazione), .keep ="unused") %>% 
  dplyr::mutate(across(where(is.double), round,3))
#sono ug/g
drupe_ind$Anno = factor(drupe_ind$Anno)

#summarizare ancora
# drupe_ind_summ =  drupe_ind %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
#   dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()


#### foglie ####
#header del file:
#Codice_azienda, N_campionamento, Anno, Estrazione, Fattore_diluizione, ml_estrazione, g_foglie_estrazione,Acido_Gallico,
#Idrossitirosolo, Acido_siringico, Acido_p.Cumarico, Acido_trans.Ferulico, Oleuropeina, Quercetina

foglie_ind = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polifenoli_HPLC_foglie.xlsx") %>% 
  janitor::remove_empty("rows")

foglie_ind = foglie_ind %>% dplyr::mutate(dplyr::across(8:14, ~.x * Fattore_diluizione * ml_estrazione / g_foglie_estrazione), .keep ="unused") %>% 
  dplyr::mutate(across(where(is.double), round,3))
#sono ug/g

foglie_ind$Anno = factor(foglie_ind$Anno)

#summarizare ancora
# foglie_ind_summ =  foglie_ind %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
#   dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()


#### olio ####
#header del file:
#Codice_azienda, Tipo_olio, Anno, Estrazione, Fattore_diluizione, ml_estrazione, g_olio_estrazione,Acido_Gallico,
#Idrossitirosolo, Acido_siringico, Acido_p.Cumarico, Acido_trans.Ferulico, Oleuropeina, Quercetina

olio_ind = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polifenoli_HPLC_olio.xlsx") %>% 
  janitor::remove_empty("rows")

olio_ind = olio_ind %>% dplyr::mutate(dplyr::across(9:14, ~.x * Fattore_diluizione * ml_estrazione * 1000/ g_olio_estrazione), .keep ="unused") %>% 
  dplyr::mutate(across(where(is.double), round,3))
#sono mg/kg

olio_ind$Anno = factor(olio_ind$Anno)

#summarizare ancora
# olio_ind_summ =  olio_ind %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno, Tipo_olio) %>% 
#   dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()


#### posa ####
#header del file:
#Codice_azienda, Tipo_olio, Anno, Estrazione, Fattore_diluizione, ml_estrazione, g_posa_estrazione,Acido_Gallico,
#Idrossitirosolo, Acido_siringico, Acido_p.Cumarico, Acido_trans.Ferulico, Oleuropeina, Quercetina

posa_ind = readxl::read_xlsx("C:/Users/fabio/Desktop/file progetto/polifenoli veri/polifenoli_HPLC_posa.xlsx") %>% 
  janitor::remove_empty("rows")

posa_ind = posa_ind %>% dplyr::mutate(dplyr::across(9:14, ~.x * Fattore_diluizione * ml_estrazione *1000/ g_posa_estrazione), .keep ="unused") %>% 
  dplyr::mutate(across(where(is.double), round,3))
#sono mg/kg

posa_ind$Anno = factor(posa_ind$Anno)

#summarizare ancora
# posa_ind_summ =  posa_ind %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno, Tipo_olio) %>% 
#   dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()


#### unione ####

polifind2020 = list("Foglie" = foglie_ind, "Drupe" = drupe_ind, "Olio" = olio_ind, "Posa" = posa_ind)

#polifind2020_summ = list("Foglie" = foglie_ind_summ, "Drupe" = drupe_ind_summ, "Olio" = olio_ind_summ, "Posa" = posa_ind_summ)

usethis::use_data(polifind2020, overwrite = TRUE)
#usethis::use_data(polifind2020_summ, overwrite = TRUE)
