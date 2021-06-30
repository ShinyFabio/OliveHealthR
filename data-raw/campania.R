## code to prepare `campania` dataset goes here
#shapefile della campania

library(sf)

campania = sf::st_read("C:/Users/fabio/Desktop/file progetto/mappa/Campania_prov.shp")
save(campania, file = "Campania.rda")

usethis::use_data(campania, overwrite = TRUE)
