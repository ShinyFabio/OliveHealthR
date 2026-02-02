#' Legge netcdf da ERA5-LAND mensile e assegna i valori ai punti delle aziende
#' 
#' @description Questa funzione Legge netcdf da ERA5-LAND mensile e assegna i valori ai punti delle aziende
#' 
#' @param ncfile ncfile path
#' @param dati_aziende A dataframe with the coordinates columns(UTM_33T_N and UTM_33T_E) and relative infos. Use data()
#' 
#' 
#' @importFrom sf st_as_sf st_transform st_coordinates st_nearest_feature
#' @importFrom dplyr select rename mutate
#' @importFrom lubridate as_date month year
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom terra sds rast
#' @importFrom tidyr pivot_longer


makedata_meteo = function(ncfile,
                          dati_aziende){
  
  # ncfile = paste0(base::system.file(package = "OliveHealthR"), "/data/precipitazioni_2020_2021.nc")
  
  df_sf <- sf::st_as_sf(x = dati_aziende,  coords = c("UTM_33T_E", "UTM_33T_N"), crs= 32633) %>% 
    sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>% as.data.frame()
  tobind = cbind(df_sf, sf::st_coordinates(df_sf$geometry)) %>% dplyr::select(-geometry) %>% dplyr::rename(Long = X, Lat = Y) %>% 
    dplyr::select(Codice_azienda, Long, Lat)

  
  # List all subdatasets (available variables in the NetCDF)
  subds <- terra::sds(ncfile)

  
  nc <- ncdf4::nc_open(ncfile)
  time2 = ncdf4::ncvar_get(nc, "valid_time")
  time2 = as.Date(time2/(24*60*60), origin = "1970-01-01", tz = "UTC")
  ncdf4::nc_close(nc)
  

  all_measure = list()
  m= "d2m"
  
  for(m in names(subds)){
    
    r <- terra::rast(ncfile, subds = m)
    r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE) %>% dplyr::rename(Long=x,Lat=y)
    # na.rm = TRUE toglie i pixel con NA quindi associa sempre un pixel vicino, si va atrovare 
    #quello senza NA piu vicino
    colnames(r_df)[-c(1,2)] <- as.character(time2)
    if(m == "d2m" | m == "t2m"){
      r_df = dplyr::mutate(r_df, across(-c(1,2), ~ .-273.15))
    }
    

    tobind_sf <- sf::st_as_sf(tobind, coords = c("Long", "Lat"), crs = 4326)
    r_df_sf  <- sf::st_as_sf(r_df,  coords = c("Long", "Lat"), crs = 4326)
    nearest_id <- sf::st_nearest_feature(tobind_sf, r_df_sf)
    
    r_df_out <- tobind %>% cbind(r_df[nearest_id, setdiff(names(r_df), c("Long", "Lat"))]) %>% 
      tidyr::pivot_longer(cols = -c(1:3),values_to = "value") %>% 
      dplyr::rename(Misura = value, Tempo = name)
    r_df_out$Tempo = lubridate::as_date(r_df_out$Tempo)

    all_measure[[m]]= r_df_out
    
  }
  
  #porto i m di acqua in mm di acqua
  all_measure$tp$Misura = all_measure$tp$Misura *1000
  return(all_measure)
  

  
}