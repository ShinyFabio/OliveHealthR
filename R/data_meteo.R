#' Make a tmap object that can be rendered in shiny
#' 
#' @description This function render a tmap using a shapefile (campania) as background and a table with coordinates for the points. 
#' 
#' @param ncfile Path of the ncfile. "C:\\Users\\fabio\\Desktop\\OliveHealthR\\data-raw\\precipitazioni\\precipit_e_altro_20_21.nc"
#' @param data A dataframe with the coordinates columns(UTM_33T_N and UTM_33T_E) and relative infos. Use data()
#' 
#' 
#' @import tmap
#' @import tmaptools
#' @importFrom sf st_as_sf st_crs st_transform st_coordinates
#' @importFrom dplyr select rename
#' @importFrom lubridate as_date month year
#' @importFrom ncdf4 nc_open ncvar_get
#' 
#' @examples \dontrun{
#' 
#' output$map1 = make_tmap(
#'  data = data = dtdrupfilt(), dotlegend = showcolumnmap2()
#'  )
#' }
#' 
#' 
#' 

makedata_meteo = function(ncfile,
                      dati_aziende){
  
  ###coordinates file
  #Conversion of data frame to sf object and then transform coordinates
  df_sf <- sf::st_as_sf(x = dati_aziende,  coords = c("UTM_33T_E", "UTM_33T_N"), crs= 32633) %>% 
    sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>% as.data.frame()
  coordinates = cbind(df_sf, sf::st_coordinates(df_sf$geometry)) %>% dplyr::select(-geometry) %>% dplyr::rename(Long = X, Lat = Y)

  ###ntcd file
  nt2 = ncdf4::nc_open(ncfile)
  lat = ncdf4::ncvar_get(nt2, "latitude") #nord/sud
  long = ncdf4::ncvar_get(nt2, "longitude") #est/ovest
  time2 = ncdf4::ncvar_get(nt2, "time")
  time2 = as.Date(time2/24, origin = "1900-01-01", tz = "UTC")
  
  timemon = time2 %>% lubridate::as_date() %>% lubridate::month(label = TRUE, abbr = FALSE)
  timey = time2 %>% lubridate::as_date() %>% lubridate::year()
  time_label = paste(timemon, timey, sep = "_")

  #precipitazioni totali
  all_measure = list()
  for(m in names(nt2$var)){
    
    for(k in 1:length(time_label)){ #lungo le date
      prec2 = data.frame(Codice_azienda = NULL, temp = NULL)
      tot_prec2 = ncdf4::ncvar_get(nt2, m, start = c(1,1,k), count = c(-1,-1, 1))
      for(i in coordinates$Codice_azienda){
        cod = coordinates %>% dplyr::filter(Codice_azienda == i) 
        anom = t(tot_prec2)
        colnames(anom) = round(long, 1)
        rownames(anom) = round(lat, 1)
        prec = anom[as.character(round(cod$Lat,1)),as.character(round(cod$Long,1))]
        tobind = data.frame(Codice_azienda = i, temp = prec)
        prec2 = rbind(prec2, tobind)
      }
      colnames(prec2) = c("Codice_azienda", time_label[k])
      if(k == 1){
        precfin = prec2
      }else{
        precfin = cbind(precfin, dplyr::select(prec2,2))
      }
    }
    all_measure[m] = list(precfin)
  }
  
  return(all_measure)
  
}