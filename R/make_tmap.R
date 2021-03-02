#' Make a tmap object that can be rendered in shiny
#' 
#' @description This function render a tmap using a shapefile (campania) as background and a table with coordinates for the points. 
#' 
#' @param data A dataframe with the coordinates columns(UTM_33T_N and UTM_33T_E) and relatives infos.
#' @param dotlegend An informative column used for the legend and for the mapping of the dots (e.g. "Polifenoli_totali" or "Azienda").
#' @param shp A shapefile used for the background in the UTM 33T coordinate system. By default shp = campania (a shapefile saved in .rda format).
#' 
#' 
#' 
#' @import tmap
#' @import tmaptools
#' @importFrom sf st_as_sf st_crs
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


make_tmap = function(data, dotlegend, shp = campania){
  

  data2 = as.data.frame(data)
  utmcoord23 = sf::st_as_sf(data2, coords = c("UTM_33T_E", "UTM_33T_N" ), crs= 32633)
    
  
  sf::st_crs(shp) = 32633

  tm_shape(shp)+ tm_polygons(col= "provincia") + tm_shape(utmcoord23) + 
      tm_dots(col = colnames(dotlegend), scale = 1.5, id= colnames(dotlegend), popup.vars = TRUE)

}
