#' Make a tmap object that can be rendered in shiny
#' 
#' @description This function render a tmap using a shapefile (campania) as background and a table with coordinates for the points. 
#' 
#' @param pointcoord A dataset filled with two columns (UTM_33T_N and UTM_33T_E). Each row is a point with its UTM 33T cordinates.
#' @param datainfo A dataset with points informations. These informations will be shown on the map.
#' @param dotlegend An informative column used for the legend and for the mapping of the dots (e.g. "Polifenoli_totali" or "Azienda").
#' @param shp A shapefile used for the background in the UTM 33T coordinate system. By default shp = campania (a shapefile saved in .rda format).
#' 
#' 
#' 
#' @import tmap
#' @import tmaptools
#' @importFrom sp SpatialPointsDataFrame CRS
#' 
#' @examples \dontrun{
#' 
#' output$map1 = make_tmap(
#'  pointcoord = filtereddata(), 
#'  datainfo = datmap1(), 
#'  dotlegend = showcolumn2(), 
#'  shp = campania)
#' }
#' 
#' 
#' 


make_tmap = function(pointcoord, datainfo, dotlegend, shp = campania){
  
    utmcoord23 <- sp::SpatialPointsDataFrame(pointcoord, datainfo, proj4string = sp::CRS("+proj=utm +zone=33 +datum=WGS84"))
    
    tm_shape(shp)+ tm_polygons(col= "provincia") + tm_shape(utmcoord23) + 
      tm_dots(col = colnames(dotlegend), scale = 1.5, id= colnames(dotlegend), popup.vars = TRUE)

}
