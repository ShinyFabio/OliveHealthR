#' Download photos and copy in www folder
#' 
#' @description This function download photos from a link and copy them in the www folder. Data are uploaded on a CNR server.
#' 
#' @usage download_photo()
#' 
#' 
#' 
#' @param url Url where download the zip file. The default is "http://bioinfo.na.iac.cnr.it/olivehealth/foto_drupe_foglie_20_21.zip".
#' 
#' @export
#' 
#' 
#' 




download_photo = function(url = "http://bioinfo.na.iac.cnr.it/olivehealth/foto_drupe_foglie_20_21.zip"){
  
  temp = tempfile()
  destpath = base::system.file(package = "OliveHealthR")
  destpath = paste0(destpath, "/app/www")
  utils::download.file(url, temp, cacheOK = FALSE, mode = "wb")
  utils::unzip(zipfile = temp, exdir = destpath)
}

