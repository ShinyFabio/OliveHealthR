#' Download photos and copy in www folder
#' 
#' @description This function download photos from a link and copy them in the www folder. Data are uploaded on https://gofile.io/uploadFiles.
#' 
#' @usage download_photo()
#' 
#' 
#' 
#' @param url Url where download the zip file. By default "https://srv-store4.gofile.io/download/Vhtmor/9e840835a48fcd254263fdc03dbf690d".
#' 
#' @export
#' 
#' 
#' 




download_photo = function(url = "https://srv-store4.gofile.io/download/Vhtmor/9e840835a48fcd254263fdc03dbf690d/imagedata.zip"){
  
  #url = paste(url, "imagedata.zip", sep = "/")
  temp = tempfile()
  destpath = base::system.file(package = "OliveHealthR")
  destpath = paste0(destpath, "/app/www")
  utils::download.file(url, temp)
  utils::unzip(zipfile = temp, exdir = destpath)
}