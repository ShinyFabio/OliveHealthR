#' Download photos and copy in www folder
#' 
#' @description This function download photos from a link and copy them in the www folder.
#' 
#' @usage download_photo()
#' 
#' 
#' 
#' @param url Url where download the zip file. By default "https://srv-store4.gofile.io/download/Vhtmor/9e840835a48fcd254263fdc03dbf690d".
#' @param destpath The destination path where to unzip the files. By default ...
#' @export
#' 
#' 
#' 




# download_photo = function(url = "https://srv-store4.gofile.io/download/Vhtmor/9e840835a48fcd254263fdc03dbf690d", destpath){
#   
#   url = paste("https://srv-store4.gofile.io/download/Vhtmor/9e840835a48fcd254263fdc03dbf690d", "imagedata.zip", sep = "/")
#   temp = tempfile()
#   download.file(url, temp)
#   unzip(zipfile = temp, exdir = "C:/Users/fabio/Desktop/www/foo")
# }

download_photo = function(){
  getwd()
}