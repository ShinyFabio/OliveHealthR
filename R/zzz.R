
.onAttach <- function(libname, pkgname){
  
  version = packageDescription(pkgname, fields = "Version")
  
  msg = paste0("========================================
", "Welcome to OliveHealthR
", pkgname, " version ", version, "

Github page: https://github.com/ShinyFabio/OliveHealthR

If you want to download all the images use download_photo() before launch OliveHealthR.

If you need the data file (.csv) download from this link:
http://bioinfo.na.iac.cnr.it/olivehealth/File_csv.rar

This message can be suppressed by:
suppressPackageStartupMessages(library(OliveHealthR))
========================================
               ")
  
  
  packageStartupMessage(msg)
  
}