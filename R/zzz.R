
.onAttach <- function(libname, pkgname){
  
  version = packageDescription(pkgname, fields = "Version")
  
  msg = paste0("========================================
", "Welcome to OliveHealthR
", pkgname, " version ", version, "

Github page: https://github.com/ShinyFabio/OliveHealthR2

If you want to download all the images use download_photo() before launch OliveHealthR.

This message can be suppressed by:
suppressPackageStartupMessages(library(OliveHealthR2))
========================================
               ")
  
  
  packageStartupMessage(msg)
  
}