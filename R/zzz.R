
.onAttach <- function(libname, pkgname){
  
  version = packageDescription(pkgname, fields = "Version")
  
  msg = paste0(
    "
   |----------------------------------------------------------------------------|
   |                     WELCOME TO ", pkgname, " (Ver ", version,")                    |
   |----------------------------------------------------------------------------|
   |      This package has been developed at IAC-CNR under the financial        |
   |    support of the Regione Campania project OliveHealth in partnership      |
   |                   with Aprol Campania and IRET-CNR.                        |
   |                                                                            |
   |                                                                            |
   |        If you want to download all the images use download_photo()         |
   |                      before launch OliveHealthR.                           |
   |                                                                            |
   |        If you need the data file (.xlsx) download from this link:          |
   |         https://bioinfo.na.iac.cnr.it/olivehealth/File_excel.rar           |
   |                                                                            |
   |                                                                            |
   |Github page: https://github.com/ShinyFabio/OliveHealthR                     |
   |----------------------------------------------------------------------------|
                                               * Package version ",version," loaded *"
  )

  packageStartupMessage(msg)
  
}