#' Make a vector with the labels for plots with unit measure
#'
#'
#' @description This function take the colnames (e.g. from the \code{Olv_select_col()}), add an unit measure and substitute "_" with space. 
#' It's useful to make a better label for a plot.
#'
#'
#' @param data An "opposite dataframe". For example if I want to change the column name of "Polifenoli_totali" (from the datapoltot dataframe),
#' I will put in data parameter the opposite dataframe called datapolind containing the default columns (i.e. "Codice_azienda, "N_campionamento", ...),
#' but also the individual polyphenols. Doing this if "colname" has no corresponding column (for example "Polifenoli_totali" is not present
#' in datapolind, but "Codice_azienda" does), it will be modified. If not, the column names doesn't change. 
#' @param colname The column (e.g. from the \code{Olv_select_col()}) that has to be changed.
#' @param unit The unit measure that has to be printed next to the column names (e.g. \code{"(mg/g drupe)"}).
#' @param optional This parameter is empty by default. If you need to specify another column that has not to be modified put here the column
#' name. For example in datapoltot there are two column that has no corresponding column in the opposite dataframe (datapolind) i.e. "Polifenoli_totali"
#' and "Presenza_larve". If you add "Presenza_larve in this parameter, it won't be changed and won't be added the unit measure. 
#'
#' 
#' @examples \dontrun{
#' 
#'  #create a dataframe
#'  polifind = data.frame(
#'  Codice_azienda = c("SA_1", "SA_2", "AV_3", "NA_1", "BN_6"), 
#'  Acido_Gallico = c(600,550,172,37,109))
#'  
#'  #create the selected column 
#'  selectedcol = data.frame(Polifenoli_totali = c(100,400,321,67,99))
#'  
#'  #The following function will give the colname with the unit measure and 
#'  #with spaces instead of underscore.
#'  label_with_unit(data = polifind, colname = selectedcol, unit = "(mg)")
#'  
#'  Polifenoli totali (mg)
#'  
#'  #but for example if I use:
#'  
#'  selectedcol2 = data.frame(Presenza_larve = c(1,0,0,2,0))
#'  
#'  label_with_unit(data = polifind, colname = selectedcol2, unit = "(mg)")
#'  #it will print:
#'  
#'  Presenza larve (mg)
#'  
#'  #because "Presenza_larve" is still not present in "polifind" but it's a 
#'  #categorical value and so doesn't need a unit measure. To solve this problem
#'  #I can specificy the \code{optional} parameter:
#'  
#'  label_with_unit(data = polifind, colname = selectedcol2, unit = "(mg)", 
#'  optional = "Presenza_larve")
#'  
#'  Presenza larve
#' }
#'


label_with_unit = function(data, colname, unit, optional = ""){ 
  
  coln = colnames(colname)
  colt= colnames(data)
  if(coln %in% colt == FALSE && coln != optional){ 
    ylab2 = paste(coln, unit)
  } else { ylab2 = coln
  }
  ylab3 = gsub("_", " ", ylab2)
  return(ylab3)
}


