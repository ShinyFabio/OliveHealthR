#' Sort and order a dataframe to be used for the heatmap
#'
#' @description Sort and/or order a dataframe to be used for the heatmap (will be the "datasorted" variable).
#' 
#' 
#' @param data The main dataframe with "Provincia" and other information columns (use data()).
#' @param data2 The second dataframe that has to be sorted and joined with data. (e.g. datapolind()). Data must has "Codice_azienda", "Azienda" "N_campionamento", and "Anno" and nothing more (NO Provincia or Cultivar or ID_foglie). So only those 3 char columns and all double columns (polifenols.
#' @param year Data will be filtered by "Anno" (e.g. "2020"). The default is NULL (data will not be filtered by year but only by n_camp).
#' @param n_camp Data will be filtered by "N_campionamento" (e.g. "R2").
#' @param heat_sort Option for ordering data (TRUE or FALSE).
#' @param add_annot A column used for the annotation (e.g. "Provincia").
#'
#'
#' @importFrom dplyr filter select inner_join
#'
#'
#' @examples \dontrun{
#' sorder_data(
#' data2 = datapolind(),
#' year = input$selyearheatind,
#' n_camp = input$numheat,
#' heat_sort = input$heatsort,
#' add_annot = input$selectannot
#' )
#' }
#'
#'



sorder_data = function(data , data2, year = NULL, n_camp, heat_sort, add_annot){
  #scegli anno e campionamento
  if(is.null(year)){
    dtfilterd = data2 %>% dplyr::filter(N_campionamento == n_camp)
  }else{
    dtfilterd = data2 %>% dplyr::filter(Anno == year) %>% dplyr::filter(N_campionamento == n_camp)
  }
 
  #ordinare
  seletannota = add_annot
  dati = dplyr::select(data, "Codice_azienda", seletannota)
  datipolif = dtfilterd %>% dplyr::select(-Azienda)
  htdata = dplyr::inner_join(datipolif, dati, by = "Codice_azienda")
  
  if(heat_sort == TRUE){
    htdata[do.call(order, htdata[as.character(seletannota[1])]), ]
  } else{
    return(htdata)
  }
  

}

