#' Make a Heatmap using ComplexHeatmap package.
#' 
#' @description This function render a Heatmap using ComplexHeatmap package.
#' 
#' @param datasorted A dataframe done with sorder_data() function. See in ?sorder_data() the "data2" parameter for help.
#' @param add_annot A column used for the annotation (e.g. "Provincia")
#' @param scale_data Scaling data option. Data can be scaled (using scale()) by row ("row"), by column ("column") or not scaled ("none"). The result is the z-score.
#' @param row_dend Option for dendogram by row (TRUE or FALSE).
#' @param col_dend Option for dendogram by column (TRUE or FALSE).
#' @param row_nclust Number of cluster in the row dendrogram (a number).
#' @param col_nclust Number of cluster in the column dendrogram (a number).
#' @param dist_method Choose a distance method (e.g "euclidean"). See stats::dist() help.
#' @param clust_method Choose a clustering method (e.g "ward.D"). See stats::hclust() help.
#' @param col_lab A label for the column (i.e. "Polifenoli" or "Misure").
#' @param unit_legend An unit label for the legend. If data are scaled will be "Z-score" otherwise will be the input.
#' @param year_presence Option for data without a year column. Default is TRUE (year column present).
#' @param col_label_size Size of the column labels. By default col_label_size = 13.2.
#'
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr select
#' @importFrom stats dist hclust as.dendrogram setNames
#' @importFrom dendextend color_branches
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap draw
#'
#' @examples \dontrun{
#' 
#' make_heatmap(
#' datasorted = dtheatsorted(), 
#' add_annot = input$selectannot, 
#' scale_data = input$selscaleheat, 
#' dist_method = input$seldistheatpol, 
#' clust_method = input$selhclustheatpol,
#' row_dend = input$rowdend, 
#' row_nclust = input$sliderrowheat,
#' col_dend = input$columndend, 
#' col_nclust = input$slidercolheat
#' )
#' 
#' 
#' }


#INPUT polifenoli
#dtheatsorted()           -datasorted
#input$selectannot        -add_annot
#input$selscaleheat       -scale_data

#input$seldistheatpol     -dist_method
#input$selhclustheatpol   -clust_method

#input$rowdend            -row_dend
#input$sliderrowheat      -row_nclust

#input$columndend         -col_dend
#input$slidercolheat      -col_nclust

make_heatmap = function(datasorted, add_annot, scale_data, row_dend, row_nclust, col_dend, col_nclust, dist_method, 
                        clust_method, col_lab, unit_legend, year_presence = TRUE, col_label_size = 13.2, bordi = c(2,2,2,15)){
  
    #creo la matrice con rownames
  if(year_presence == TRUE){
    temp = datasorted %>% dplyr::select(-Anno, -N_campionamento, -add_annot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
  }else{
    temp = datasorted %>% dplyr::select(-N_campionamento, -add_annot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
  }
    
    #scale none, row, column
    if(scale_data == "column"){
      temp = scale(temp) # scale and center columns
      #ora se ho una colonna con tutti 0, scale restituisce una colonna con tutti NaN. Qui sostituisco le colonne
      #con tutti NaN con tutti 0.
      for(i in seq(1:length(temp[1,]))){
        if(mean(is.na(temp[,i])) == 1){
          temp[,i] = 0
        }
      }
      unit_legend = "Z-score"
    } else if(scale_data == "row"){
      temp = t(scale(t(temp))) # scale and center rows
      unit_legend = "Z-score"
    } 
    
    #dendrogram = none', 'row', 'column' or 'both' 
    if(row_dend == TRUE){
      row_dend2 = temp %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
      row_dend2 = dendextend::color_branches(row_dend2, k = row_nclust)
      row_split = row_nclust
    } else {
      row_dend2 = FALSE
      row_split = NULL
    }
    
    if(col_dend == TRUE){
      col_dend2 = temp %>% t() %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
      col_dend2 = dendextend::color_branches(col_dend2, k = col_nclust)
      col_split = col_nclust
    } else {
      col_dend2 = FALSE
      col_split = NULL
    }
    
    
    annotdata = dplyr::select(datasorted, Codice_azienda, add_annot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    leng = annotdata %>% dplyr::select(add_annot) %>% table() %>% length()
    colorannot = stats::setNames(grDevices::rainbow(n = leng), c(row.names(table(annotdata))))
    colorannot = stats::setNames(list(colorannot), paste(add_annot))
    col_ha = ComplexHeatmap::HeatmapAnnotation(df = annotdata, which = "row", col = colorannot)
    
    
    ht = ComplexHeatmap::Heatmap(temp, name = unit_legend,  rect_gp = grid::gpar(col = "white", lwd = 1), row_title = "Codice azienda", 
                                 column_title = col_lab, row_names_gp = grid::gpar(fontsize = 10), column_names_gp = grid::gpar(fontsize = col_label_size),
                                 cluster_rows = row_dend2, cluster_columns = col_dend2, 
                                 left_annotation = col_ha,
                                 column_split = col_split, row_split = row_split)
    ht = ComplexHeatmap::draw(ht, padding = unit(bordi, "mm"))



}