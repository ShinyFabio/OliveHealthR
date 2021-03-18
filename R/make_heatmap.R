#' Make a Heatmap using ComplexHeatmap package.
#' 
#' @description This function render a Heatmap using ComplexHeatmap package.
#' 
#' @param datasorted A dataframe done with sorder_data() function. See in ?sorder_data() the "data2" parameter for help.
#' @param add_annot A column used for the annotation (e.g. "Provincia")
#' @param scale_data Scaling data option. Data can be scaled by row ("row"), by column ("column") or not scaled ("none").  
#' @param row_dend Option for dendogram by row (TRUE or FALSE).
#' @param col_dend Option for dendogram by column (TRUE or FALSE).
#' @param row_nclust Number of cluster in the row dendrogram (a number).
#' @param col_nclust Number of cluster in the column dendrogram (a number).
#' @param dist_method Choose a distance method (e.g "euclidean"). See stats::dist() help.
#' @param clust_method Choose a clustering method (e.g "ward.D"). See stats::hclust() help.
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

make_heatmap = function(datasorted, add_annot, scale_data, row_dend, row_nclust, col_dend, col_nclust, dist_method, clust_method){
  
    #creo la matrice con rownames 
    temp = datasorted %>% dplyr::select(-Anno, - N_campionamento, -add_annot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    
    #scale none, row, column
    if(scale_data == "column"){
      temp = scale(temp) # scale and center columns
    } else if(scale_data == "row"){
      temp = t(scale(t(temp))) # scale and center rows
    } 
    
    #dendrogram = none', 'row', 'column' or 'both' 
    if(row_dend == TRUE){
      row_dend = temp %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
      row_dend = dendextend::color_branches(row_dend, k = row_nclust)
      row_split = row_nclust
    } else {
      row_dend = FALSE
      row_split = NULL
      
    }
    
    if(col_dend == TRUE){
      col_dend = temp %>% t() %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
      col_dend = dendextend::color_branches(col_dend, k = col_nclust)
      col_split = col_nclust
    } else {
      col_dend = FALSE
      col_split = NULL
    }
    
    
    annotdata = dplyr::select(datasorted, Codice_azienda, add_annot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    leng = annotdata %>% dplyr::select(add_annot) %>% table() %>% length()
    colorannot = stats::setNames(grDevices::rainbow(n = leng), c(row.names(table(annotdata))))
    colorannot = stats::setNames(list(colorannot), paste(add_annot))
    col_ha = ComplexHeatmap::HeatmapAnnotation(df = annotdata, which = "row", col = colorannot)
    
    
    ht = ComplexHeatmap::Heatmap(temp, name = "ug/ml",  rect_gp = grid::gpar(col = "white", lwd = 1), row_title = "Codice azienda", 
                                 column_title = "Polifenoli", row_names_gp = grid::gpar(fontsize = 11),
                                 cluster_rows = row_dend, cluster_columns = col_dend, 
                                 left_annotation = col_ha,
                                 column_split = col_split, row_split = row_split)
    ht = ComplexHeatmap::draw(ht)



}