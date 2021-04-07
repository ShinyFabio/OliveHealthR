#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom plotly renderPlotly ggplotly layout
#' @rawNamespace import(ggplot2, except = last_plot)
#' @rawNamespace import(stats, except = filter)
#' @import tmap
#' @import tmaptools
#' @import ggfortify
#' @import htmltools
#' @import scales
#' @importFrom dplyr select inner_join mutate filter rename across group_by summarise left_join ungroup n semi_join
#' @importFrom tidyr unite starts_with separate
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom grid gpar
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom lubridate year
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapWidget
#' @importFrom readr read_delim locale parse_factor
#' @importFrom ComplexHeatmap Heatmap HeatmapAnnotation draw
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom DT renderDT datatable formatRound
#' @importFrom grDevices rainbow
#' @importFrom dendextend color_branches
#' @importFrom sf st_as_sf st_crs
#' @importFrom VIM aggr
#' @importFrom stringr str_replace_all
#' @importFrom factoextra fviz_nbclust fviz_cluster fviz_dend fviz_silhouette eclust
#' @importFrom cluster pam clara
#' @importFrom gridExtra grid.arrange
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

  
  
  ############# Upload file ###############
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "navb1",
                      selected = "panel2")
  })
  
  #codice per far funzionare il conditional panel legato al "file1".
  output$file1_ready <- reactive({
    return(!is.null(input$file1))
  })
  outputOptions(output, "file1_ready", suspendWhenHidden = FALSE)    # activate the output so it runs although not displayed
  
  
  
  
  #nascondi prima il tab descrizione per poi visualizzarlo quando carico il file descrizione.csv
  hideTab(inputId = "tab2", target = "Descrizione")
  observeEvent(input$desc1, {
    showTab(inputId = "tab2", target = "Descrizione")
  })
  
  
  
  #carica il file come .csv
  data = reactive({
    req(input$file1)
    readr::read_delim(input$file1$datapath, delim = input$delim, col_names = input$header, na = "", local = readr::locale(encoding = "windows-1252")) 
  })
  
  
  
  #carico il file descrizione csv
  descri2 = reactive({
    req(input$desc1)
    req(data())
    temp = readr::read_delim(input$desc1$datapath, delim = input$delim, col_names = input$header, local = readr::locale(encoding = "windows-1252"))
    temp2 = data() %>% dplyr::select(Codice_azienda, Azienda)
    dplyr::inner_join(x = temp, y = temp2, by = "Codice_azienda")
  })
  
  
  
  #carica il file drupe come .csv
  drupe = reactive({
    req(input$drupeinput)
    x = readr::read_delim(input$drupeinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(date_format = "%d/%m/%Y", encoding = "windows-1252"))
    x$Indice_maturazione = factor(x$Indice_maturazione, levels = c(0:8), ordered = TRUE)
    x$Fase_fenologica = factor(x$Fase_fenologica, levels = c(51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89), ordered = TRUE)
    return(x)
  })
  
  oliocamp = reactive({
    req(input$olioinput)
    readr::read_delim(input$olioinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(date_format = "%d/%m/%Y", encoding = "windows-1252"))
    
  })
  
  
  
  #carico il file polifenoli come .csv
  polif = reactive({
    req(input$polifinput)
    x = readr::read_delim(input$polifinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = ",", encoding = "windows-1252"))
    x$Presenza_larve = readr::parse_factor(as.character(x$Presenza_larve), levels = c("0","1","2"), ordered = TRUE)
    return(x)
  })
  
  
  #carico i file morfometria
  
  #file foglie
  morfoleaf = reactive({
    req(input$morfoleafinput)
    readr::read_delim(input$morfoleafinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = ",", encoding = "windows-1252"))
  })
  
  #file morfometria drupe
  morfodrupe = reactive({
    req(input$morfodrupeinput)
    readr::read_delim(input$morfodrupeinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = ",", encoding = "windows-1252"))
  })
  
  #file morfometria endocarpo
  morfoendo = reactive({
    req(input$morfoendoinput)
    readr::read_delim(input$morfoendoinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = ",", encoding = "windows-1252"))
  })
  
  #file rapporti drupe endocarpo
  morforatio = reactive({
    req(input$morforatioinput)
    readr::read_delim(input$morforatioinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = ",", encoding = "windows-1252"))
  })
  
  
  output$content = DT::renderDT({
    data()
  })
  
  #####DESCRIZIONE####
  
  #join con data per aggiungere la colonna "Azienda"
  
  output$descriz = DT::renderDT({
    req(descri2())
    dplyr::select(descri2(), "Azienda")
    }, selection = "single", server = FALSE, rownames = FALSE)

  ####selezionare la riga dell'azienda    
  y11 = reactive({
    nroww=input$descriz_rows_selected
    descri2()[nroww,]
  })
  #e stamparla
  output$taby12 = renderUI({
    HTML(paste(y11()[,2]))
  })
  #stampo anche i contatti
  output$contatti = renderUI({
    HTML(paste("<b>",y11()[,3],"</b>"))
  })
  
  

  
  ############## Cultivar principale #################
  output$numcult = renderText({
    cult = data() %>% dplyr::select(Cultivar_principale) %>% table() %>% length()
    HTML(paste("Nel dataset sono presenti", "<b>", cult, "</b>", "cultivar ripartite secondo il seguente grafico:"))
  })
  
  #####grafico cultivar con scelta del tipo di grafico   
  output$cultplot = renderUI({
    if(input$selplotcult == 1){
      output$pie1 = plotly::renderPlotly({
        data() %>% plotly::plot_ly(labels = ~Cultivar_principale, type= "pie", textposition = 'inside', textinfo = 'label+value',
                           marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)), 
                           showlegend = FALSE) %>% plotly::layout(title = "Presenza delle varie cultivar sul territorio")
      })
      plotlyOutput("pie1")
    } else {
      output$bar1 = plotly::renderPlotly({
        dd = ggplot(data=data()) + geom_bar(mapping = aes(x = Cultivar_principale, fill = Cultivar_principale)) + 
          scale_y_continuous(breaks = scales::pretty_breaks()) + ggtitle("Presenza delle varie cultivar sul territorio") + 
          xlab("Cultivar") + ylab("Conta") + 
          theme(axis.title = element_text(face="bold", size = 13), axis.text.x = element_text(angle = 315, hjust = 0, size = 11)) + 
          scale_fill_manual(values = c("#d62728","#2ca02c", "#ff7f0e", "#1f77b4", "#e77c7c", "#5fd35f", "#ffb574", "#57a9e2", "#17becf", "#bcbd22", "#7f7f7f", "#e377c2", "#8c564b", "#9467bd" ))
        
      })
      plotly::plotlyOutput("bar1")
    }
  })
  
  
  ##### Mappa Aziende ####
  
  

  #creo datmap1 senza le colonne UTM
  datmap1 = reactive({data() %>% dplyr::select(!starts_with("UTM"))
  })
  

  observeEvent(data(), {
    updateSelectInput(session, "select3", choices = colnames(datmap1()))
  })
  
  
  ###stampa mappa
  output$map1 = renderTmap({
    req(data())
    colmap = Olv_select_col(data = datmap1(), input = input$select3)
    make_tmap(data = data(), dotlegend = colmap)
  })

  
  ############ SCHEDE CAMPIONAMENTO FOGLIE E DRUPE ###############
  
  output$tabledrupscheda = renderDT({
    req(drupe())
    drupe()
  })
  
  
  
  #fare il join di data con le drupe
  datadrupemap = reactive({
    req(drupe())
    dplyr::inner_join(x = data(), y = drupe(), by = "Codice_azienda")
  })
  
  #rimuovere colonne coordinate
  datadrupe = reactive({
    req(datadrupemap())
    datadrupemap() %>% dplyr::select(!starts_with("UTM"))
  })
  

  ##########  Grafici datadrupe  #########
  

  #selezionare colonna X da plottare
  showcolumnx = reactive({
    Olv_select_col(data = datadrupe(), input = input$selectx)
  })  
  
  observeEvent(datadrupe(), {
    updateSelectInput(session, "selectx", choices=colnames(datadrupe()))
    updateSelectInput(session, "selecty", choices=colnames(datadrupe()))
    updateSelectInput(session, "selectfill", choices=colnames(datadrupe()))
  })
  
  ###selezionare colonna Y da plottare
  showcolumny = reactive({
    Olv_select_col(data = datadrupe(), input = input$selecty)
  }) 
  
  ###selezionare colonna per il riempimento
  fillcolumn = reactive({
    Olv_select_col(data = datadrupe(), input = input$selectfill)
  }) 
  
  
  #aggiorna il selectinput , "selyearscatter" in base agli anni presenti e filtra
  observeEvent(dtdrupanno(), {
    updateSelectInput(session, "selyearscatter", choices = row.names(table(dplyr::select(dtdrupanno(), "Anno"))))
  })
  dtplotyear2 = reactive({
    dtdrupanno() %>% dplyr::filter(Anno == input$selyearscatter)
  })
  
  
  ###scegliere anche il campionamento (scatter plot)
  dtdrupfilt2 = reactive({
    req(dtplotyear2())
    dplyr::filter(dtplotyear2(), N_campionamento == input$num2)
  })
  
  ###grafico classico (scatter plot)   , position = "jitter" , alpha = 0.7
  output$plotxy = plotly::renderPlotly({
    temp = ggplot(data = dtdrupfilt2()) +
      geom_count(mapping = aes_string(x = colnames(showcolumnx()), y = colnames(showcolumny()), colour = colnames(fillcolumn()))) +
      scale_size_continuous(range = c(3,9)) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fillcolumn()))))
  })
  
  
  
  ## Barplot 
  ###modificare la colonna campionamento con unite (R1_2020)
  dtnumyunite = reactive({
    req(datadrupe())
    datadrupe() %>% dplyr::mutate(Anno = lubridate::year(Data_campionamento)) %>% 
      tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = TRUE)
    
  })
  
  ###salvare quanti campionamenti ci sono nei file
  numerocamp = reactive({
    req(dtnumyunite())
    dtnumyunite() %>% dplyr::select(N_campionamento) %>% table() %>% row.names()
  })
  
  ###aggiornare il checkbox in base al numero dei campionamenti
  observeEvent(numerocamp(), {
    updateCheckboxGroupInput(session, "checkcamp", choices = numerocamp(),  selected = numerocamp())
  })
  
  
  ###filtrare in base al numero di campionamento per colorare il barplot
  colorcamp = reactive({
    req(dtnumyunite())
    dtnumyunite() %>% dplyr::filter(N_campionamento %in% input$checkcamp)
  })
  
  
  
  ###grafico a barre
  output$barplot1 = plotly::renderPlotly({
    temp2=ggplot(data=colorcamp()) + 
      geom_col(mapping = aes_string(x = colnames(showcolumnx()), y = colnames(showcolumny()), fill = "N_campionamento"), position = position_dodge2(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank())
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })
  
  
  
  
  #### mappa (datadrupe)##
  
  observeEvent(drupe(), {
    updateSelectInput(session, "select2map", choices = colnames(drupe()))
  })
  
  #crea la colonna anno 
  dtdrupanno = reactive({
    req(datadrupemap())
    datadrupemap() %>% dplyr::mutate(Anno = lubridate::year(Data_campionamento)) #cambiato datadrupe
  })
  
  
  #aggiorna il selectinput "selyear" in base agli anni presenti
  observeEvent(dtdrupanno(), {
    updateSelectInput(session, "selyear", choices = row.names(table(dplyr::select(dtdrupanno(), "Anno"))))
  })
  

  #stampo mappa2
  output$map2 = renderTmap({
    req(dtdrupanno())
    #filtra in base all'anno selezionato e il campionamento
    datamap = dtdrupanno() %>% dplyr::filter(Anno == input$selyear) %>% dplyr::filter(N_campionamento == input$num)
    colmap = Olv_select_col(data = drupe(), input = input$select2map)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  
  
  #### seconda mappa
  

  observeEvent(drupe(), {
    updateSelectInput(session, "select3map", choices = colnames(drupe()))
  })
  
  #aggiorna il selectinput "selyear" in base agli anni presenti
  observeEvent(dtdrupanno(), {
    updateSelectInput(session, "selyear2", choices = row.names(table(dplyr::select(dtdrupanno(), "Anno"))))
  })
  

  output$map3 = renderTmap({
    req(drupe())
    req(dtdrupanno())
    datamap = dtdrupanno() %>% dplyr::filter(Anno == input$selyear2) %>% dplyr::filter(N_campionamento == input$num2map)
    colmap = Olv_select_col(data = drupe(), input = input$select3map)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  

  
  
  ################# Foto campioni ############
  
  
  #crea la tabella
  output$prov2 = DT::renderDT(dplyr::select(data(), c("Azienda", "Codice_azienda")), selection = "single", server = FALSE, rownames = FALSE)
  

  #aggiorna il selectinput "selyearfoto" in base agli anni presenti e seleziono
  observeEvent(dtdrupanno(), {
    updateSelectInput(session, "selyearfoto", choices = row.names(table(dplyr::select(dtdrupanno(), "Anno"))))
  })
  
  
  #####selezionare la riga dell'azienda    
  selprov = reactive({
    req(input$prov2_rows_selected)
    nroww=input$prov2_rows_selected
    x= dplyr::select(data(), "Codice_azienda")
    y= dplyr::select(data(), "Provincia")
    z = paste("www/foto_drupe_foglie", input$selyearfoto, input$campfoto, sep = "/")
    paste(z, y[nroww,], x[nroww,], sep = "/")
  })
  
  
  #foto foglie
  output$phfoglia = renderUI({
    x = paste(selprov(), "foglie", sep="_")
    foglia = paste0(x,".jpg")
    tags$img(src = foglia, width = "75%", height = "75%")
  })
  
  #foto drupe
  output$phdrupa = renderUI({
    x = paste(selprov(), "drupe", sep="_")
    drupa = paste0(x, ".jpg")
    tags$img(src = drupa, width = "75%", height = "75%")
  })
  
  
  ############# SCHEDE CAMPIONAMENTO OLIO ##############
  
  output$tableolioscheda = renderDT({
    req(oliocamp())
    oliocamp()
  })
  
  
  ##########################POLIFENOLI##############################################
  
  #polif
  #fare il join di data con i polifenoli
  datapolifmap = reactive({
    req(polif())
    z = data() %>% dplyr::select("Codice_azienda", "Cultivar_principale", "Azienda", "UTM_33T_E", "UTM_33T_N")
    x = dplyr::inner_join(x = z, y = polif(), by = "Codice_azienda")
    u1 = within(x, levels(Presenza_larve)[levels(Presenza_larve) == "0"] <- "Non individuabili") 
    u2 = within(u1, levels(Presenza_larve)[levels(Presenza_larve) == "1"] <- "Poche larve")
    u3 = within(u2, levels(Presenza_larve)[levels(Presenza_larve) == "2"] <- "Molte larve")
    return(u3)
  })
  
  datapolif = reactive({
    req(datapolifmap())
    datapolifmap() %>% dplyr::select(!starts_with("UTM"))
  })
  
  #######polifenoli totali#####
  
  #data polifenoli totali
  datapoltot= reactive({
    dplyr::select(datapolif(), c("Codice_azienda", "Azienda", "Anno", "N_campionamento", "Cultivar_principale", "Polifenoli_tot", "Presenza_larve"))
  })
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatapoltot = reactive({
    req(datapoltot())
    data = datapoltot() %>% dplyr::select(where(is.double) & -Anno)
    return(data)
  })
  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxpoltot", data = nadatapoltot)
  
  #crea tabella polifenoli totali
  output$tablepoltot = DT::renderDT({
    req(datapoltot())
    temp = datapoltot() %>% dplyr::rename("Polifenoli_tot_(mg/g)" = "Polifenoli_tot")
    DT::datatable(temp, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      DT::formatRound(columns = "Polifenoli_tot_(mg/g)", digits = 2) 
    
  })
  

  
  #selezionare colonna X da plottare
  showcoltotx = reactive({
    Olv_select_col(data = datapoltot(), input = input$selectxtot)
  })  
  
  observeEvent(datapoltot(), {
    updateSelectInput(session, "selectxtot", choices=colnames(datapoltot()))
    updateSelectInput(session, "selectfilltot", choices=colnames(datapoltot()))
    updateSelectInput(session, "selectytot", choices=colnames(datapoltot()))
    updateSelectInput(session, "selyearscattertot", choices = row.names(table(dplyr::select(datapoltot(), "Anno"))))
    updateSelectInput(session, "selectxtotbar", choices=colnames(datapoltot()))
    updateSelectInput(session, "selectytotbar", choices=colnames(datapoltot()))
  })
  
  ###selezionare colonna Y da plottare

  showcoltoty = reactive({
    Olv_select_col(data = datapoltot(), input = input$selectytot)
  })  

  
  ###selezionare colonna per il riempimento
  fillcolumntot = reactive({
    Olv_select_col(data = datapoltot(), input = input$selectfilltot)
  }) 

  
  #aggiorna il selectinput , "selyearscattertot" in base agli anni presenti e filtra
  dtplotyeartot2 = reactive({
    datapoltot() %>% dplyr::filter(Anno == input$selyearscattertot)
  })
  
  
  ###scegliere anche il campionamento (scatter plot)
  dtdrupfilttot2 = reactive({
    req(dtplotyeartot2())
    dplyr::filter(dtplotyeartot2(), N_campionamento == input$numtot)
  })
  
  
  xlabtot = reactive({
    label_with_unit(data = datapolind(), colname = showcoltotx(), unit = "(mg/g drupe)", optional = "Presenza_larve")
  })

  ylabtot = reactive({
    label_with_unit(data = datapolind(), colname = showcoltoty(), unit = "(mg/g drupe)", optional = "Presenza_larve")
  })
  
  filllabtot = reactive({
    label_with_unit(data = datapolind(), colname = fillcolumntot(), unit = "(mg/g drupe)", optional = "Presenza_larve")
  })
  

  
  ###grafico classico (scatter plot)   , position = "jitter" , alpha = 0.7
  output$totscatplot = plotly::renderPlotly({
    temp = ggplot(data = dtdrupfilttot2()) + 
      geom_count(mapping = aes_string(x = colnames(showcoltotx()), y = colnames(showcoltoty()), colour = colnames(fillcolumntot()))) + 
      ylab(ylabtot()) + xlab(xlabtot()) + labs(colour=filllabtot()) + #scale_size_continuous(range = c(3,9)) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = filllabtot())))
  })
  
  
  #########BOXPLOT POLIFENOLI TOTALI#####################
  
  ###modificare la colonna campionamento con unite (R1_2020)
  datapoltotyearunite = reactive({
    req(datapoltot())
    datapoltot() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = TRUE)
    
  })
  
  ###salvare quanti campionamenti ci sono nei file
  numerocamptot = reactive({
    req(datapoltotyearunite())
    datapoltotyearunite() %>% dplyr::select(N_campionamento) %>% table() %>% row.names()
  })
  
  ###aggiornare il checkbox in base al numero dei campionamenti
  observeEvent(numerocamptot(), {
    updateCheckboxGroupInput(session, "checkcamptot", choices = numerocamptot(),  selected = numerocamptot())
  })
  
  
  ###filtrare in base al numero di campionamento per colorare il barplot
  colorcamptot = reactive({
    req(datapoltotyearunite())
    datapoltotyearunite() %>% dplyr::filter(N_campionamento %in% input$checkcamptot)
  })
  
  
  
  ###grafico a barre
  output$barplottot = plotly::renderPlotly({
    
    x = Olv_select_col(data = datapoltot(), input = input$selectxtotbar)
    y = Olv_select_col(data = datapoltot(), input = input$selectytotbar)
    
    xlabg = label_with_unit(data = datapoltot(), colname = x, unit = "(µg/g)")
    ylabg = label_with_unit(data = datapoltot(), colname = y, unit = "(µg/g)")
    
    
    temp2=ggplot(data=colorcamptot()) + 
      geom_col(mapping = aes_string(x = colnames(x), y = colnames(y), fill = input$selectfilltotbar), position = position_dodge2(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(ylabtot()) + xlab(xlabtot())
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })
  
  
  
  
  ##################################POLIFENOLI INDIVIDUALI##############################
  
  
  #data polifenoli individuali
  datapolind = reactive({
    dplyr::select(datapolif(), !c("Polifenoli_tot", "Presenza_larve"))
  })
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatapolind = reactive({
    req(datapolind())
    data = datapolind() %>% dplyr::select(where(is.double) & -Anno)
    return(data)
  })
  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxpolind", data = nadatapolind)
  
  
  #crea tabella polifenoli individuali con l'unità di misura
  output$tablepolind = DT::renderDT({ 
    req(datapolind())
    temp = datapolind()
    for(i in seq(6,length(temp))){
      names(temp)[i] = paste0(names(temp)[i], "_(ug/g)")
    }
    return(temp)
    
  })
  
  
  ###################SCATTER PLOT INDIVIDUALI#################

  observeEvent(datapolind(), {
    updateSelectInput(session, "selectxind", choices=colnames(datapolind()))
    updateSelectInput(session, "selectyind", choices=colnames(datapolind()))
    updateSelectInput(session, "selectfillind", choices=colnames(datapolind()))
    updateSelectInput(session, "selyearscatterind", choices = row.names(table(dplyr::select(datapolind(), "Anno"))))
  })
  

  #aggiorna il selectinput , "selyearscatterind" in base agli anni presenti e filtra
  dtplotyearind2 = reactive({
    datapolind() %>% dplyr::filter(Anno == input$selyearscatterind)
  })
  
  
  ###scegliere anche il campionamento (scatter plot)
  dtdrupfiltind2 = reactive({
    req(dtplotyearind2())
    dplyr::filter(dtplotyearind2(), N_campionamento == input$numind)
  })
  
 

  ####grafico classico (scatter plot)   , position = "jitter" , alpha = 0.7
  output$scatterindpol = plotly::renderPlotly({
    
    x = Olv_select_col(data = datapolind(), input = input$selectxind)
    y = Olv_select_col(data = datapolind(), input = input$selectyind)
    fill = Olv_select_col(data = datapolind(), input = input$selectfillind)
    
    xlabg = label_with_unit(data = datapoltot(), colname = x, unit = "(µg/g)")
    ylabg = label_with_unit(data = datapoltot(), colname = y, unit = "(µg/g)")
    filllab = label_with_unit(data = datapoltot(), colname = fill, unit = "(µg/g)")

    temp = ggplot(data = dtdrupfiltind2()) + 
      geom_count(mapping = aes_string(x = colnames(x), y = colnames(y), colour = colnames(fill))) + 
      ylab(ylabg) + xlab(xlabg) + labs(colour=filllab) +#scale_size_continuous(range = c(3,9)) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = filllab)))
  })
  
  
  ###############BARPLOT INDIVIDUALI##########
  
  observeEvent(datapolind(), {
    updateSelectInput(session, "selectxindbar", choices=colnames(datapolind()))
    updateSelectInput(session, "selectyindbar", choices=colnames(datapolind()))
  })
  

  ###modificare la colonna campionamento con unite (R1_2020)
  datapolindyearunite = reactive({
    req(datapolind())
    datapolind() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = FALSE)
    
  })
  
  ###salvare quanti campionamenti ci sono nei file
  numerocampind = reactive({
    req(datapolindyearunite())
    datapolindyearunite() %>% dplyr::select(N_campionamento) %>% table() %>% row.names()
  })
  
  ###aggiornare il checkbox in base al numero dei campionamenti
  observeEvent(numerocampind(), {
    updateCheckboxGroupInput(session, "checkcampind", choices = numerocampind(),  selected = numerocampind())
  })
  
  
  ###filtrare in base al numero di campionamento per colorare il barplot
  colorcampind = reactive({
    req(datapolindyearunite())
    datapolindyearunite() %>% dplyr::filter(N_campionamento %in% input$checkcampind)
  })
  


  ###grafico a barre
  output$barplotind = plotly::renderPlotly({
    req(datapolind())
    x = Olv_select_col(data = datapolind(), input = input$selectxindbar)
    y = Olv_select_col(data = datapolind(), input = input$selectyindbar)

    xlabg = label_with_unit(data = datapoltot(), colname = x, unit = "(µg/g)")
    ylabg = label_with_unit(data = datapoltot(), colname = y, unit = "(µg/g)")

    temp2=ggplot(data=colorcampind()) + 
      geom_col(mapping = aes_string(x = colnames(x), y = colnames(y), fill = input$selectfillindbar), position = position_dodge2(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(ylabg) + xlab(xlabg)
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })
  
  
  
  ################# HEATMAP POLIFENOLI INDIVIDUALI ##################
  
  
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti
  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearheatind", choices = row.names(table(dplyr::select(datapolind(), "Anno"))))
  })


  
  dtheatsorted = reactive({
    sorder_data(
      data = data(),
      data2 = dplyr::select(datapolind(), -Cultivar_principale), #devo toglierlo perchè l'ho aggiunto prima
      year = input$selyearheatind, 
      n_camp = input$numheat, 
      heat_sort = input$heatsort, 
      add_annot = input$selectannot)
  })
  

  
  #creo slider per colonna
  output$sliderheatcol <- renderUI({
    req(dtheatsorted())
    len = dtheatsorted() %>% dplyr::select(-Anno, - N_campionamento,  -Codice_azienda, -Cultivar_principale, -input$selectannot)
    sliderInput("slidercolheat", "Numero cluster:", min=2, max=length(len), value=2, step = 1)
  })
  
  
  #creo l'heatmap
  dataheat = reactive({
    make_heatmap(
      datasorted = dtheatsorted(),
      add_annot = input$selectannot,
      scale_data = input$selscaleheat,
      dist_method = input$seldistheatpol,
      clust_method = input$selhclustheatpol,
      row_dend = input$rowdend,
      row_nclust = input$sliderrowheat,
      col_dend = input$columndend,
      col_nclust = input$slidercolheat,
      col_lab = "Polifenoli",
      unit_legend = "ug/g"
    )
  })
  

  observeEvent(input$updateheat,{
    dataheat2 = dataheat()
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, dataheat2, output_id = "heatmap_output", layout = "1|23", width1 = 650, height1 = 430)
  })
  
  
  ###################### CORRELATION PLOT POLIFENOLI ####################################
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearcorrind", choices = row.names(table(dplyr::select(datapolind(), "Anno"))))
  })

  
  ###creo il corrplot
  output$corrplotind = plotly::renderPlotly({
    req(datapolind())
    ###scegliere anno e  il campionamento (scatter plot)
    datatemp = datapolind() %>% dplyr::filter(Anno == input$selyearcorrind) %>% dplyr::filter(N_campionamento == input$numcorr)
    
    temp = datatemp %>% dplyr::select(-Anno, - N_campionamento, -Azienda, -Codice_azienda, -Cultivar_principale)
    temp2 = round(stats::cor(temp, use = "na.or.complete"),1)
    par(xpd = TRUE)
    
    plot = ggcorrplot::ggcorrplot(temp2, hc.order = TRUE, type = "lower", outline.col = "white", show.diag = TRUE)
    plotly::ggplotly(plot)
   
  })
  
  
  
  
  ######################### PCA #####################################################
  
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearpca", choices = row.names(table(dplyr::select(datapolind(), Anno))))
  })

  #rimuovo le righe di NA se ci sono. pcadatina mi servirà dopo per il semi_join con data().
  pcadatina = reactive({
    req(datapolind())
    #filtro in base agli anni presenti e scelgo anche il num campionamento
    datapolind() %>% dplyr::filter(Anno == input$selyearpca) %>% dplyr::filter(N_campionamento == input$numpca) %>% 
      dplyr::select(-Anno, -N_campionamento, -Azienda, -Cultivar_principale) %>% stats::na.exclude()
  })
  
  pcadati = reactive({
    data = pcadatina() %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    stats::princomp(data, cor = input$selcorpca)
  })
  
  #slider dinamico per la scelta delle pcs
  output$sliderpc <- renderUI({
    req(pcadati())
    pca = pcadati()
    sliderInput("selpcs", "Numero di Componenti Principali (PC)", min=1, max=length(pca$sdev), value=2, step = 1)
  })
  
  
  
  ###plot loadings
  output$loadings = plotly::renderPlotly({
    req(pcadati())
    pca = pcadati()
    loadpca = as.data.frame(pca$loadings[, input$selpcs])
    loadpca = tibble::rownames_to_column(loadpca)
    
    pcasdev = as.data.frame(round(pca$sdev^2/sum(pca$sdev^2)*100, 2))
    
    colnames(loadpca) = c("Polifenoli", paste0("PC", input$selpcs))
    loadplot = ggplot(loadpca) + geom_col(aes(x = Polifenoli, y = loadpca[,2], fill = Polifenoli)) +
      labs(y = paste0("PC", input$selpcs, " ", "(", pcasdev[as.numeric(input$selpcs), ], "%", ")"), title = "Loadings")
    plotly::ggplotly(loadplot)
  })
  
  ###screeplot
  output$screeplot <- plotly::renderPlotly({
    pca = pcadati()
    var = cumsum(100*pca$sdev^2/sum(pca$sdev^2)) 
    var = as.data.frame(cbind(var)) %>% tibble::rownames_to_column()
    colnames(var) = c("Componenti_principali", "Varianza_spiegata")
    
    screegg = ggplot(var, aes(Componenti_principali,Varianza_spiegata)) +
      geom_line(colour = "red", group = 1, linetype = "dashed", size = 1) + geom_point(size = 4, colour = "red") + 
      labs(x = "Componenti principali", y = "Varianza spiegata (%)", title = "Screeplot") +
      scale_y_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10)))
    plotly::ggplotly(screegg)
    
  })
  
  
  ###biplot
  output$biplot = plotly::renderPlotly({
    req(pcadati())
    #qui con semi_join mi prendo solo le righe di data() presenti anche in pcadatina().
    temp = autoplot(pcadati(), data = dplyr::semi_join(data(), pcadatina()), shape = input$shpbiplot, colour = input$colbiplot, loadings = TRUE, loadings.colour = 'blue', 
                    loadings.label = TRUE, loadings.label.size = 4, title = "Screeplot")
    plotly::ggplotly(temp)
  })
  
  
  ### plot 3D
  output$pca3dpolind = plotly::renderPlotly({
    req(pcadati())
    pc = pcadati()
    scoreind = pc$scores
    scoreindtb = scoreind %>% as.data.frame() %>% tibble::rownames_to_column("Codice_azienda") %>% tibble::as_tibble()
    scoreindjoin = dplyr::left_join(x = scoreindtb, y = dplyr::select(data(), Codice_azienda, Provincia, Cultivar_principale, Areale), by = "Codice_azienda")

    scoreindjoin %>% plotly::plot_ly(x = ~Comp.1, y = ~Comp.2, z= ~Comp.3, type = "scatter3d", mode = "markers", color = ~base::get(input$col3dind))
    
  })
  

  
  #################### MAPPA POLIFENOLI ###########################################
  

  observeEvent(datapolif(), {
    updateSelectInput(session, "mapxpol", choices = colnames(datapolif()))
    updateSelectInput(session, "selyearpol", choices = row.names(table(dplyr::select(datapolif(), "Anno"))))
  })
  
  
  #stampa mappa
  output$mappol = renderTmap({
    req(datapolif())
    req(datapolifmap())
    column = Olv_select_col(data = datapolif(), input = input$mapxpol)
    datamap = datapolifmap() %>% dplyr::filter(Anno == input$selyearpol) %>% dplyr::filter(N_campionamento == input$numpol)
    make_tmap(data = datamap, dotlegend = column)
  })
  
  
  
  ############# AGGIUNGERE SECONDA MAPPA POLIFENOLI########
  

  observeEvent(datapolif(), {
    updateSelectInput(session, "mapxpol2", choices = colnames(datapolif()))
    updateSelectInput(session, "selyearpol2", choices = row.names(table(dplyr::select(datapolif(), "Anno"))))
  })
  
  
  #stampa mappa 2
  output$mappol2 = renderTmap({
    req(datapolif())
    req(datapolifmap())
    datamap = datapolifmap() %>% dplyr::filter(Anno == input$selyearpol2) %>% dplyr::filter(N_campionamento == input$numpol2)
    column = Olv_select_col(data = datapolif(), input = input$mapxpol2)
    make_tmap(data =  datamap, dotlegend = column)
  })
  
  
  
  
  #################### MORFOMETRIA ############################
  
 
  
  #scegli i dati in base alla selezione
  
  datamorfo = reactive({
    req(data())
    if(input$selfilemorfo == "foglie"){
      tempdata = morfoleaf()
    } else if(input$selfilemorfo == "drupe"){
      tempdata = morfodrupe()
    } else if(input$selfilemorfo == "endocarpo"){
      tempdata = morfoendo()
    } else{
      tempdata = morforatio()
    }
    
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale)
    x = dplyr::left_join(x = z, y = tempdata, by = "Codice_azienda")
    return(x)
  })
  
  #valutare i missing value
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatamorfo = reactive({
    req(datamorfo())
    data = datamorfo() %>% dplyr::select(where(is.double) & -Anno)
      data = data %>% dplyr::select(!starts_with("ID"))
    return(data)
  })
  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxmorfo", data = nadatamorfo)
  

  
  nfoglieoliveold = reactive({
    if(input$selfilemorfo == "foglie"){
      x = "ID_foglia"
    } else{
      x = "ID_oliva"
    }
    return(x)
  })

  nfoglieolivenew =  reactive({
    if(input$selfilemorfo == "foglie"){
      x = "N_foglie"
    } else{
      x = "N_olive"
    }
    return(x)
  })
  
  datamorfomap = reactive({
    req(datamorfo())
    z = data() %>% dplyr::select("Codice_azienda", "UTM_33T_E", "UTM_33T_N")
    x = dplyr::inner_join(x = z, y = datamorfo(), by = "Codice_azienda")
    return(x)
  })
  
  output$dtmorfo = DT::renderDT({
    req(datamorfo())
    datadt = datamorfo() %>% dplyr::filter(Anno == input$selyeardtmorfo)
    if(input$summarizetab == TRUE){
      x = Olv_select_col(data = datadt, input = input$selectdtmorfo)
      dt = datadt %>% dplyr::group_by(dplyr::across(colnames(x))) %>% 
        dplyr::summarise(dplyr::across(where(is.double), ~round(mean(., na.rm = TRUE),input$selroundmorfo)), n = dplyr::n()) %>% 
        dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% dplyr::select(!starts_with("ID"))
      # "!!paste() := n" permette di rinominare usando un vettore contenente il nome. n è il colnames vecchio.

    } else{
     dt = datadt %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = input$selroundmorfo))
    }
    return(dt)
  })
  
  
  ################# FOTO CAMPIONI ############
  
  #crea la tabella
  output$dtfotomorfo = DT::renderDT(dplyr::select(data(), c("Azienda", "Codice_azienda")), selection = "single", server = FALSE, rownames = FALSE)
  
  
  #aggiorna il selectinput "selyearfotomorfo" in base agli anni presenti e seleziono
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearfotomorfo", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
  })
  
  
  #####selezionare la riga dell'azienda    
  selprovmorfo = reactive({
    req(input$dtfotomorfo_rows_selected)
    nroww=input$dtfotomorfo_rows_selected
    x= dplyr::select(data(), "Codice_azienda")
    paste("www/morfometria", input$selyearfotomorfo, input$selfilemorfo, x[nroww,], sep = "/")
    #qui ho eliminato la parte del campionamento perchè la morfometria c'è solo su R2
  })
  
  
  #foto 
  output$phmorfo = renderUI({
    req(input$dtfotomorfo_rows_selected)
    nroww=input$dtfotomorfo_rows_selected
    cod = dplyr::select(data(), "Codice_azienda")
    path = paste("www/morfometria", input$selyearfotomorfo, input$selfilemorfo, sep = "/")
    #qui ho eliminato la parte del campionamento perchè la morfometria c'è solo su R2

    if(input$selfilemorfo == "foglie"){
      fotopath = paste(path, cod[nroww,], sep = "/")
      fotopath2 = paste0(fotopath, ".jpg")
      box(width=NULL, status = "primary", title = "Foto",align = "center", 
         tags$img(src = fotopath2, width = "75%", height = "75%")
      ) 
    } else if(input$selfilemorfo == "drupe"){
      pathfoto = paste(path, "foto", cod[nroww,], sep = "/")
      pathfoto2 = paste0(pathfoto, ".jpg")
      path3d = paste(path, "3D", cod[nroww,], sep = "/")
      path3d1 = paste0(path3d, "_ol1.jpg")
      path3d2 = paste0(path3d, "_ol2.jpg")
      path3d3 = paste0(path3d, "_ol3.jpg")
      
      fluidPage(
        fluidRow(
          column(6, 
            box(width=NULL, status = "primary", title = "Foto",align = "center", 
                tags$img(src = pathfoto2, width = "100%", height = "100%"))
          ),
          column(6,
            box(width=NULL, status = "primary", title = "Modelli 3D", align = "center",
                fluidRow(
                  h5("Drupa 1"),
                  tags$img(src = path3d1, width = "65%", height = "65%")
                ),
                fluidRow(
                  h5("Drupa 2"),
                  tags$img(src = path3d2, width = "65%", height = "65%")
                ),
                fluidRow(
                  h5("Drupa 3"),
                  tags$img(src = path3d3, width = "65%", height = "65%")
                )
                ))
            )
          )
      
 
    } else {
      strong(h3("NO DATA", align = "center"))
    }
    
  })
  

  observeEvent(datamorfo(), {
    #boxplot e barplot
    updateSelectInput(session, "selectymorfobb", choices=colnames(dplyr::select(datamorfo(), where(is.double) & -dplyr::any_of(c("Anno", "ID_oliva")))))

    #mappa 1
    updateSelectInput(session, "mapxmorfomap1", choices=c(colnames(dplyr::select(datamorfo(), -paste(nfoglieoliveold()))),paste(nfoglieolivenew())))
    updateSelectInput(session, "selyearmorfomap1", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
    updateSelectInput(session, "nummorfomap1", choices = row.names(table(dplyr::select(datamorfo(), "N_campionamento"))))
    #mappa 1
    updateSelectInput(session, "mapxmorfomap2", choices=c(colnames(dplyr::select(datamorfo(), -paste(nfoglieoliveold()))),paste(nfoglieolivenew())))
    updateSelectInput(session, "selyearmorfomap2", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
    updateSelectInput(session, "nummorfomap2", choices = row.names(table(dplyr::select(datamorfo(), "N_campionamento"))))
    #tabella
    updateSelectInput(session, "selyeardtmorfo", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
  })
  
  
  
  ### Grafico a barre
  output$barmorfo = renderPlotly({
    req(datamorfo())
    x = Olv_select_col(data = datamorfo(), input = input$selectxmorfobb)
    y =  Olv_select_col(data = datamorfo(), input = input$selectymorfobb)
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfobb)
    summ = datamorfo() %>% dplyr::group_by(Codice_azienda, Provincia, Azienda, Cultivar_principale, Anno, N_campionamento) %>% 
      dplyr::summarise(across(where(is.double), mean , na.rm = TRUE), n = dplyr::n()) %>%
      dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% dplyr::select(!starts_with("ID"))
    temp = ggplot(data = summ, mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"), 
                                                   fill = paste0("`",colnames(fill),"`"))) + geom_col() + 
      ggplot2::stat_summary(data = datamorfo(), geom = "errorbar", fun.data = mean_se) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  
  
  ### Boxplot

  output$boxmorfo = renderPlotly({
    req(datamorfo())
    x = Olv_select_col(data = datamorfo(), input = input$selectxmorfobb)
    y =  Olv_select_col(data = datamorfo(), input = input$selectymorfobb)
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfobb)
    temp = ggplot(data = datamorfo(), 
                  mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"), fill = paste0("`",colnames(fill),"`"))) + 
    geom_boxplot() + geom_jitter() + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  
  ##### Scatter plot
  
  datamorfoscatt = reactive({
    req(datamorfo())
    if(input$summarizescatt == TRUE){
      x = Olv_select_col(data = datamorfo(), input = input$selectsummscatt)
      dt = datamorfo() %>% dplyr::group_by(dplyr::across(colnames(x))) %>% 
        dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE), n = dplyr::n()) %>% 
        dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% select(!starts_with("ID"))

    } else{
      dt = datamorfo()
    }
  })
  
  observeEvent(datamorfoscatt(),{
    updateSelectInput(session, "selectxmorfoscatt", choices=colnames(datamorfoscatt()))
    updateSelectInput(session, "selectymorfoscatt", choices=colnames(datamorfoscatt()))
    updateSelectInput(session, "selectfillmorfoscatt", choices=colnames(datamorfoscatt()))
  })
  
  output$scattmorfo = renderPlotly({
    req(datamorfoscatt())
    dt = datamorfoscatt()
    x = Olv_select_col(data = dt, input = input$selectxmorfoscatt)
    y =  Olv_select_col(data = dt, input = input$selectymorfoscatt)
    fill = Olv_select_col(data = dt, input = input$selectfillmorfoscatt)
    temp = ggplot(data = dt, 
                  mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"), color = paste0("`",colnames(fill),"`"))) + 
      geom_count(size = 2)  + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    
    #in jitter se voglio colorare i punti aggiungo aes_string(color = paste0("`",colnames(fill),"`"))
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  
  #### HEATMAP MORFO ####
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearheatmorfo", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
    updateSelectInput(session, "numheatmorfo", choices = row.names(table(dplyr::select(datamorfo(), "N_campionamento"))))
  })
  
  
  #per poter far funzionare il tutto devo avere un file identico a datapolind (ovvero con cod_azienda, provincia e 
  #il resto (n_camp. anno...). Datamorfo ha Provincia e Cultivar in più quindi li elimino. Elimino anche gli ID o mi 
  #da problemi quando scalo per colonne). Inoltre summarizzo anche.
  datamorfoheat = reactive({
    temp = datamorfo() %>% dplyr::select(!starts_with("ID") & -c(Provincia,Cultivar_principale)) %>% dplyr::group_by(Codice_azienda, Anno, N_campionamento, Azienda) %>% 
      dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE)) %>% dplyr::ungroup()
    #aggiungo ungroup() o l'heatmap mi da problemi (tolgo il gruppo).
    
    #dato che in questo caso scalo i dati direttamente per colonna (quindi avrò lo z-score senza unità di misura),
    #elimino dalle columnames le unità di misura.
    colnames(temp) = stringr::str_replace_all(colnames(temp),"[()]","!")  #sostituisco parentesi con !
    colnames(temp) = gsub(pattern = "_!.*", replacement = "", x = colnames(temp)) #elimino da _! in poi
    return(temp)
  })
  

  dtheatsortedmorfo = reactive({
    sorder_data(
      data = data(),
      data2 = datamorfoheat(),
      year = input$selyearheatmorfo,
      n_camp = input$numheatmorfo,
      heat_sort = input$heatsortmorfo,
      add_annot = input$selectannotmorfo)
  })

  
  #creo slider per colonna
  output$sliderheatcolmorfo <- renderUI({
    req(dtheatsortedmorfo())
    len = dtheatsortedmorfo() %>% dplyr::select(-Anno, - N_campionamento,  -Codice_azienda, -input$selectannotmorfo)
    sliderInput("slidercolheatmorfo", "Numero cluster:", min=2, max=length(len), value=2, step = 1)
  })
  
  
  #creo l'heatmap
  dataheatmorfo = reactive({
    make_heatmap(
      datasorted = dtheatsortedmorfo(),
      add_annot = input$selectannotmorfo,
      scale_data = "column",
      dist_method = input$seldistheatmorfo,
      clust_method = input$selhclustheatmorfo,
      row_dend = input$rowdendmorfo,
      row_nclust = input$sliderrowheatmorfo,
      col_dend = input$columndendmorfo,
      col_nclust = input$slidercolheatmorfo,
      col_lab = "Misure"
    )
  })
  
  
  observeEvent(input$updateheatmorfo,{
    dataheat2 = dataheatmorfo()
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, dataheat2, output_id = "heatmap_outputmorfo", layout = "1|23", width1 = 750, height1 = 550)
  })
  

  
  
  ###### PCA morfo ####
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearpcamorfo", choices = row.names(table(dplyr::select(datamorfo(), Anno))))
    updateSelectInput(session, "numpcamorfo", choices = row.names(table(dplyr::select(datamorfo(), "N_campionamento"))))
  })

  #rimuovo le righe di NA se ci sono. pcadatinamorfo mi servirà dopo per il semi_join con data().
  pcadatinamorfo = reactive({
    req(datamorfo())
    #filtro in base agli anni presenti e scelgo anche il num campionamento
    datapre = datamorfo() %>% dplyr::filter(Anno == input$selyearpcamorfo) %>% dplyr::filter(N_campionamento == input$numpcamorfo)
    
    #se scelgo di summarizzare tolgo tutto tranne codice_azienda e i double, summarizzo e trasformo codice_azienda 
    #in rownames
    if(input$summarizepcamorfo == TRUE){
      datapre %>% dplyr::select(Codice_azienda, where(is.double), -dplyr::any_of(c("Anno", colnames(dplyr::select(datapre, starts_with("ID")))))) %>% 
        dplyr::group_by(Codice_azienda) %>% dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% stats::na.exclude()
    } else{
      #se non voglio summarizzare tolgo tutto tranne cod_azienda, l'ID e i double. Poi unisco cod_azienda e ID (SA_01_1 etc.)
      #e trasformo in rownames
      data = datapre %>% dplyr::select(Codice_azienda, starts_with("ID"), where(is.double), -Anno)
      data %>% tidyr::unite("Codice_azienda", c("Codice_azienda", colnames(dplyr::select(datapre, starts_with("ID"))))) %>% stats::na.exclude()
    }

  })


  #pca
  pcadatimorfo = reactive({
    req(pcadatinamorfo())
    data = pcadatinamorfo() %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    stats::princomp(data, cor = input$selcorpcamorfo)
  })

  
  
  #dataframe da usare per il colore riempimento e il plot 3D. Dagli scores estraggo i rownames per poi fare il join con data.
  #Se non ho summarizzato divido Codice_azienda eliminando l'ID (da cod.az. = SA_01_3 a cod.az. = SA_01 | ID = _3)
  datacolorpcamorfo = reactive({
    req(pcadatimorfo())
    statna = pcadatimorfo()
    scores = statna$scores
    scoretb = scores %>% as.data.frame() %>% tibble::rownames_to_column("Codice_azienda") %>% tibble::as_tibble()
    if(input$summarizepcamorfo == FALSE){
      scoretb = scoretb %>% tidyr:::separate(Codice_azienda, into = c("Codice_azienda", "ID"), sep = 5) #sep -2 ultime 2 cifre
    }
    dplyr::left_join(x = scoretb, y = dplyr::select(data(), Codice_azienda, Provincia, Cultivar_principale, Areale), by = "Codice_azienda")
    
    
  })
  
  
  #slider dinamico per la scelta delle pcs
  output$sliderpcmorfo <- renderUI({
    req(pcadatimorfo())
    pca = pcadatimorfo()
    sliderInput("selpcsmorfo", "Numero di Componenti Principali (PC)", min=1, max=length(pca$sdev), value=2, step = 1)
  })



  ###plot loadings
  output$loadingsmorfo = plotly::renderPlotly({
    req(pcadatimorfo())
    pca = pcadatimorfo()
    loadpca = as.data.frame(pca$loadings[, input$selpcsmorfo])
    loadpca = tibble::rownames_to_column(loadpca)

    pcasdev = as.data.frame(round(pca$sdev^2/sum(pca$sdev^2)*100, 2))

    colnames(loadpca) = c("Misure", paste0("PC", input$selpcsmorfo)) #c("Polifenoli", pste0(...))
    loadplot = ggplot(loadpca) + geom_col(aes(x = Misure, y = loadpca[,2], fill = Misure)) +
      labs(y = paste0("PC", input$selpcsmorfo, " ", "(", pcasdev[as.numeric(input$selpcsmorfo), ], "%", ")"), title = "Loadings")
    plotly::ggplotly(loadplot)
  })

  ###screeplot
  output$screeplotmorfo <- plotly::renderPlotly({
    pca = pcadatimorfo()
    var = cumsum(100*pca$sdev^2/sum(pca$sdev^2))
    var = as.data.frame(cbind(var)) %>% tibble::rownames_to_column()
    colnames(var) = c("Componenti_principali", "Varianza_spiegata")

    screegg = ggplot(var, aes(Componenti_principali, Varianza_spiegata)) +
      geom_line(colour = "red", group = 1, linetype = "dashed", size = 1) + geom_point(size = 4, colour = "red") +
      labs(x = "Componenti principali", y = "Varianza spiegata (%)", title = "Screeplot") +
      scale_y_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10)))
    plotly::ggplotly(screegg)

  })


  ###biplot
  output$biplotmorfo = plotly::renderPlotly({
    req(pcadatimorfo())

    temp = autoplot(pcadatimorfo(), data = datacolorpcamorfo(), shape = input$shpbiplotmorfo, colour = input$colbiplotmorfo, loadings = TRUE, loadings.colour = 'blue',
                    loadings.label = TRUE, loadings.label.size = 4, title = "Biplot")
    plotly::ggplotly(temp)  
 
  })

  #### Plot 3D

  output$pca3dmorfo = plotly::renderPlotly({
    req(datacolorpcamorfo())

    datacolorpcamorfo() %>% plotly::plot_ly(x = ~Comp.1, y = ~Comp.2, z= ~Comp.3, type = "scatter3d", mode = "markers", color = ~base::get(input$col3dmorfo))
    
  })
  
  
  ###### Clustering morfo ######
  
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearclustmorfo", choices = row.names(table(dplyr::select(datamorfo(), Anno))))
    updateSelectInput(session, "numclustmorfo", choices = row.names(table(dplyr::select(datamorfo(), "N_campionamento"))))
  })

  dataclustmorfo = reactive({
    req(datamorfo())
    #filtro in base agli anni presenti e scelgo anche il num campionamento
    datapre = datamorfo() %>% dplyr::filter(Anno == input$selyearclustmorfo) %>% dplyr::filter(N_campionamento == input$numclustmorfo)

    #se scelgo di summarizzare tolgo tutto tranne codice_azienda e i double, summarizzo e trasformo codice_azienda
    #in rownames
    datapre = datapre %>% dplyr::select(Codice_azienda, where(is.double), -dplyr::any_of(c("Anno", colnames(dplyr::select(datapre, starts_with("ID")))))) %>%
        dplyr::group_by(Codice_azienda) %>% dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% stats::na.exclude()
    datapre %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda") %>% scale()

  })

  #grafici per la scelta del numero di cluster
  output$numclustergraph = renderPlot({
    req(dataclustmorfo())
    
    if(input$selclusthmorfo == "K-means"){
      p1 = factoextra::fviz_nbclust(dataclustmorfo(), stats::kmeans, method = "gap_stat")
      p2 = factoextra::fviz_nbclust(dataclustmorfo(), stats::kmeans, method = "wss")
      p3 = factoextra::fviz_nbclust(dataclustmorfo(), stats::kmeans, method = "silhouette")
    } else if(input$selclusthmorfo == "PAM"){
      p1 = factoextra::fviz_nbclust(dataclustmorfo(), cluster::pam, method = "gap_stat")
      p2 = factoextra::fviz_nbclust(dataclustmorfo(), cluster::pam, method = "wss")
      p3 = factoextra::fviz_nbclust(dataclustmorfo(), cluster::pam, method = "silhouette")
    } else{
      p1 = factoextra::fviz_nbclust(dataclustmorfo(), cluster::clara, method = "gap_stat")
      p2 = factoextra::fviz_nbclust(dataclustmorfo(), cluster::clara, method = "wss")
      p3 = factoextra::fviz_nbclust(dataclustmorfo(), cluster::clara, method = "silhouette")
    }
    
    if(input$selclustmethod == "Partizionale"){
      gridExtra::grid.arrange(p1, p2, p3, ncol = 2)
    } else{
      meth = c("single","complete","ward.D","ward.D2")
      d = stats::dist(dataclustmorfo())
      par(mfrow=c(2,2))
      for(i in seq(1,4)){
        hs = stats::hclust(d, method = meth[i])
        plot(hs$height, pch=16, main = meth[i], ylab = "Height")
      }
    }
  })
  
  #data cluster
  output$plotclustermorfo = renderPlot({
    req(dataclustmorfo())
    if(input$selclustmethod == "Partizionale"){
    if(input$selclusthmorfo == "K-means"){
      clust = stats::kmeans(dataclustmorfo(), centers = input$selnumclustmorfo, nstart = 25)
      
    } else if (input$selclusthmorfo == "PAM"){
      clust = cluster::pam(dataclustmorfo(), k = input$selnumclustmorfo)
    } else {
      clust = cluster::clara(dataclustmorfo(), k = input$selnumclustmorfo)
    }
    factoextra::fviz_cluster(clust, data = dataclustmorfo(), ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())
    } else {
      hcluster = factoextra::eclust(dataclustmorfo(), "hclust", hc_method = input$selhclustmeth, k = input$selnumclustmorfo)
      p1 = factoextra::fviz_dend(hcluster, palette = "jco", rect = TRUE, show_labels = FALSE)
      p2 = factoextra::fviz_silhouette(hcluster)
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })
  
  

  ### Mappa

  #stampa mappa 1
  output$mapmorfo1 = renderTmap({
    req(datamorfomap())
    
    datamap = datamorfomap() %>% dplyr::filter(Anno == input$selyearmorfomap1) %>% dplyr::filter(N_campionamento == input$nummorfomap1)
    
    datam = datamap %>% dplyr::group_by(Codice_azienda, Provincia, Azienda, Cultivar_principale, N_campionamento) %>%
      dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE), n= dplyr::n()) %>% 
      dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% select(!starts_with("ID"))
    #qui devo mettere per forza provincia, azienda etc perchè summarizzando per valori "double" tutto il resto
    #non presente nei gruppi viene perso.
    datasel = dplyr::select(datam, !starts_with("UTM"))
    column = Olv_select_col(data = datasel, input = input$mapxmorfomap1)
    make_tmap(data =  datam, dotlegend = column)
  })
 
  
  #stampa mappa 2
  output$mapmorfo2 = renderTmap({
    req(datamorfomap())
    
    datamap = datamorfomap() %>% dplyr::filter(Anno == input$selyearmorfomap2) %>% dplyr::filter(N_campionamento == input$nummorfomap2)
    
    datam = datamap %>% dplyr::group_by(Codice_azienda, Provincia, Azienda, Cultivar_principale) %>% 
      dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE), n = dplyr::n()) %>% 
      dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% select(!starts_with("ID"))
    datasel = dplyr::select(datam, !starts_with("UTM"))
    column = Olv_select_col(data = datasel, input = input$mapxmorfomap2)
    make_tmap(data =  datam, dotlegend = column)
  })
  
  
 
}
