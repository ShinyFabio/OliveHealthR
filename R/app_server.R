#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom plotly renderPlotly ggplotly layout
#' @rawNamespace import(ggplot2, except = last_plot)
#' @rawNamespace import(stats, except = filter)
#' @import tmap
#' @import tmaptools
#' @import ggfortify
#' @import htmltools
#' @import scales
#' @importFrom dplyr select inner_join mutate filter rename
#' @importFrom tidyr unite
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
  drupe= reactive({
    req(input$drupeinput)
    x = readr::read_delim(input$drupeinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(date_format = "%d/%m/%Y", encoding = "windows-1252"))
    x$Indice_maturazione = factor(x$Indice_maturazione, levels = c(0:8), ordered = TRUE)
    x$Fase_fenologica = factor(x$Fase_fenologica, levels = c(51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89), ordered = TRUE)
    return(x)
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

  
  ##### MAPPE DATADRUPE #####
  ####
  ####seconda mappa (datadrupe)##
  

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
  
  
  #### stampa terza mappa (drupe) ###
  

  observeEvent(drupe(), {
    updateSelectInput(session, "select3map", choices = colnames(drupe()))
  })
  
  #aggiorna il selectinput "selyear" in base agli anni presenti
  observeEvent(dtdrupanno(), {
    updateSelectInput(session, "selyear2", choices = row.names(table(dplyr::select(dtdrupanno(), "Anno"))))
  })
  

  #stampa mappa2
  output$map3 = renderTmap({
    req(drupe())
    req(dtdrupanno())
    datamap = dtdrupanno() %>% dplyr::filter(Anno == input$selyear2) %>% dplyr::filter(N_campionamento == input$num2map)
    colmap = Olv_select_col(data = drupe(), input = input$select3map)
    make_tmap(data = datamap, dotlegend = colmap)
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
  
  
  
  ######### BARPLOT ###
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
  
  
  
  ################# FOTO CAMPIONI ############
  
  
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
  
  
  ##########################POLIFENOLI##############################################
  
  #polif
  #fare il join di data con i polifenoli
  datapolifmap = reactive({
    req(polif())
    z = data() %>% dplyr::select("Codice_azienda", "Azienda", "UTM_33T_E", "UTM_33T_N")
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
    dplyr::select(datapolif(), c("Codice_azienda", "Azienda", "Anno", "N_campionamento", "Polifenoli_tot", "Presenza_larve"))
  })
  
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
    
    temp2=ggplot(data=colorcamptot()) + 
      geom_col(mapping = aes_string(x = colnames(showcoltotx()), y = colnames(showcoltoty()), fill = "N_campionamento"), position = position_dodge2(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(ylabtot()) + xlab(xlabtot())
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })
  
  
  
  
  ##################################POLIFENOLI INDIVIDUALI##############################
  
  
  #data polifenoli individuali
  datapolind= reactive({
    dplyr::select(datapolif(), !c("Polifenoli_tot", "Presenza_larve"))
  })
  
  #crea tabella polifenoli individuali con l'unità di misura
  output$tablepolind = DT::renderDT({ 
    req(datapolind())
    temp = datapolind()
    for(i in seq(6,length(temp))){
      names(temp)[i] = paste0(names(temp)[i], "_(ug/ml)")
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
    
    xlabg = label_with_unit(data = datapoltot(), colname = x, unit = "(µg/ml)")
    ylabg = label_with_unit(data = datapoltot(), colname = y, unit = "(µg/ml)")
    filllab = label_with_unit(data = datapoltot(), colname = fill, unit = "(µg/ml)")

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

    xlabg = label_with_unit(data = datapoltot(), colname = x, unit = "(µg/ml)")
    ylabg = label_with_unit(data = datapoltot(), colname = y, unit = "(µg/ml)")

    temp2=ggplot(data=colorcampind()) + 
      geom_col(mapping = aes_string(x = colnames(x), y = colnames(y), fill = "N_campionamento"), position = position_dodge2(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(ylabg) + xlab(xlabg)
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })
  
  
  
  ################# HEATMAP POLIFENOLI INDIVIDUALI ##################
  
  
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti
  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearheatind", choices = row.names(table(dplyr::select(datapolind(), "Anno"))))
  })


  ###scegliere l'anno e  il campionamento
  dtindfiltheat = reactive({
    req(datapolind())
    datapolind() %>% dplyr::filter(Anno == input$selyearheatind) %>% dplyr::filter(N_campionamento == input$numheat)
  })
  
  
  #ordinare
  dtheatsorted = reactive({
    
    req(dtindfiltheat())
    seletannota = input$selectannot
    dati = dplyr::select(data(), Codice_azienda, input$selectannot)
    datipolif = dplyr::select(dtindfiltheat(), -Azienda)
    htdata = dplyr::inner_join(datipolif, dati, by = "Codice_azienda")
    
    if(input$heatsort == TRUE){
      htdata[do.call(order, htdata[as.character(seletannota[1])]), ]
    } else{
      return(htdata)
    }
    
  })
  
  
  #creo slider per colonna
  output$sliderheatcol <- renderUI({
    req(dtheatsorted())
    len = dtheatsorted() %>% dplyr::select(-Anno, - N_campionamento,  -Codice_azienda, -input$selectannot)
    sliderInput("slidercolheat", "Numero cluster:", min=2, max=length(len), value=2, step = 1)
  })
  
  
  #creo l'heatmap
  dataheat = reactive({
    req(dtheatsorted())
    #creo la matrice con rownames 
    temp = dtheatsorted() %>% dplyr::select(-Anno, - N_campionamento, -input$selectannot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    
    #scale none, row, column
    if(input$selscaleheat == "column"){
      temp = scale(temp) # scale and center columns
    } else if(input$selscaleheat == "row"){
      temp = t(scale(t(temp))) # scale and center rows
    } 
    
    #dendrogram = none', 'row', 'column' or 'both' 
    if(input$rowdend == TRUE){
      row_dend = temp %>% stats::dist(method = input$seldistheatrow) %>% stats::hclust(method = input$selhclustheatrow) %>% stats::as.dendrogram()
      row_dend = dendextend::color_branches(row_dend, k = input$sliderrowheat)
      row_split = input$sliderrowheat
    } else {
      row_dend = FALSE
      row_split = NULL
      
    }
    
    if(input$columndend == TRUE){
      col_dend = temp %>% t() %>% stats::dist(method = input$seldistheatcol) %>% stats::hclust(method = input$selhclustheatcol) %>% stats::as.dendrogram()
      col_dend = dendextend::color_branches(col_dend, k = input$slidercolheat)
      col_split = input$slidercolheat
    } else {
      col_dend = FALSE
      col_split = NULL
    }
    
    
    annotdata = dplyr::select(dtheatsorted(), Codice_azienda, input$selectannot) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
    leng = annotdata %>% dplyr::select(input$selectannot) %>% table() %>% length()
    colorannot = stats::setNames(grDevices::rainbow(n = leng), c(row.names(table(annotdata))))
    colorannot = stats::setNames(list(colorannot), paste(input$selectannot))
    col_ha = ComplexHeatmap::HeatmapAnnotation(df = annotdata, which = "row", col = colorannot)
    
    
    ht = ComplexHeatmap::Heatmap(temp, name = "ug/ml",  rect_gp = grid::gpar(col = "white", lwd = 1), row_title = "Codice azienda", 
                               column_title = "Polifenoli", row_names_gp = grid::gpar(fontsize = 11),
                               cluster_rows = row_dend, cluster_columns = col_dend, 
                               left_annotation = col_ha,
                               column_split = col_split, row_split = row_split)
    ht = ComplexHeatmap::draw(ht)
    return(ht)
    
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
    
    temp = datatemp %>% dplyr::select(-Anno, - N_campionamento, -Azienda, - Codice_azienda)
    temp2 = round(stats::cor(temp),1)
    par(xpd = TRUE)
    
    plot = ggcorrplot::ggcorrplot(temp2, hc.order = TRUE, type = "lower", outline.col = "white", show.diag = TRUE)
    plotly::ggplotly(plot)
   
  })
  
  
  
  
  ######################### PCA #####################################################
  
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearpca", choices = row.names(table(dplyr::select(datapolind(), Anno))))
  })

  
  pcadati = reactive({
    req(datapolind())
    #filtro in base agli anni presenti e scelgo anche il num campionamento
    data = datapolind() %>% dplyr::filter(Anno == input$selyearpca) %>% dplyr::filter(N_campionamento == input$numpca) %>% 
      dplyr::select(-Anno, -N_campionamento, -Azienda) %>% as.data.frame() %>% tibble::column_to_rownames("Codice_azienda")
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
      labs(y = paste0("PC2", " ", "(", pcasdev[as.numeric(2), ], "%", ")"), title = "Loadings")
    plotly::ggplotly(loadplot)
  })
  
  ###screeplot
  output$screeplot <- plotly::renderPlotly({
    pca = pcadati()
    var = cumsum(pca$sdev^2/sum(pca$sdev^2)) 
    var = as.data.frame(cbind(var)) %>% tibble::rownames_to_column()
    colnames(var) = c("Componenti_principali", "Varianza_spiegata")
    
    screegg = ggplot(var, aes(Componenti_principali,Varianza_spiegata)) +
      geom_line(colour = "red", group = 1, linetype = "dashed", size = 1) + geom_point(size = 4, colour = "red") + 
      labs(x = "Componenti principali", y = "Varianza spiegata (%)", title = "Screeplot") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1)))
    plotly::ggplotly(screegg)
    
  })
  
  
  ###biplot
  output$biplot = plotly::renderPlotly({
    req(pcadati())
    temp = autoplot(pcadati(), data = data(), colour = input$colbiplot, loadings = TRUE, loadings.colour = 'blue', 
                    loadings.label = TRUE, loadings.label.size = 4, title = "Screeplot")
    plotly::ggplotly(temp)
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
    
    z = data() %>% dplyr::select("Codice_azienda", "Provincia", "Azienda")
    x = dplyr::inner_join(x = z, y = tempdata, by = "Codice_azienda")
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
    datamorfo()
  })
  
  
  ### Grafico a barre
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selectxmorfo", choices=colnames(datamorfo()))
    updateSelectInput(session, "selectymorfo", choices=colnames(datamorfo()))
    updateSelectInput(session, "selectfillmorfo", choices=colnames(datamorfo()))
    updateSelectInput(session, "mapxmorfo2", choices=colnames(datamorfo()))

  })
  
  output$barmorfo = renderPlotly({
    req(datamorfo())
    x = Olv_select_col(data = datamorfo(), input = input$selectxmorfo)
    y =  Olv_select_col(data = datamorfo(), input = input$selectymorfo)
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfo)
    temp = ggplot(data = datamorfo(), aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"), 
                                                      fill = paste0("`",colnames(fill),"`"))) + 
      ggplot2::stat_summary(fun = mean, geom = "bar") + ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  
  
  ### Boxplot

  output$boxmorfo = renderPlotly({
    req(datamorfo())
    x = Olv_select_col(data = datamorfo(), input = input$selectxmorfo)
    y =  Olv_select_col(data = datamorfo(), input = input$selectymorfo)
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfo)
    temp = ggplot(data = datamorfo()) + 
    geom_boxplot(mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"), 
                                      fill = paste0("`",colnames(fill),"`"))) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
  plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  ### Mappa
  
  
  #stampa mappa 2
  output$mapmorfo1 = renderTmap({
    req(datamorfomap())
    
    datam = datamorfo() %>% dplyr::group_by(Codice_azienda, Anno, N_campionamento) %>% 
      dplyr::summarise(across(where(is.double), mean , na.rm = TRUE))
    coord = datamorfomap() %>% dplyr::group_by(Codice_azienda, Anno, N_campionamento) %>% 
      dplyr::summarise(across(where(is.double), mean , na.rm = TRUE))
    
    #datamap = coord %>% dplyr::filter(Anno == input$selyearpol2) %>% dplyr::filter(N_campionamento == input$numpol2)
    column = Olv_select_col(data = datam, input = input$mapxmorfo2)
    make_tmap(data =  coord, dotlegend = column)
  })
 
  
}
