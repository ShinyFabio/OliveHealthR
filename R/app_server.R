#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom plotly renderPlotly ggplotly layout
#' @rawNamespace import(ggplot2, except = last_plot)
#' @rawNamespace import(stats, except = filter)
#' @rawNamespace import(dplyr, except = lag)
#' @import tmap
#' @import tmaptools
#' @import ggfortify
#' @import htmltools
#' @import scales
#' @importFrom tidyr unite starts_with separate gather drop_na
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom grid gpar
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom lubridate year yday leap_year
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapWidget
#' @importFrom readr read_delim locale parse_factor
#' @importFrom ComplexHeatmap Heatmap HeatmapAnnotation draw
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom corrplot corrplot
#' @importFrom DT renderDT datatable formatRound
#' @importFrom grDevices rainbow hcl.colors colorRampPalette
#' @importFrom dendextend color_branches
#' @importFrom sf st_as_sf st_crs
#' @importFrom VIM aggr
#' @importFrom stringr str_replace_all
#' @importFrom factoextra fviz_nbclust fviz_cluster fviz_dend fviz_silhouette eclust
#' @importFrom cluster pam clara
#' @importFrom gridExtra grid.arrange
#' @importFrom calendR calendR
#' @importFrom FSA dunnTest
#' @importFrom fmsb radarchart
#' @importFrom janitor remove_empty
#' @importFrom DataEditR dataEditUI dataEditServer
#' @importFrom shinyBS bsModal
#' @noRd


app_server <- function( input, output, session ) {
  # List the first level callModules here


  ############# Upload file ###############
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "navb1",
                      selected = "panel2")
  })
  
  observeEvent(input$jumptohome, {
    updateTabsetPanel(session, "navb1",
                      selected = "panel1")
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
  data2 = reactive({
    req(input$file1)
    readr::read_delim(input$file1$datapath, delim = input$delim, col_names = input$header, na = "", local = readr::locale(encoding = "windows-1252")) %>%
      janitor::remove_empty("rows")
  })
  


  data = mod_update_data_server("updataaziende", original_data = data2, type_data = "azienda")


  #carico il file descrizione csv
  descri2 = reactive({
    req(input$desc1)
    req(data())
    temp = readr::read_delim(input$desc1$datapath, delim = input$delim, col_names = input$header, local = readr::locale(encoding = "windows-1252")) %>%
      janitor::remove_empty("rows")
    temp2 = data() %>% dplyr::select(Codice_azienda, Azienda)
    dplyr::inner_join(x = temp, y = temp2, by = "Codice_azienda")
  })
  
  
  #file già in memoria. Per vedere i file originali e lo script per produrli vedere in data-raw.
  drupe2 = reactive(drupecamp2020)
  
  
  drupe = mod_update_data_server("updatadrupe", original_data = drupe2, type_data = "camp_drupe", confronto_aziende = data)
  
  # drupe = reactive({
  #   x = drupe3()
  #   x$Indice_maturazione = factor(x$Indice_maturazione, levels = c(0:8), ordered = TRUE)
  #   x$Fase_fenologica = factor(x$Fase_fenologica, levels = c(51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89), ordered = TRUE)
  #   return(x)
  # })
  # 
  oliocamp = reactive(oliocampionamento2020)
  
  assaggi = reactive(assaggi2020)
  
  #carico il file polifenoli come .csv
  polif2 = reactive({
    req(input$polifinput)
    x = readr::read_delim(input$polifinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = input$decim, date_format = "%d/%m/%Y", encoding = "windows-1252")) %>% 
      janitor::remove_empty("rows")
    #x$Presenza_larve = readr::parse_factor(as.character(x$Presenza_larve), levels = c("0","1","2"), ordered = TRUE)
    return(x)
  })
  
  polif = mod_update_data_server("updatapol", original_data = polif2, type_data = "polifenoli", confronto_aziende = data)
  
  
  
  ##### Carico i file polifenoli LCxLC ___________
  lcpolfoglie = reactive(polifenoliLCxLC2020$Foglie)
  
  lcpoldrupe = reactive(polifenoliLCxLC2020$Drupe)
  
  lcpololio = reactive(polifenoliLCxLC2020$Olio)
  
  lcpolposa = reactive(polifenoliLCxLC2020$Posa)
  
  lcpolsansa = reactive(polifenoliLCxLC2020$Sansa)
  
  
  #carico i file morfometria
  
  #file foglie
  morfoleaf = reactive(morfometria2020$Foglie)
  
  #file morfometria drupe
  morfodrupe = reactive(morfometria2020$Drupe)
  
  #file morfometria endocarpo
  morfoendo = reactive(morfometria2020$Endocarpo)
  
  #file rapporti drupe endocarpo
  morforatio = reactive(morfometria2020$Rapporti)
  
  
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
  

  #crea la colonna anno
  dtdrupanno = reactive({
    req(datadrupemap())
    datadrupemap() %>% dplyr::mutate(Anno = lubridate::year(Data_campionamento)) #cambiato datadrupe
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
  

  mod_render_map_server("modulo_mappa_datadrupe", datamap = datadrupemap, datacol = drupe, extract_year = TRUE, extract_ncamp = TRUE)
  
  
  ################# Foto campioni ############
  
  
  #crea la tabella
  output$prov2 = DT::renderDT(dplyr::select(data(), c("Codice_azienda", "Cultivar_principale", "Azienda")), selection = "single", server = FALSE, rownames = FALSE)
  

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
  
  
  
  ##### Mappa Olio
  observeEvent(oliocamp(), {
    updateSelectInput(session, "selcoloilmap", choices = colnames(oliocamp()))
    updateSelectInput(session, "selyearoilmap", choices = row.names(table(lubridate::year(oliocamp()$Data_campionamento))))
  })
  

  #stampo mappa
  output$mapolio = renderTmap({
    req(oliocamp())    
    #filtra in base all'anno selezionato 
    datayear = oliocamp() %>% dplyr::filter(lubridate::year(Data_campionamento) == input$selyearoilmap)
    #data with coordinates
    data = data() %>% dplyr::select(Codice_azienda, starts_with("UTM"))
    
    datamap = dplyr::left_join(x = data, y = datayear, by = "Codice_azienda")

    colmap = Olv_select_col(data = oliocamp(), input = input$selcoloilmap)
    make_tmap(data = datamap, dotlegend = colmap, palette = "Set1")
  })
  
  
  
  ############ Calendario #################

 
  #vettore per il radiobutton
  checkfilecalend = reactive({
    if(!is.null(drupe()) == TRUE && !is.null(oliocamp()) == TRUE){
      c("Tutto", "Olio", "Drupe e foglie")
    } else if(!is.null(drupe()) == TRUE && !is.null(oliocamp()) == FALSE){
      "Drupe e foglie"
    }else if (!is.null(drupe()) == FALSE && !is.null(oliocamp()) == TRUE){
      "Olio"
    }
  })
  
  #aggiorna il awesomeradio "selfilecalend" in base ai file
  observeEvent(checkfilecalend(), {
    updateAwesomeRadio(session, "selfilecalend", choices = checkfilecalend())
  })
  
  
  
  joinfilecalendar = reactive({
    drupdate = dplyr::select(drupe(), Codice_azienda, Data_campionamento, N_campionamento) %>% dplyr::mutate(campione = "Campionamento drupe e foglie") %>% tidyr::drop_na()
    oliodate = dplyr::select(oliocamp(), Codice_azienda, Data_campionamento, N_campionamento) %>% dplyr::mutate(campione = "Campionamento olio") %>% tidyr::drop_na()
    
    if(input$selfilecalend == "Tutto"){
      dplyr::bind_rows(oliodate, drupdate) 
    } else if(input$selfilecalend == "Drupe e foglie"){
      drupdate
    } else if(input$selfilecalend == "Olio"){
      oliodate
    }
  })
  
  
  #scelgo il campionamento
  selcampcalendar = reactive({
    req(joinfilecalendar())
    if(input$selfilecalend == "Drupe e foglie"){
      if(input$selcampcalend == "R1"){
        joinfilecalendar() %>% dplyr::filter(N_campionamento == "R1") %>% dplyr::select(-N_campionamento)
      } else if(input$selcampcalend == "R2"){
        joinfilecalendar() %>% dplyr::filter(N_campionamento == "R2") %>% dplyr::select(-N_campionamento)
      } else{
        joinfilecalendar() %>% dplyr::select(-N_campionamento)
      }
    } else{
      joinfilecalendar() %>% dplyr::select(-N_campionamento)
    }
  })
  
  

  #filtra in base all'anno
  observeEvent(selcampcalendar(), {
    updateSelectInput(session, "selyearcalend", choices = unique(lubridate::year(selcampcalendar()$Data_campionamento)))
  })
  
  yearcalendar = reactive({
    req(selcampcalendar())
    dplyr::filter(selcampcalendar(), lubridate::year(Data_campionamento) == input$selyearcalend)
    })

  
  #scelgo l'azienda
  observeEvent(yearcalendar(), {
    updateSelectInput(session, "selaziendacalend", choices = c("Tutte", unique(yearcalendar()$Codice_azienda)), selected = "Tutte")
  })
  
  joinfiltered = reactive({
    req(yearcalendar())
    if(input$selaziendacalend == "Tutte"){
      yearcalendar()
    } else{
      yearcalendar() %>% dplyr::filter(Codice_azienda == input$selaziendacalend) 
    }
  })
  
  #creo il calendario
  output$yearcalendar = renderPlot({
    req(joinfiltered())
    #mi salvo quanti giorni ci sono in quell'anno
    if (lubridate::leap_year(as.numeric(input$selyearcalend)) == TRUE){totaldays = 366} else{totaldays = 365}
    
    
    #mi salvo i giorni speciali
    onlyspecial = joinfiltered() %>% dplyr::mutate(day = lubridate::yday(Data_campionamento)) %>% 
      dplyr::select(campione, day) %>% dplyr::distinct()
    
    #ora però ho il problema di due campionamenti diversi (es. olio e drupe) nello stesso giorno. Infatto avrò due giorni
    #uguali ma con campionamenti diversi (es. day 314 = olio, day 314 = drupe). 
    #Per fare questo mi creo un vettore contenente i duplicati. Lo faccio avanti e indietro (fromLast = T) o mi prende solo l'ultimo doppione
    all_dup = duplicated(onlyspecial$day) + duplicated(onlyspecial$day, fromLast = TRUE) >0
    
    #poi lo unisco con i dati, e se quella riga ha i doppioni (all_dup = T) mi cambia la scritta, altrimenti lascio come sta. 
    #Elimino i doppioni con distinct e elimino la colonna dei true/false
    ggg = cbind(onlyspecial, all_dup) %>% dplyr::mutate(campione = ifelse(all_dup == TRUE, "Campionamento olio, drupe e foglie", campione)) %>% 
      dplyr::distinct() %>% dplyr::select(-all_dup)
    
    #ora mi creo il vettore finale contenente i NA dove non ci sta nulla e lo unisco agli "eventi"
    totspecial =  dplyr::left_join(tibble(day = rep(1:totaldays)), ggg, by = "day") 
    
    ncolors = grDevices::hcl.colors(length(row.names(table(totspecial$campione))), palette = "Harmonic")
    #creo il calendario
    calendR::calendR(year = input$selyearcalend, special.days = totspecial$campione,  
      special.col = ncolors, legend.pos = "right", mbg.col = 4, day.size = 5, months.size = 15, bg.col = "#f4f4f4", 
      lty = 0, months.col = "white") + ggplot2::theme(legend.key.size = unit(5, units = "mm"), # Keys size
                                                      legend.text = element_text(size = 20))
    
  })

  
  
  ########### Assaggi sensoriali ############
  
  ########## Tabella
  output$tableassaggischeda = renderDT({
    req(assaggi())
    assaggi()
  })
  
  
  ########## Grafici
  
  #fare il join di data con assaggi (questo lo uso solo per gli allegati)
  assaggidataph = reactive({
    req(assaggi())
    x = dplyr::inner_join(x = data(), y = assaggi(), by = "Codice_azienda")
  })
  
  
  #e unisco codice_azienda con tipo olio
  assaggidatamap = reactive({
    req(assaggidataph())
    assaggidataph() %>% tidyr::unite(col = Codice_azienda, Codice_azienda, Tipo_olio, sep = "_", remove = TRUE)
  })
  
  #rimuovere colonne coordinate
  dataassaggi = reactive({
    req(assaggidatamap())
    assaggidatamap() %>% dplyr::select(!starts_with("UTM"))
  })
  
  
  observeEvent(dataassaggi(), {
    #scatterplot
    updateSelectInput(session, "selectxassaggiscatt", choices=colnames(dataassaggi()))
    updateSelectInput(session, "selectyassaggiscatt", choices=colnames(dplyr::select(dataassaggi(), where(is.double))))
    updateSelectInput(session, "selectfillassaggi", choices=colnames(dataassaggi()))
    updateSelectInput(session, "selyearscatterassagg", choices = row.names(table(dplyr::select(dataassaggi(), "Anno"))))
    
    #barplot
    updateSelectInput(session, "selyearbarassagg", choices = row.names(table(dplyr::select(dataassaggi(), "Anno"))))
    
    #spider plot
    updateSelectInput(session, "selyearspiderassaggi", choices = row.names(table(dplyr::select(dataassaggi(), "Anno"))))
    updateSelectInput(session, "selcodspiderassaggi1", choices = row.names(table(dplyr::select(dataassaggi(), "Codice_azienda"))))
    updateSelectInput(session, "selcodspiderassaggi2", choices = row.names(table(dplyr::select(dataassaggi(), "Codice_azienda"))))
    
    
  })
  
  

  ###grafico classico (scatter plot)   
  output$scattplotassagg = plotly::renderPlotly({
    dtassaggiyear = dataassaggi() %>% dplyr::filter(Anno == input$selyearscatterassagg)
    x = Olv_select_col(data = dataassaggi(), input = input$selectxassaggiscatt)
    y = Olv_select_col(data = dataassaggi(), input = input$selectyassaggiscatt)
    fill = Olv_select_col(data = dataassaggi(), input = input$selectfillassaggi)
    temp = ggplot(data = dtassaggiyear) +
      geom_count(mapping = aes_string(x = colnames(x), y = colnames(y), colour = colnames(fill))) +
      scale_size_continuous(range = c(3,9)) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  
  
  ## Barplot 

  ###grafico a barre
  output$barplotassagg = plotly::renderPlotly({
    x = Olv_select_col(data = dataassaggi(), input = input$selectxassaggibar)
    y = Olv_select_col(data = dataassaggi(), input = input$selectyassaggibar)
    assaggiyear = dataassaggi() %>% dplyr::filter(Anno == input$selyearbarassagg)
    gatherassaggi = tidyr::gather(assaggiyear, Mediana_misura, Valore, c(Mediana_fruttato, Mediana_amaro, Mediana_piccante), factor_key=TRUE)
    
    #if(input$barplotassaggi == "Affiancato"){
     temp2=ggplot(data=gatherassaggi) + 
      geom_col(mapping = aes_string(x = "Codice_azienda", y = "Valore", fill = "Mediana_misura"), position = input$barplotassaggi) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) 
    # }else{
    #   #assaggiuniti = dtassaggiyear() %>% dplyr::select(-starts_with("CVR"), -Presenza_difetti)
    #   temp2 = ggplot(data = gatherassaggi) + 
    #     geom_col(mapping = aes_string(x = "Codice_azienda", y = "Valore", fill = "Mediana_misura"), position = position_stack() ) + 
    #     theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank())
    # }
    plotly::ggplotly(temp2)
  })
  
  
  # Spider plot
  output$assaggispider = renderPlot({
    #seleziono anno, prendo solo cod.az e mediana e cod diventa rownames
    radardata = dataassaggi() %>% dplyr::filter(Anno == input$selyearspiderassaggi) %>% 
      dplyr::select(Codice_azienda, starts_with("Mediana")) %>% column_to_rownames("Codice_azienda")
    #creo righe max e min
    max_min = data.frame(Mediana_fruttato = c(0,10), Mediana_amaro = c(0,10), Mediana_piccante = c(0,10))
    rownames(max_min) <- c("Max", "Min")
    #unisco maxmin con radardata
    radardatamm = rbind(max_min, radardata)
    
    if(input$addcodspiderassaggi == FALSE){
     create_beautiful_radarchart(radardatamm[c("Max", "Min", input$selcodspiderassaggi1),], 
                                caxislabels = c(0, 2, 4, 6, 8, 10), 
                                color = grDevices::hcl.colors(2, palette = "Dynamic")) 
    } else{
      create_beautiful_radarchart(radardatamm[c("Max", "Min", input$selcodspiderassaggi1, input$selcodspiderassaggi2),], 
                                  caxislabels = c(0, 2, 4, 6, 8, 10), 
                                  color = grDevices::hcl.colors(2, palette = "Dynamic")) 
    }
      
    
  }, width = 800, height = 600)
  
  
  ############## Foto Allegati
  #crea la tabella
  output$dtallegato = DT::renderDT(dplyr::select(assaggidataph(), c("Azienda", "Codice_azienda", "Tipo_olio")), selection = "single", server = FALSE, rownames = FALSE, options = list("pageLength" = 15))
  
  
  #aggiorna il selectinput "selyearfoto" in base agli anni presenti e seleziono
  observeEvent(assaggidataph(), {
    updateSelectInput(session, "selyearallegatph", choices = row.names(table(dplyr::select(dataassaggi(), "Anno"))))
  })
  
  

  #foto cromatogramma allegat
  output$phallegati = renderUI({
    req(input$dtallegato_rows_selected)
    nroww=input$dtallegato_rows_selected
    x = dplyr::select(dataassaggi(), "Codice_azienda")
    z = paste("www/assaggi", input$selyearallegatph, sep = "/") #selyearfoto e campfoto
    selallegat = paste(z, x[nroww,], sep = "/")
    allegato = paste0(selallegat,".jpg")
    tags$img(src = allegato, width = "75%", height = "75%")
  })
  
  
  
  ######## mappa allegati
  
  mod_render_map_server("modulo_mappa_assaggi", datamap = assaggidatamap, datacol = assaggi, extract_year = FALSE, extract_ncamp = FALSE)
  
  
  
  
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
  datapoltot = reactive({
    dplyr::select(datapolif(), c("Codice_azienda", "Azienda", "Anno", "N_campionamento", "Cultivar_principale", "Polifenoli_tot", "Presenza_larve"))
  })
  
  #data polifenoli totali per mappa
  datapoltotmap = reactive({
    dplyr::select(datapolifmap(), c("Codice_azienda", "Azienda", "Anno", "N_campionamento", "Cultivar_principale", "Polifenoli_tot", "Presenza_larve", "UTM_33T_E", "UTM_33T_N"))
    
  })
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatapoltot = reactive({
    req(datapoltot())
    data = datapoltot() %>% dplyr::select(where(is.double) & -Anno)
    return(data)
  })
  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxpoltot", data = nadatapoltot, text_size = 1.1)
  
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
  
  
  #########BARPLOT POLIFENOLI TOTALI#####################
  
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
  
  
  
  #################### Mappa Polifenoli totali ###########################################
  
  
  #mod_render_map_server("modulo_mappa_polifenoli", datamap = datapolifmap / datapolindmap, datacol =  datapolind, extract_year = FALSE, extract_ncamp = TRUE)
  
  
  
  
  observeEvent(datapoltot(), {
    updateSelectInput(session, "colpoltotmap1", choices = colnames(datapoltot()))
    updateSelectInput(session, "colpoltotmap2", choices = colnames(datapoltot()))
  })
  
  
  
  
  #aggiorna il selectinput "selyear" in base agli anni presenti
  observeEvent(datapoltotmap(), {
    updateSelectInput(session, "yearpoltotmap1", choices = row.names(table(dplyr::select(datapoltotmap(), Anno))))
    updateSelectInput(session, "yearpoltotmap2", choices = row.names(table(dplyr::select(datapoltotmap(), Anno))))
    
  })
  
  
  #stampo mappa
  output$poltotmap1 = renderTmap({
    req(datapoltotmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapoltotmap() %>% dplyr::filter(Anno == input$yearpoltotmap1) %>% dplyr::filter(N_campionamento == input$numpoltotmap1)
    colmap = Olv_select_col(data = datapoltot(), input = input$colpoltotmap1)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  
  
  #### seconda mappa
  
  output$poltotmap2 = renderTmap({
    req(datapoltotmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapoltotmap() %>% dplyr::filter(Anno == input$yearpoltotmap2) %>% dplyr::filter(N_campionamento == input$numpoltotmap2)
    colmap = Olv_select_col(data = datapoltot(), input = input$colpoltotmap2)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  
  
  ##################################POLIFENOLI INDIVIDUALI##############################
  
  
  #data polifenoli individuali
  datapolind = reactive({
    dplyr::select(datapolif(), !c("Polifenoli_tot", "Presenza_larve"))
  })
  
  #data polifenoli individuali per mappa
  datapolindmap = reactive({
    dplyr::select(datapolifmap(), !c("Polifenoli_tot", "Presenza_larve"))
  })
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatapolind = reactive({
    req(datapolind())
    data = datapolind() %>% dplyr::select(where(is.double) & -Anno)
    return(data)
  })
  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxpolind", data = nadatapolind, text_size = 1.1)
  
  
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
  
  #dtheatsorted ha le seguenti colonne: Codice_azienda, N_campionamento, Anno, i vari polifenoli individuali 
  #e input$selectannot (che può essere Provincia, Areale o Cultivar_principale)

  
  #creo slider per colonna. Qui praticamente rimango solo con i vari polifenoli individuali
  output$sliderheatcol <- renderUI({
    req(dtheatsorted())
    len = dtheatsorted() %>% dplyr::select(where(is.double), -Anno)
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
      unit_legend = "ug/g",
      bordi = c(4,2,2,15)
    )
  })
  

  observeEvent(input$updateheat,{
    dataheat2 = dataheat()
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, dataheat2, output_id = "heatmap_output", layout = "1|23", width1 = 850, height1 = 550)
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
    
    temp = datatemp %>% dplyr::select(where(is.double), -Anno)
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
    if(input$selbiplotpolind == "Biplot"){
     temp = autoplot(pcadati(), data = dplyr::semi_join(data(), pcadatina()), shape = input$shpbiplot, colour = input$colbiplot, loadings = TRUE, loadings.colour = 'blue', 
                    loadings.label = TRUE, loadings.label.size = 4, title = "Biplot") 
    }else{
      temp = autoplot(pcadati(), data = dplyr::semi_join(data(), pcadatina()), shape = input$shpbiplot, colour = input$colbiplot, title = "Plot") 
    }
    
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
  

  
  #################### Mappa Polifenoli individuali ###########################################
  


  observeEvent(datapolind(), {
    updateSelectInput(session, "colpolindmap1", choices = colnames(datapolind()))
    updateSelectInput(session, "colpolindmap2", choices = colnames(datapolind()))
  })
  

  #aggiorna il selectinput "selyear" in base agli anni presenti
  observeEvent(datapolindmap(), {
    updateSelectInput(session, "yearpolindmap1", choices = row.names(table(dplyr::select(datapolindmap(), Anno))))
    updateSelectInput(session, "yearpolindmap2", choices = row.names(table(dplyr::select(datapolindmap(), Anno))))
    
  })
  
  
  #stampo mappa
  output$polindmap1 = renderTmap({
    req(datapolindmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapolindmap() %>% dplyr::filter(Anno == input$yearpolindmap1) %>% dplyr::filter(N_campionamento == input$numpolindmap1)
    colmap = Olv_select_col(data = datapolind(), input = input$colpolindmap1)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  
  
  #### seconda mappa
  
  output$polindmap2 = renderTmap({
    req(datapolindmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapolindmap() %>% dplyr::filter(Anno == input$yearpolindmap2) %>% dplyr::filter(N_campionamento == input$numpolindmap2)
    colmap = Olv_select_col(data = datapolind(), input = input$colpolindmap2)
    make_tmap(data = datamap, dotlegend = colmap)
  })

  
  ################# Cromatogrammi polifenoli individuali ################
  
  
  #crea la tabella
  output$prov3 = DT::renderDT(dplyr::select(data(), c("Azienda", "Codice_azienda")), selection = "single", server = FALSE, rownames = FALSE, options = list("pageLength" = 15))
  
  
  #aggiorna il selectinput "selyearfoto" in base agli anni presenti e seleziono
  observeEvent(datapolif(), {
    updateSelectInput(session, "selyearcromatph", choices = row.names(table(dplyr::select(datapolif(), "Anno"))))
  })
  
  
  #####selezionare la riga dell'azienda cromatogramma drupe
  selprovcromat = reactive({
    req(input$prov3_rows_selected)
    nroww=input$prov3_rows_selected
    x = dplyr::select(data(), "Codice_azienda")
    z = paste("www/cromatogrammi", input$selyearcromatph, input$selfilepolind, input$campcromatph, sep = "/") #selyearfoto e campfoto
    paste(z, x[nroww,], sep = "/")
  })
  
  
  #foto cromatogramma drupe
  output$phcromat = renderUI({
    croma = paste0(selprovcromat(),".jpg")
    tags$img(src = croma)
  })
  
  
  
  #################  POLIFENOLI LCxLC ###################
  
  #scegli i dati in base alla selezione
  
  datalcxlc = reactive({
    req(data())
    if(input$selfilepollc == "foglie"){
      tempdata = lcpolfoglie()
    } else if(input$selfilepollc == "drupe"){
      tempdata = lcpoldrupe()
    } else if(input$selfilepollc == "olio"){
      tempdata = lcpololio()
    } else if (input$selfilepollc == "posa"){
      tempdata = lcpolposa()
    }else{
      tempdata = lcpolsansa()
    }
    #aggiungo lo zero se il numero è una cifra
    tempdata$PEAK = formatC(tempdata$PEAK,
                           width = 2,
                           flag = "0")
    return(tempdata)
  })
  
  #data long
  datalclong = reactive({
    req(datalcxlc())
    temp = datalcxlc() %>% tidyr::gather(Codice_azienda, Quantificazione, colnames(datalcxlc()[,6:length(datalcxlc())])) %>% dplyr::select(Codice_azienda, everything())
    #ora divido il codice azienda nelle varie info (codice e Id). Id verrà poi diviso in n_camp rem ("_") e Estrazione
    temp = temp %>% tidyr::separate(Codice_azienda, into = c("Codice_azienda", "ID"), sep = 5)
    temp %>% tidyr::separate(ID, into = c("rem", "N_campionamento", "Estrazione"), sep = "_", fill = "right") %>% dplyr::select(-rem)
  })
  
  
  output$dtlcxlc = renderDT({
    req(datalclong())
    if(input$dttypelc == "Wide"){
      datalcxlc()
    }else{
      datalclong()
    }
  })
  
  
  #valutare i missing value
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatalc = reactive({
    req(datalcxlc())
    if(input$dttypelc == "Wide"){
      data = datalcxlc()
    }else{
      data = lcwidepolif()
    }
    data %>% dplyr::select(where(is.double))
  })
  

  #creo il modulo per i NA
  mod_render_NAbox_server("naboxlc", data = nadatalc, text_size = 0.8, margins = c(20,5,3,1))
  
  
  
  ###### data wide con polifenoli sulle colonne
  ###### File del tipo Codice_azienda | Cultivar_principale | Azienda | Areale | N_campionamento | Peak_01.. | Peak_02_... 
  lcwidepolif = reactive({
    temp = datalcxlc() %>% tidyr::gather(Codice_azienda, Quantificazione, colnames(datalcxlc()[,6:length(datalcxlc())])) %>% 
      dplyr::select(Codice_azienda, everything())
    #creo una seconda colonna PEAK2
    lclong_test = dplyr::select(temp,-c(3:4,6)) %>% dplyr::mutate(PEAK2 = "Peak") %>% 
      tidyr::unite(col = Compounds, PEAK2, PEAK, Compounds, sep = "_", remove = TRUE)
    
    #trasformo in wide con i polifenoli sulle colonne e codice_azienda sulle righe
    lcwidecompound = tidyr::pivot_wider(data = lclong_test, names_from = Compounds, values_from = Quantificazione)
    
    #codice azienda da SA_01_EXT_R2 deve diventare SA_01_EXT || R2
    lcpolifazienda = lcwidecompound %>% tidyr::separate(Codice_azienda, into = c("Codice_azienda", "ID"), sep = 5) %>% 
      tidyr::separate(ID, into = c("rem", "N_campionamento", "Estrazione"), sep = "_", fill = "right") %>% dplyr::select(-rem)
    
    #ora join con data() per aggiungere cultivar e azienda
    z = data() %>% dplyr::select(Codice_azienda, Azienda, Cultivar_principale, Provincia, Areale)   #qui ho aggiunto provincia e areale
    lcpolifazienda = dplyr::right_join(x = z, y = lcpolifazienda, by = "Codice_azienda")
    
    #gli NA in Estrazione diventano ""
    lcpolifazienda$Estrazione[is.na(lcpolifazienda$Estrazione)] = ""
    
    #unisco codice_azienda con estrazione
    lcpolifazienda2 = tidyr::unite(lcpolifazienda, col = Codice_azienda, Codice_azienda, Estrazione, sep = "_", remove = TRUE)
    #e se estrazione è vuoto invece di avere SA_01_ tolgo il "_" finale.
    for(i in seq(1:length(lcpolifazienda2$Codice_azienda))){
      if(nchar(lcpolifazienda2$Codice_azienda[i]) == 6){
        lcpolifazienda2$Codice_azienda[i] = substring(lcpolifazienda2$Codice_azienda[i],1,5)
      }
    }
    return(lcpolifazienda2)
  })
  
 
  #crea la tabella
  output$dtfotolc = DT::renderDT({
    ncamp = ifelse(input$ncampcromatlc == "1_campionamento", "R1", "R2")
    lcwidepolif() %>% filter(N_campionamento == ncamp) %>% dplyr::select(c("Codice_azienda", "Cultivar_principale", "Azienda"))
    }, selection = "single", server = FALSE, rownames = FALSE)
  
  

  #foto cromatogramma drupe
  output$phcromatlc = renderUI({
    req(input$dtfotolc_rows_selected)
    nroww=input$dtfotolc_rows_selected
    ncamp = ifelse(input$ncampcromatlc == "1_campionamento", "R1", "R2")
    
    cod = lcwidepolif() %>% filter(N_campionamento == ncamp) %>% dplyr::select("Codice_azienda")
    path = paste("www/cromatogrammi_LCxLC/2020", input$selfilepollc, input$ncampcromatlc, cod[nroww,], sep = "/")
    croma = paste0(path,".png")
    tags$img(src = croma, style="width: 800px")

  })
  
  #crea la tabella dei polifenoli dell'azienda selezionata
  output$poliffotolc = DT::renderDT({
    req(input$dtfotolc_rows_selected)
    nroww=input$dtfotolc_rows_selected
    ncamp = ifelse(input$ncampcromatlc == "1_campionamento", "R1", "R2")
    #prendo un'azienda filtro e tolgo le colonne in più. Se è vuota, NULL
    cod = dplyr::filter(lcwidepolif(), N_campionamento == ncamp) 
    cod = cod[nroww,] %>%  dplyr::select(-c(1:4)) 
    hhh = tidyr::pivot_longer(cod, cols = starts_with("Peak"), names_to = "Compounds", values_to = "Quantizzazione (mg/Kg)")
    h2 = hhh %>% tidyr::separate(Compounds, into = c("Peak", "Compounds"), sep = 8)
    h2 %>% tidyr::separate(Peak, into = c("rem", "Peak"), sep = "_") %>% dplyr::select(-c(1:3))
  })
  
  
  
  ####### Grafici LCxLC######
  
  #data da usara per i grafici. 
  #Se per azienda, avrò un'azienda, la colonna compound e la colonna quantizzazione.
  #Se per polifenolo, avrò la colonna Codice_azienda con tutte le aziende e una colonna di un polifenolo (es.Peak_1_Acid_gallic)
  
  
  observeEvent(lcwidepolif(), {
    updateSelectInput(session, "lcselaziendascatt", choices = unique(lcwidepolif()$Codice_azienda))
    updateSelectInput(session, "lcselpolifscatt", choices = colnames(dplyr::select(lcwidepolif(), starts_with("Peak"))))
    updateSelectInput(session, "lcselcultscatt", choices = unique(lcwidepolif()$Cultivar_principale))
  })
  
  #questo mi serve per aggiornare il ncamp in base alla selezione
  datalcgraphcamp = reactive({
    req(lcwidepolif())
    if(input$lcdatatypescatt == "Azienda"){
      lcwidepolif() %>% dplyr::filter(Codice_azienda == input$lcselaziendascatt)
    }else if(input$lcdatatypescatt == "Cultivar principale"){
      lcwidepolif() %>% dplyr::filter(Cultivar_principale == input$lcselcultscatt)
    }else{
      lcwidepolif()
    }
  })
  
  observeEvent(datalcgraphcamp(), {
    updateSelectInput(session, "numscattlc", choices = unique(datalcgraphcamp()$N_campionamento))
  })
  
  
  datalcgraph = reactive({
    datancamp = datalcgraphcamp() %>% dplyr::filter(N_campionamento == input$numscattlc)

    if(input$lcdatatypescatt == "Polifenolo"){
      datancamp = datancamp %>% dplyr::select(-dplyr::starts_with("Peak"), input$lcselpolifscatt)
      datancamp = datancamp %>% dplyr::mutate(Presenza = dplyr::case_when(
        dplyr::select(datancamp, input$lcselpolifscatt) == 0 ~ "Assente",
        dplyr::select(datancamp, input$lcselpolifscatt) == -1 ~ "<LOQ",
        TRUE ~ "Presente"
      ))
    }else{
      datancamp = tidyr::gather(datancamp, Compounds, Quantificazione, colnames(dplyr::select(datancamp, starts_with("Peak"))))
      if(input$sintscattlc == TRUE){
        datancamp = datancamp %>% dplyr::group_by(dplyr::across("Compounds")) %>% 
          dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE), N_aziende = dplyr::n()) %>% dplyr::ungroup() %>%
          dplyr::mutate(Quantificazione = dplyr::case_when(Quantificazione < 0 ~ -1, TRUE ~ Quantificazione))
      }
      datancamp = datancamp %>% dplyr::mutate(Presenza = dplyr::case_when(
        Quantificazione == 0 ~ "Assente",
        Quantificazione == -1 ~ "<LOQ",
        TRUE ~ "Presente"   #TRUE sarebbe l'equivalente di else quindi in tutti gli altri casi diventa "Presente"
      ))
      if(input$logscattlc == TRUE){
        #qui modifico i valori di quantificazione per evitare problemi col logaritmo in ggplot. 
        #Se il valore è -1 (<LOQ) lo trasformo in 0.5 così non esce errore e il log diventa un numero negativo, 
        #se è 0 diventa 1 (così se il valore è 0 il logaritmo non esce -inf ma esce 0), altrimenti rimane uguale. 
        #Nota: log(0) = inf e log(-1) errore.
        datancamp = datancamp %>% dplyr::mutate(Quantificazione = ifelse(Quantificazione == -1, 0.5, 
                                                                         ifelse(Quantificazione == 0, 1, Quantificazione)))
      }
      
    }

    datancamp$Presenza = factor(datancamp$Presenza, levels = c("<LOQ", "Assente", "Presente"), ordered = FALSE)
    datancamp
  })
  
  
  
  observeEvent(datalcgraph(), {
    updateSelectInput(session, "fillscattlc", choices = colnames(dplyr::select(datalcgraph(), -dplyr::any_of(c(dplyr::starts_with("Peak"), "N_campionamento", "Presenza")))))
    updateSelectInput(session, "lcselpolbar", choices = c("Tutti", unique(datalcgraph()$Compounds)), selected = "Tutti")
    
    
  })
  

  # Scatterplot
  
  #aggiungo na.omit() nei colori così se ci sono NA non li conta nella scelta dei colori e non da errore
  output$scatterlc = renderPlotly({
    
    if(input$logscattlc == TRUE){
      transf = "log10"
      yname = "Log quantificazione (mg/Kg)"
    }else{
      transf = "identity"
      yname = "Quantificazione (mg/Kg)"
      }
    #ora plotly mi mostra nel tooltip il log e non il valore normale (in barplot non succede). Per risolvere
    #creo una label in ggplot e poi la riporto nei tooltip in ggplotly
    pos_jitter = ifelse(input$lcdatatypescatt == "Cultivar principale", "jitter", "identity")
    size_points = ifelse(input$lcdatatypescatt == "Cultivar principale", 2, 3)
    if(input$lcdatatypescatt == "Polifenolo"){
      temp = ggplot(data = datalcgraph()) + 
        geom_count(mapping = aes_string(x = "Codice_azienda", y = input$lcselpolifscatt, shape = "Presenza", color = input$fillscattlc)) + #,color = grDevices::hcl.colors(length(na.omit(dplyr::select(datalcgraph(),input$lcselpolifscatt))), palette = "Dynamic")
        scale_shape_manual(values=c(10, 1, 16),drop = FALSE, labels = c("<LOQ", "Assente", "Presente")) + 
        theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + ylab(paste(input$lcselpolifscatt, "(mg/Kg)"))
      plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = "Presenza")))
    }else{
      temp = ggplot(data = datalcgraph(), aes_string(label = "Quantificazione")) + 
        geom_count(mapping = aes_string(x = "Compounds", y = "Quantificazione", shape = "Presenza", color = input$fillscattlc), size = size_points, position = pos_jitter) + #, color = grDevices::hcl.colors(length(na.omit(datalcgraph()$Quantificazione)), palette = "Dynamic")
        scale_shape_manual(values=c(10, 1, 16), drop = FALSE, labels = c("<LOQ", "Assente", "Presente")) + 
        theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(yname) +
        scale_y_continuous(trans = transf)
      plotly::ggplotly(temp, tooltip = c("Compounds", "Presenza", "label", input$fillscattlc)) %>% plotly::layout(legend = list(title = list(text = "Legenda"))) 
    }
    

  })
  
  
  # Barplot
  output$barplotlc = renderPlotly({
    if(input$logscattlc == TRUE){
      transf = "log10"
      yname = "Log quantificazione (mg/Kg)"
    }else{
      transf = "identity"
      yname = "Quantificazione (mg/Kg)"
      }
    
    if(input$lcdatatypescatt == "Polifenolo"){
      temp = ggplot(data=datalcgraph()) + 
        geom_col(mapping = aes_string(x = "Codice_azienda", y = input$lcselpolifscatt, fill = input$fillscattlc, linetype = "Presenza")) + 
        theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(paste(input$lcselpolifscatt, "(mg/Kg)"))
      plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$fillscattlc))) 
    }else{
      if(input$lcselpolbar != "Tutti"){
        data = datalcgraph() %>% dplyr::filter(Compounds == input$lcselpolbar)
      }else{data = datalcgraph()}
      
     temp = ggplot(data) + 
      geom_col(mapping = aes_string(x = "Compounds", y = "Quantificazione", fill = input$fillscattlc, linetype = "Presenza"), position = input$bartypelc) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(yname)+
      scale_y_continuous(trans = transf)
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$fillscattlc)))  
    }
    
  })
  
  
  
  
  ############# HEATMAP LCxLC
  
  #lcwidepolif() =  Codice_azienda | Cultivar_principale | Azienda | Provincia | Areale | N_campionamento | Peak_01.. | Peak_02_... 
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti
  observeEvent(lcwidepolif(), {
    #updateSelectInput(session, "selyearheatmorfo", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
    updateSelectInput(session, "numheatlc", choices = unique(lcwidepolif()$N_campionamento))
    updateSelectInput(session, "cultheatlc", choices = c("Tutte", unique(lcwidepolif()$Cultivar_principale)))
    
  })
  
  lcheatsorted = reactive({
    #dato che qui codice_azienda è diverso, non posso usare sorderdata() ma devo farlo a mano.
    #Filtro e tolgo Azienda e areale e poi faccio scegliere uno tra provincia e cultivar
    if(input$cultheatlc != "Tutte"){
      dtfilterd = lcwidepolif() %>% dplyr::filter(Cultivar_principale == input$cultheatlc)
    }else{
      dtfilterd = lcwidepolif()
    }
    
    dtfilterd = dtfilterd %>% dplyr::filter(N_campionamento == input$numheatlc) %>% dplyr::select(-c(Azienda, Areale))
    if(input$selectannotlc == "Provincia"){
      dtfilterd = dtfilterd %>% dplyr::select(-Cultivar_principale)
    }else{
      dtfilterd = dtfilterd %>% dplyr::select(-Provincia)
    }
    seletannota = input$selectannotlc
    if(input$heatsortlc == TRUE){
      dtfilterd[do.call(order, dtfilterd[as.character(seletannota[1])]), ]
    } else{
      return(dtfilterd)
    }
  })
  
  #lcheatsorted ha le seguenti colonne: Codice_azienda, N_campionamento, [Anno (non c'è qui)], i vari picchi 
  #e input$selectannotlc (che può essere Provincia o Cultivar_principale).Ho eliminato year = "..."
  #così di default è null e non filtra per anno
  
  
  #creo slider per colonna. Qui praticamente rimango solo con i vari polifenoli individuali
  output$slidercolheatlc <- renderUI({
    req(lcheatsorted())
    len = lcheatsorted() %>% dplyr::select(where(is.double)) #ho tolto "-Anno"
    sliderInput("slidercolheatlc", "Numero cluster:", min=2, max=length(len), value=2, step = 1)
  })
  

  
  #creo l'heatmap
  dataheatlc = reactive({
    make_heatmap(
      datasorted = lcheatsorted(),
      add_annot = input$selectannotlc,
      scale_data = input$selscaleheatlc,
      dist_method = input$seldistheatlc,
      clust_method = input$selhclustheatlc,
      row_dend = input$rowdendlc,
      row_nclust = input$sliderrowheatlc,
      col_dend = input$columndendlc,
      col_nclust = input$slidercolheatlc,
      col_lab = "Compounds",
      unit_legend = "mg/Kg",
      year_presence = FALSE,
      col_label_size = 11.5,
      bordi = c(25,2,2,10)
    )
  })
  
  
  observeEvent(input$updateheatlc,{
    dataheat2 = dataheatlc()
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, dataheat2, output_id = "heatmap_outputlc", layout = "1|23", width1 = 1150, height1 = 700)
  })
  
  
  ####################### PCA LCxLC #######################
  
  observeEvent(lcwidepolif(), {
    updateSelectInput(session, "numcamppcalc", choices = unique(lcwidepolif()$N_campionamento))
  })
  

  #rimuovo le righe di NA se ci sono e faccio la PCA. Dato che ci sono colonne con soli zero uso dplyr::select(where(~ any(. != 0)))
  #per selezionare le colonne che non hanno tutti 0.
  pcadatiLC = reactive({
    req(lcwidepolif())
    #scelgo il num campionamento e faccio la pca stavolta con prcomp perchè ci sono più colonne che righe.
    lcwidepolif() %>% dplyr::filter(N_campionamento == input$numcamppcalc) %>% 
      dplyr::select(Codice_azienda, where(is.double)) %>% dplyr::select(where(~ any(. != 0))) %>% stats::na.exclude() %>%
      as.data.frame() %>% tibble::column_to_rownames("Codice_azienda") %>% stats::prcomp(scale = input$scalepcalc) #-Anno e non c'è cor =...
  })

  

  
  #slider dinamico per la scelta delle pcs
  output$sliderpclc <- renderUI({
    req(pcadatiLC())
    pca = pcadatiLC()
    sliderInput("selpcslc", "Numero di Componenti Principali (PC)", min=1, max=length(pca$sdev), value=2, step = 1)
  })
  
  
  
  ###plot loadings
  output$loadingslc = plotly::renderPlotly({
    req(pcadatiLC())
    pca = pcadatiLC()
    loadpca = as.data.frame(pca$rotation[, input$selpcslc]) #invece di loadings ci sono i rotation
    loadpca = tibble::rownames_to_column(loadpca)
    
    pcasdev = as.data.frame(round(pca$sdev^2/sum(pca$sdev^2)*100, 2))
    
    colnames(loadpca) = c("Compounds", paste0("PC", input$selpcslc))
    loadplot = ggplot(loadpca) + geom_col(aes(x = Compounds, y = loadpca[,2], fill = Compounds)) +
      labs(y = paste0("PC", input$selpcslc, " ", "(", pcasdev[as.numeric(input$selpcslc), ], "%", ")"), title = "Loadings") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
    plotly::ggplotly(loadplot)
  })
  
  ###screeplot
  output$screeplotlc <- plotly::renderPlotly({
    pca = pcadatiLC()
    var = cumsum(100*pca$sdev^2/sum(pca$sdev^2)) 
    var = as.data.frame(cbind(var)) %>% tibble::rownames_to_column()
    colnames(var) = c("Componenti_principali", "Varianza_spiegata")
    #non so perchè non ordina le componenti e devo farlo io
    var$Componenti_principali = factor(var$Componenti_principali, levels = c(1:length(var$Componenti_principali)))
    
    
    screegg = ggplot(var, aes(Componenti_principali,Varianza_spiegata)) +
      geom_line(colour = "red", group = 1, linetype = "dashed", size = 1) + geom_point(size = 4, colour = "red") + 
      labs(x = "Componenti principali", y = "Varianza spiegata (%)", title = "Screeplot") +
      scale_y_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10)))
    plotly::ggplotly(screegg)
    
  })
  
  
  ###biplot
  output$biplotlc = plotly::renderPlotly({
    req(pcadatiLC())
    #dato che a monte ho eliminato le righe dove non c'erano valori nei picchi (i double), faccio lo stesso qui con drop_na()
    datilabel = lcwidepolif() %>% dplyr::filter(N_campionamento == input$numcamppcalc) %>% tidyr::drop_na(starts_with("Peak"))
    if(input$selbiplotlc == "Biplot"){
      temp = autoplot(pcadatiLC(), data = datilabel, shape = input$shpbiplotlc, colour = input$colbiplotlc, loadings = TRUE, loadings.colour = 'blue', 
                      loadings.label = TRUE, loadings.label.size = 4, title = "Biplot")
    }else{
      temp = autoplot(pcadatiLC(), data = datilabel, shape = input$shpbiplotlc, colour = input$colbiplotlc, title = "Plot")
    }
    
    
    plotly::ggplotly(temp)
  })
  
  
  ### plot 3D
  output$pca3dlc = plotly::renderPlotly({
    req(pcadatiLC())
    pc = pcadatiLC()
    scoreind = pc$x #qui invece degli scores ci sono gli x
    scoreindtb = scoreind %>% as.data.frame() %>% tibble::rownames_to_column("Codice_azienda") %>% tibble::as_tibble()
    
    #datilabel = lcwidepolif() %>% dplyr::filter(N_campionamento == input$numcamppcalc)
    scoreindjoin = dplyr::left_join(x = scoreindtb, y = dplyr::select(lcwidepolif(), Codice_azienda, Provincia, Cultivar_principale, Areale), by = "Codice_azienda")
    #invece di Comp.1, Comp.2 e Comp.3 qui ho PC1, PC2 e PC3
    scoreindjoin %>% plotly::plot_ly(x = ~PC1, y = ~PC2, z= ~PC3, type = "scatter3d", mode = "markers", color = ~base::get(input$col3dlc))
    
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
  
  
  ################# Foto campioni ############
  
  #crea la tabella
  output$dtfotomorfo = DT::renderDT(dplyr::select(data(), c("Codice_azienda", "Cultivar_principale", "Azienda")), selection = "single", server = FALSE, rownames = FALSE)
  
  
  #aggiorna il selectinput "selyearfotomorfo" in base agli anni presenti e seleziono
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearfotomorfo", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
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
  
  ######## Grafici morfo ##########

  observeEvent(datamorfo(), {
    #boxplot e barplot
    updateSelectInput(session, "selectymorfobb", choices=colnames(dplyr::select(datamorfo(), where(is.double) & -dplyr::any_of(c("Anno", "ID_oliva")))))

    
    #grafici IOC
    updateSelectInput(session, "selectfillmorfoioc", choices = colnames(dplyr::select(datamorfo(), ends_with("(IOC)"))))
    updateSelectInput(session, "selectfillmorfoioc2", choices = colnames(dplyr::select(datamorfo(), ends_with("(IOC)"))))
    
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
    summ = datamorfo() %>% dplyr::group_by(Codice_azienda, dplyr::across(c(colnames(x), colnames(fill)))) %>%
      dplyr::summarise(across(where(is.double), mean , na.rm = TRUE), n = dplyr::n()) %>%
      dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% dplyr::select(!starts_with("ID"))
    
    temp = ggplot(data = summ, mapping = aes_string(x = colnames(x), y = paste0("`",colnames(y), "`"), 
                                                   fill = paste0("`",colnames(fill),"`"))) + geom_col() + 
      ggplot2::stat_summary(data = datamorfo(), geom = "errorbar", fun.data = mean_se) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  
  
  ### Boxplot

  output$boxmorfo = renderPlotly({
    req(datamorfo())
    y =  Olv_select_col(data = datamorfo(), input = input$selectymorfobb)
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfobb)
    temp = ggplot(data = datamorfo(), 
                  mapping = aes_string(x = input$selectxmorfobb, y = paste0("`",colnames(y), "`"), fill = paste0("`",colnames(fill),"`"))) + 
    geom_boxplot() + geom_jitter(width = 0.3) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
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
  
  
  
  # Grafici IOC
  
  output$iocmorfo = renderPlotly({
    req(datamorfo())
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfoioc)
    if(input$selplotioc == "2"){
      if(input$iocselfreq == "Frequenza assoluta"){
        temp = ggplot(data = datamorfo(), mapping = aes_string(x = input$selectxmorfoioc, fill = paste0("`",colnames(fill),"`"))) + 
          geom_bar(stat = "count") + scale_y_continuous(breaks = scales::pretty_breaks()) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
      } else{
        temp = ggplot(data = datamorfo(), mapping = aes_string(x = input$selectxmorfoioc, fill = paste0("`",colnames(fill),"`"))) + 
          geom_bar(stat = "count", position = "fill") + scale_y_continuous(breaks = scales::pretty_breaks()) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
      }
      plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
    } else{
     
      datamorfo() %>% plotly::plot_ly(labels = ~base::get(colnames(fill)), type= "pie", textposition = 'inside', textinfo = 'label+value',
                                       marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)), 
                                       showlegend = TRUE) %>% plotly::layout(title = paste("Frequenza assoluta di ", colnames(fill)))
    }
  })
    
  
  #secondo grafico IOC
  output$iocmorfo2 = renderPlotly({
    req(datamorfo())
    fill = Olv_select_col(data = datamorfo(), input = input$selectfillmorfoioc2)
    if(input$selplotioc2 == "2"){
      if(input$iocselfreq2 == "Frequenza assoluta"){
        temp = ggplot(data = datamorfo(), mapping = aes_string(x = input$selectxmorfoioc2, fill = paste0("`",colnames(fill),"`"))) + 
          geom_bar(stat = "count") + scale_y_continuous(breaks = scales::pretty_breaks()) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
      } else{
        temp = ggplot(data = datamorfo(), mapping = aes_string(x = input$selectxmorfoioc2, fill = paste0("`",colnames(fill),"`"))) + 
          geom_bar(stat = "count", position = "fill") + scale_y_continuous(breaks = scales::pretty_breaks()) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
      }
      plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
    } else{
      
      datamorfo() %>% plotly::plot_ly(labels = ~base::get(colnames(fill)), type= "pie", textposition = 'inside', textinfo = 'label+value',
                                      marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)), 
                                      showlegend = TRUE) %>% plotly::layout(title = paste("Frequenza assoluta di ", colnames(fill)))
    }
  })
  
  #### Heatmap morfo ####
  
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
    len = dtheatsortedmorfo() %>% dplyr::select(where(is.double), -Anno)
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
      col_lab = "Misure",
      bordi = c(6,2,2,15)
      
    )
  })
  
  
  observeEvent(input$updateheatmorfo,{
    dataheat2 = dataheatmorfo()
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, dataheat2, output_id = "heatmap_outputmorfo", layout = "1|23", width1 = 850, height1 = 600)
  })
  

  
  ###### Correlation plot morfo ######
  
  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearcorrmorfo", choices = row.names(table(dplyr::select(datamorfo(), "Anno"))))
  })
  
  
  ###creo il corrplot
  output$corrplotmorfo = plotly::renderPlotly({
    req(datamorfo())
    ###scegliere anno e  il campionamento (scatter plot)
    datatemp = datamorfo() %>% dplyr::filter(Anno == input$selyearcorrmorfo) 
    
    temp = datatemp %>% dplyr::select(where(is.double), -Anno)
    temp2 = round(stats::cor(temp, use = "na.or.complete"),1)
    par(xpd = TRUE)
    
    ggcorrplot::ggcorrplot(temp2, hc.order = TRUE, type = "lower", outline.col = "white", show.diag = FALSE) %>% plotly::ggplotly()
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
    if(input$selbiplotmorfo == "Biplot"){
     temp = autoplot(pcadatimorfo(), data = datacolorpcamorfo(), shape = input$shpbiplotmorfo, colour = input$colbiplotmorfo, loadings = TRUE, loadings.colour = 'blue',
                    loadings.label = TRUE, loadings.label.size = 4, title = "Biplot") 
    }else{
      temp = autoplot(pcadatimorfo(), data = datacolorpcamorfo(), shape = input$shpbiplotmorfo, colour = input$colbiplotmorfo, title = "Plot")
    }
    
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
  
  
  
  ###### Test d'ipotesi #####################
  
  #dati filtrati per anno
  observeEvent(datamorfo(), {
    updateSelectInput(session, "selyearttestmorfo", choices = row.names(table(dplyr::select(datamorfo(), Anno))))
  })
  
  datamorfoyeartest = reactive({
    datamorfo() %>% dplyr::filter(Anno == input$selyearttestmorfo)
  })
  
  ###### t-test 
  #aggiorna il selectinput "selvarttest" in base ai double presenti
  observeEvent(datamorfoyeartest(), {
    updateSelectInput(session, "catvarttest", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.character))))
    updateSelectInput(session, "numvarttest", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.double), -Anno, -starts_with("ID"))))
  })
  
  #culttest1 e 2
  observeEvent(input$catvarttest, {
    updateSelectInput(session, "culttest1", choices = unique(dplyr::select(datamorfoyeartest(), input$catvarttest)))
    updateSelectInput(session, "culttest2", choices = unique(dplyr::select(datamorfoyeartest(), input$catvarttest)))
  })
  
  
   #creo la variabile dei dati con le due opzioni
  datattest = reactive({
    req(datamorfoyeartest())
    datamorfoyeartest() %>% dplyr::filter(.data[[input$catvarttest]] %in% c(input$culttest1, input$culttest2))
    #datamorfo()[datamorfo()[[input$catvarttest]] %in% c(input$culttest1, input$culttest2),]
    
  })
  

  #test normalità con shapiro test
  shapiro1data = reactive({
    req(datamorfoyeartest())
    #shp1 = datamorfo()[datamorfo()[[input$catvarttest]] %in% input$culttest1,] %>% dplyr::pull(input$numvarttest) %>% 
     # stats::shapiro.test()
    shp1 = datamorfoyeartest() %>% dplyr::filter(.data[[input$catvarttest]] %in% input$culttest1) %>%
      dplyr::pull(input$numvarttest) %>%  stats::shapiro.test()
    shp1$data.name = paste(input$culttest1)
    shp1
  })
  
  output$shapiro1 = renderPrint({
    shapiro1data()
  })
  
  shapiro2data = reactive({
    req(datamorfoyeartest())
    shp2 = datamorfoyeartest() %>% dplyr::filter(.data[[input$catvarttest]] %in% input$culttest2) %>%
      dplyr::pull(input$numvarttest) %>%  stats::shapiro.test()
    shp2$data.name = paste(input$culttest2)
    shp2
  })
  
  output$shapiro2 = renderPrint({
    shapiro2data()
  })
  

  #output per l'ui
  output$shapttestmorfoui = reactive({
    if(shapiro1data()$p.value < 0.05 && shapiro2data()$p.value < 0.05){
      "distribuzione normale"
    }else{"distribuzione non normale"}
  })
  outputOptions(output, 'shapttestmorfoui', suspendWhenHidden = FALSE)
  
  
  #test varianza F-test
  output$vartest1 = renderPrint({
    req(datattest())
    num = datattest() %>% dplyr::pull(input$numvarttest)
    cat = datattest() %>% dplyr::pull(input$catvarttest)
    var1 = stats::var.test(num ~ cat)
    var1$data.name = paste(input$numvarttest, "~", input$catvarttest)
    var1
  })
  
  
  #Boxplot
  output$boxttest = plotly::renderPlotly({
    req(datattest())
    y =  Olv_select_col(data = datattest(), input = input$numvarttest)
    fill = Olv_select_col(data = datattest(), input = input$catvarttest)
    
    temp = ggplot(data = datattest(), 
                  mapping = aes_string(x = paste0("`",colnames(fill), "`"), y = paste0("`",colnames(y), "`"), fill = paste0("`",colnames(fill),"`"))) + 
      geom_boxplot() + geom_jitter(width = 0.3) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  #T-test
  output$ttest1 = renderPrint({
    req(datattest())
    num = datattest() %>% dplyr::pull(input$numvarttest)
    cat = datattest() %>% dplyr::pull(input$catvarttest)
    if(input$selectttest == "T-test"){
      test = stats::t.test(num ~ cat, var.equal = input$selvarequal)
      test$data.name = paste(input$numvarttest, "~", input$catvarttest)
    } else{
      test = stats::wilcox.test(num ~ cat)
      test$data.name = paste(input$numvarttest, "~", input$catvarttest)
    }
    test
  })
  

  ######################### correlation test ____________________________
  
  #aggiorna il selectinput in base ai double presenti
  observeEvent(datamorfoyeartest(), {
    updateSelectInput(session, "corrtest1", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.double), -Anno, -starts_with("ID"))))
    updateSelectInput(session, "corrtest2", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.double), -Anno, -starts_with("ID"))))
    
    updateSelectInput(session, "corrtestfill", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.character))))
    
  })
  
  #test normalità con shapiro test
  output$shapirocorr1 = renderPrint({
    req(datamorfoyeartest())
    shp1 = datamorfoyeartest() %>% dplyr::pull(input$corrtest1) %>% stats::shapiro.test()
    shp1$data.name = paste(input$corrtest1)
    shp1
  })
  
  output$shapirocorr2 = renderPrint({
    req(datamorfoyeartest())
    shp2 = datamorfoyeartest() %>% dplyr::pull(input$corrtest2) %>% stats::shapiro.test()
    shp2$data.name = paste(input$corrtest2)
    shp2
  })

  #selectcorrtest
  
  #T-test
  output$corrtest = renderPrint({
    req(datamorfoyeartest())
    x = datamorfoyeartest() %>% dplyr::pull(input$corrtest1)
    y = datamorfoyeartest() %>% dplyr::pull(input$corrtest2)
    
    test = stats::cor.test(x, y, method = input$selectcorrtest)
    test$data.name = paste(input$corrtest1, "and", input$corrtest2)
    test
  })
  
  
  #scatterplot
  output$scattcorrtest = plotly::renderPlotly({
    req(datamorfoyeartest())
    x = Olv_select_col(data = datamorfoyeartest(), input = input$corrtest1)
    y =  Olv_select_col(data = datamorfoyeartest(), input = input$corrtest2)
    fill = Olv_select_col(data = datamorfoyeartest(), input = input$corrtestfill)
    
    #voglio un singol fit (linea della regressione lineare) o più fit?
    if(input$numfitcorrtest == FALSE){
      temp = ggplot(data = datamorfoyeartest(), 
                  mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"))) + 
      geom_point(mapping = aes_string(color = paste0("`",colnames(fill), "`"))) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + 
      geom_smooth(method = lm, mapping = aes_string(fill = paste0("`",colnames(fill), "`")), se = input$selsecorrtest)
    } else{
    temp = ggplot(data = datamorfoyeartest(),
                  mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"))) +
      geom_point(mapping = aes_string(color = paste0("`",colnames(fill), "`"))) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + 
      geom_smooth(method=lm, se = input$selsecorrtest)

    }
   
    plotly::ggplotly(temp) 
  })
  
  
  
  ########################## TEST INDIPENDENZA CHI QUADRO E FISHER _________
  
  observeEvent(datamorfo(), {
    #IOC
    updateSelectInput(session, "chisqtest2", choices = colnames(dplyr::select(datamorfo(), ends_with("(IOC)"))))
    
    #filtro
    updateSelectInput(session, "chisqtestfilt", choices = row.names(table(dplyr::select(datamorfo(), "Cultivar_principale"))))
  })
  
  #data filtered or not
  datachisqmorfo = reactive({
    if(!is.null(input$chisqtestfilt)){
      datamorfo() %>% dplyr::filter(Cultivar_principale %in% input$chisqtestfilt)
    }else{
      datamorfo()
    }
  })
  
  #tabella contingenza
  output$contingtablemorfo = renderTable({
    req(datachisqmorfo())

    x = datachisqmorfo() %>% dplyr::pull(input$chisqtest1)
    ioc = datachisqmorfo() %>% dplyr::pull(input$chisqtest2)
    as.data.frame.matrix(table(x, ioc))
  }, striped=TRUE, bordered = TRUE,rownames = T)
  
  
  #test indipendenza
  
  chisqmorfo = reactive({
    req(datachisqmorfo())
    x = datachisqmorfo() %>% dplyr::pull(input$chisqtest1)
    ioc = datachisqmorfo() %>% dplyr::pull(input$chisqtest2)
    if(input$selectchisqtest == "Test d'indipendenza Chi-quadro"){
      stats::chisq.test(table(x, ioc), simulate.p.value = input$simulatechisq)
    } else{
      stats::fisher.test(table(x, ioc), simulate.p.value = input$simulatechisq)
    }
  })
  

  
  #output per l'ui
  signiftestchisqmorfo = reactive({
    if(chisqmorfo()$p.value < input$pvalchisqmorfo){
      "significativo"
    } else{
      "non significativo"
    }
  })
  

  output$signiftestchisqmorfoui = reactive({
    signiftestchisqmorfo()
  })
  outputOptions(output, 'signiftestchisqmorfoui', suspendWhenHidden = FALSE)
  

  output$chisqmorfoprint = renderPrint({
    chisqmorfo()
    })

  
  #corrplot dei residui di chi-square

  output$plotresidchisq = renderPlot({
    if(input$selectchisqtest == "Test d'indipendenza Chi-quadro"){
      col2 = grDevices::colorRampPalette(c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#FFFFFF", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"))
      corrplot::corrplot(t(chisqmorfo()$residuals), is.cor = FALSE, mar = c(1, 1, 1, 1), col = col2(200), tl.srt = 45, title = "Corrplot dei residui",cl.lim = c(-max(abs(chisqmorfo()$residuals)), max(abs(chisqmorfo()$residuals))))
    }

  })

  

  
  ############################# ANOVA ____________
  #aggiorna il selectinput "selvarttest" in base ai double presenti
  observeEvent(datamorfoyeartest(), {
    updateSelectInput(session, "anovanum", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.double), -Anno, -starts_with("ID"))))
    updateSelectInput(session, "anovacat", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.character), -c(Azienda, N_campionamento, starts_with("ID")))), selected = "Cultivar_principale")
    updateSelectInput(session, "anovacat2", choices = colnames(dplyr::select(datamorfoyeartest(), where(is.character), -c(Azienda, N_campionamento, starts_with("ID")))))
  })
  
  observe({
    if(input$selectanovatest == "Two-way ANOVA"){
      updateAwesomeRadio(session, "selectanovatest2", choices = "ANOVA", selected = "ANOVA")
    }else{
      updateAwesomeRadio(session, "selectanovatest2", choices = c("ANOVA", "Kruskal-Wallis"))
    }
  })
  
  #test normalità con shapiro test
  shapiroanova1data = reactive({
    req(datamorfoyeartest())
    shp1 = datamorfoyeartest() %>% dplyr::pull(input$anovanum) %>% stats::shapiro.test()
    shp1$data.name = paste(input$anovanum)
    shp1
  })
  
  output$shapiroanova1 = renderPrint({
    shapiroanova1data()
  })
  
  
  #output per l'ui
  output$shapanovamorfoui = reactive({
    if(shapiroanova1data()$p.value < 0.05){
      "distribuzione normale"
    }else{"distribuzione non normale"}
  })
  outputOptions(output, 'shapanovamorfoui', suspendWhenHidden = FALSE)
  
  
  
  #Boxplot anova
  output$boxanova = plotly::renderPlotly({
    req(datamorfoyeartest())
    y =  Olv_select_col(data = datamorfoyeartest(), input = input$anovanum)
    x = Olv_select_col(data = datamorfoyeartest(), input = input$anovacat)
    
    temp = ggplot(data = datamorfoyeartest(), 
                  mapping = aes_string(x = paste0("`",colnames(x), "`"), y = paste0("`",colnames(y), "`"), fill = paste0("`",colnames(x),"`"))) + 
      geom_boxplot() + geom_jitter(width = 0.3) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(x))))
  })
  

  anova1morfo = reactive({
    req(datamorfoyeartest())
    var_numerica = datamorfoyeartest() %>% dplyr::pull(input$anovanum)
    var_categorica = datamorfoyeartest() %>% dplyr::pull(input$anovacat)
    if(input$selectanovatest == "Two-way ANOVA" ){
      var_categorica1 = datamorfoyeartest() %>% dplyr::pull(input$anovacat)
      var_categorica2 = datamorfoyeartest() %>% dplyr::pull(input$anovacat2)
      if(input$anova2typemorfo == "Modello additivo"){
        stats::aov(var_numerica ~ var_categorica1 + var_categorica2)
      }else{
       stats::aov(var_numerica ~ var_categorica1 * var_categorica2) 
      }
    }else{
      stats::aov(var_numerica ~ var_categorica)
    }
  })
  
  output$anova1morfoprint = renderPrint({
    summary(anova1morfo())
  })
  

  
  #kruskal-wallis data
  kruskmorfodata = reactive({
    req(datamorfoyeartest())
    var_numerica = datamorfoyeartest() %>% dplyr::pull(input$anovanum)
    var_categorica = datamorfoyeartest() %>% dplyr::pull(input$anovacat)
    kru = stats::kruskal.test(var_numerica ~ var_categorica)
    kru$data.name = paste(input$anovanum, "by", input$anovacat)
    kru
  })
  #kruskal-wallis
  output$kruskmorfo = renderPrint({
    kruskmorfodata()
  })
  
  #variabile che mi dice se il test è significativo
  signiftestmorfo = reactive({
    anovasumm = summary(anova1morfo())
    if(round(anovasumm[[1]][["Pr(>F)"]][[1]], digits = 4) <= input$pvalanovamorfo ||  kruskmorfodata()$p.value <= input$pvalanovamorfo){
      "significativo"
    } else{
      "non significativo"
    }
  })
  
  #output per l'ui
  output$signiftestmorfoui = reactive({
    signiftestmorfo()
  })
  outputOptions(output, 'signiftestmorfoui', suspendWhenHidden = FALSE)
  
  #post hoc tukey o dunn
  posthocmorfo = reactive({
    req(anova1morfo())
    if(signiftestmorfo() == "significativo"){
     if(input$selectanovatest2 == "Kruskal-Wallis"){
      var_numerica = datamorfoyeartest() %>% dplyr::pull(input$anovanum)
      var_categorica = datamorfoyeartest() %>% dplyr::pull(input$anovacat)
      FSA::dunnTest(var_numerica ~ var_categorica, method = "bh")
     } else{
       anova1morfo() %>% stats::TukeyHSD()
     }
    } 
  })
  
  #mi serve perchè altrimenti quando passo da un file morfo all'altro mi si riduce la dimensione del grafico
  cdata <- session$clientData
  
  #grafico post-hoc
  output$posthocmorfograph = plotly::renderPlotly({
    req(posthocmorfo())
    if(input$selectanovatest2 == "Kruskal-Wallis"){
      data = as.data.frame(posthocmorfo()$res) %>% dplyr::select("Comparison", "P.adj") %>% tibble::as_tibble() %>% 
        tidyr::separate(Comparison, c("Cultivar_2", "Cultivar_1"), sep = " - " )
    } else{
      #trasformo in df, prendo solo la colonna di p.adj, prendo i rownames e trasformo in tibble
      data = as.data.frame(posthocmorfo()[1:1]) %>% dplyr::select(ends_with("adj")) %>% rownames_to_column("Cultivar_principale") %>% tibble::as_tibble() 
      #divido in due colonne le cultivar, rinomino la colonna dei p.adj e creo la colonna p_value
      data = data %>% tidyr::separate(Cultivar_principale, c("Cultivar_1", "Cultivar_2"), sep = "-") %>% dplyr::rename("P.adj" = ends_with("adj"))
    }
    
    data = data %>% dplyr::mutate(p_value = case_when(round(P.adj, digits = 4) > input$pvalanovamorfo ~ "non significativo",
                                                    round(P.adj, digits = 4) <= input$pvalanovamorfo ~ "significativo"))
    
    temp = ggplot2::ggplot(data, aes_string(x = "Cultivar_1", y = "Cultivar_2", fill = "p_value")) + geom_tile(colour = "black") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + scale_fill_manual(values = c("significativo" = "#f8766d", "non significativo" = "#00bfc4"))
    plotly::ggplotly(temp, width = cdata$output_posthocmorfograph_width, height = cdata$output_posthocmorfograph_height)
  })

  
  ######### Mappa morfo ##########

  #stampa mappa 1
  output$mapmorfo1 = renderTmap({
    req(datamorfomap())
    
    datamap = datamorfomap() %>% dplyr::filter(Anno == input$selyearmorfomap1) %>% dplyr::filter(N_campionamento == input$nummorfomap1)
    column2 = Olv_select_col(data = datamap, input = input$mapxmorfomap1)
    datam = datamap %>% dplyr::group_by(Codice_azienda, dplyr::across(colnames(column2))) %>%
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
    column2 = Olv_select_col(data = datamap, input = input$mapxmorfomap2)
    datam = datamap %>% dplyr::group_by(Codice_azienda, dplyr::across(colnames(column2))) %>% 
      dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE), n = dplyr::n()) %>% 
      dplyr::rename(!! paste(nfoglieolivenew()) := n) %>% select(!starts_with("ID"))
    datasel = dplyr::select(datam, !starts_with("UTM"))
    column = Olv_select_col(data = datasel, input = input$mapxmorfomap2)
    make_tmap(data =  datam, dotlegend = column)
  })
  
  
 
}
