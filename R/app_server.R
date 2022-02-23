#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs show
#' @importFrom plotly renderPlotly ggplotly layout
#' @rawNamespace import(ggplot2, except = last_plot)
#' @rawNamespace import(stats, except = filter)
#' @rawNamespace import(dplyr, except = lag)
#' @import tmap
#' @import tmaptools
#' @import ggfortify
#' @import htmltools
#' @import scales
#' @importFrom tidyr unite starts_with separate gather drop_na pivot_longer
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom grid gpar
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom lubridate year yday leap_year
#' @importFrom InteractiveComplexHeatmap makeInteractiveComplexHeatmap
#' @importFrom readr read_delim locale parse_factor
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom corrplot corrplot
#' @importFrom DT renderDT datatable formatRound
#' @importFrom grDevices rainbow hcl.colors colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dendextend color_branches
#' @importFrom sf st_as_sf st_crs
#' @importFrom VIM aggr
#' @importFrom stringr str_replace_all str_sort
#' @importFrom factoextra fviz_nbclust fviz_cluster fviz_dend fviz_silhouette eclust
#' @importFrom cluster pam clara
#' @importFrom gridExtra grid.arrange
#' @importFrom calendR calendR
#' @importFrom FSA dunnTest
#' @importFrom fmsb radarchart
#' @importFrom janitor remove_empty
#' @importFrom DataEditR dataEditUI dataEditServer
#' @importFrom shinyBS bsModal
#' @import gifski
#' @importFrom gganimate anim_save animate transition_reveal
#' @importFrom ncdf4 nc_open
#' @noRd


app_server <- function( input, output, session ) {
  # List the first level callModules here


  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "navb1",
                      selected = "panel2")
  })
  
  observeEvent(input$jumptohome, {
    updateTabsetPanel(session, "navb1",
                      selected = "panel1")
  })
  
  # #codice per far funzionare il conditional panel legato al "file1".
  # output$file1_ready <- reactive({
  #   return(!is.null(input$file1))
  # })
  # outputOptions(output, "file1_ready", suspendWhenHidden = FALSE)    # activate the output so it runs although not displayed
  # 
  
  observeEvent(input$ins_passw,{
    if(input$ins_passw == "lcxlc"){
      shinyjs::show(selector = "ul li:hidden")
    }
  })
  
  
  
  ############# Upload file ###############
  
  ###aziende


  output$valbox_aziende = renderUI({
    if(exists("data_aziende") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                h4(strong("File aziende: "),style = "color: white"),
                h5("Nessun file aziende presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }else{
      n_az = data_aziende$Codice_azienda %>% unique() %>% length()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                h4(strong("File aziende: "),style = "color: white"),
                h5(strong(n_az), " aziende coinvolte", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }
  })
  
  #carica il file
  data = eventReactive(input$load_files,{
    if(exists("data_aziende") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File aziende caricato!")), type = "message")
      data_aziende
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File aziende non caricato")), type = "error")
    }
  })

  #data = mod_update_data_server("updataaziende", original_data = data2, type_data = "azienda")


  ###Schede campionamento 


  output$valbox_drupecamp = renderUI({
    if(exists("drupecamp2020") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Drupe e foglie: "),style = "color: white"),
                     h5("Nessun file presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }else{
      years = drupecamp2020$Anno %>% na.omit() %>% unique()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Drupe e foglie: "),style = "color: white"),
                     h5("Anni presenti:  ", strong(paste(years, collapse = ", ")), style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }
  })
  
  drupe2 = eventReactive(input$load_files,{
    if(exists("drupecamp2020") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File campionamento drupe e foglie caricato!")), type = "message")
      drupecamp2020
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File campionamento drupe e foglie non caricato")), type = "error")
    }
  })
  
  drupe = mod_update_data_server("updatadrupe", original_data = drupe2, type_data = "camp_drupe", confronto_aziende = data)
  

#### OLIO 

  output$valbox_oliocamp = renderUI({
    if(exists("oliocampionamento2020") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Olio: "),style = "color: white"),
                     h5("Nessun file presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }else{
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Olio: "),style = "color: white"),
                     h5("Anni presenti:  ", strong(paste(levels(oliocampionamento2020$Anno), collapse = ", ")), style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }
  })
  
  oliocamp = eventReactive(input$load_files,{
    if(exists("oliocampionamento2020") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File campionamento olio caricato!")), type = "message")
      oliocampionamento2020
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File campionamento olio non caricato")), type = "error")
    }
  })
  
  
  #### ASSAGGI
  output$valbox_assaggi = renderUI({
    if(exists("assaggi2020") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Analisi sensoriali: "),style = "color: white"),
                     h5("Nessun file presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }else{
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Analisi sensoriali: "),style = "color: white"),
                     h5("Anni presenti:  ", strong(paste(levels(assaggi2020$Anno), collapse = ", ")), style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }
  })
  
  assaggi = eventReactive(input$load_files,{
    if(exists("assaggi2020") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File analisi sensoriali caricato!")), type = "message")
      assaggi2020
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File analisi sensoriali non caricato")), type = "error")
    }
  })
  
  
  #### POLIFENOLI TOTALI
  
  output$valbox_poltot = renderUI({
    if(exists("poliftot2020") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Polifenoli totali: "),style = "color: white"),
                     h5("Nessun file presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }else{
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Polifenoli totali: "),style = "color: white"),
                     h5("Foglie (", strong(paste(levels(poliftot2020$Foglie$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Drupe (", strong(paste(levels(poliftot2020$Drupe$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Olio (", strong(paste(levels(poliftot2020$Olio$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Posa (", strong(paste(levels(poliftot2020$Posa$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Sansa (", strong(paste(levels(poliftot2020$Sansa$Anno), collapse = ", "),")"), style = "color: white")
                     ),
              column(3, style = "padding-right: 0px; text-align: right;", br(), br(),
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }
  })

  poliftot = eventReactive(input$load_files,{
    if(exists("poliftot2020") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File polifenoli totali caricato!")), type = "message")
      poliftot2020
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File polifenoli totali non caricato")), type = "error")
    }
  })
  
  
  ##### POLIFENOLI INDIVIDUALI
  
  output$valbox_polind = renderUI({
    if(exists("polifind2020") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Polifenoli individuali: "),style = "color: white"),
                     h5("Nessun file presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }else{
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Polifenoli individuali: "),style = "color: white"),
                     h5("Foglie (", strong(paste(levels(polifind2020$Foglie$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Drupe (", strong(paste(levels(polifind2020$Drupe$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Olio (", strong(paste(levels(polifind2020$Olio$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Posa (", strong(paste(levels(polifind2020$Posa$Anno), collapse = ", "),")"), style = "color: white")
              ),
              column(3, style = "padding-right: 0px; text-align: right;", br(), br(),
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }
  })
  
  polifind = eventReactive(input$load_files,{
    if(exists("polifind2020") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File polifenoli individuali caricato!")), type = "message")
      polifind2020
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File polifenoli individuali non caricato")), type = "error")
    }
  })

    
  #carico il file polifenoli come .csv
  polif2 = reactive({
    req(input$polifinput)
    x = readr::read_delim(input$polifinput$datapath, delim = input$delim, col_names = input$header, local = readr::locale(decimal_mark = input$decim, date_format = "%d/%m/%Y", encoding = "windows-1252")) %>% 
      janitor::remove_empty("rows")
    #x$Presenza_larve = readr::parse_factor(as.character(x$Presenza_larve), levels = c("0","1","2"), ordered = TRUE)
    return(x)
  })
  
  polif = mod_update_data_server("updatapol", original_data = polif2, type_data = "polifenoli", confronto_aziende = data)
  
  
  
  #carico i file morfometria
  
  output$valbox_morfo = renderUI({
    if(exists("morfometria2020") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Morfometria: "),style = "color: white"),
                     h5("Nessun file presente in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }else{
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Morfometria: "),style = "color: white"),
                     h5("Foglie (", strong(paste(unique(morfometria2020$Foglie$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Drupe (", strong(paste(unique(morfometria2020$Drupe$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Endocarpo (", strong(paste(unique(morfometria2020$Endocarpo$Anno), collapse = ", "),")"), style = "color: white"),
                     h5("Rapporti (", strong(paste(unique(morfometria2020$Rapporti$Anno), collapse = ", "),")"), style = "color: white")
              ),
              column(3, style = "padding-right: 0px; text-align: right;", br(), br(),
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )))
    }
  })
  
  morfom = eventReactive(input$load_files,{
    if(exists("morfometria2020") != FALSE){
      showNotification(tagList(icon("check"), HTML("&nbsp;File morfometria caricato!")), type = "message")
      morfometria2020
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;File morfometria non caricato")), type = "error")
    }
  })

  
  
  ##### Carico i file polifenoli LCxLC ___________
  lcpolfoglie = reactive(polifenoliLCxLC2020$Foglie)
  
  lcpoldrupe = reactive(polifenoliLCxLC2020$Drupe)
  
  lcpololio = reactive(polifenoliLCxLC2020$Olio)
  
  lcpolposa = reactive(polifenoliLCxLC2020$Posa)
  
  lcpolsansa = reactive(polifenoliLCxLC2020$Sansa)
  
  output$content = DT::renderDT({
    data()
  })
  


  
  ############## Cultivar principale #################
  output$numcult = renderText({
    req(data())
    cult = data() %>% dplyr::select(Cultivar_principale) %>% table() %>% length()
    HTML(paste("Nel dataset sono presenti", "<b>", cult, "</b>", "cultivar ripartite secondo il seguente grafico:"))
  })
  
  #####grafico cultivar con scelta del tipo di grafico   
  output$cultplot = renderUI({
    req(data())
    if(input$selplotcult == 1){
      output$pie1 = plotly::renderPlotly({
        data() %>% plotly::plot_ly(labels = ~Cultivar_principale, type= "pie", textposition = 'inside', textinfo = 'label+value',
                           marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)), 
                           showlegend = FALSE) %>% plotly::layout(title = "Presenza delle varie cultivar sul territorio")
      })
      plotlyOutput("pie1")
    } else {
      output$bar1 = plotly::renderPlotly({
        getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
        dd = ggplot(data=data()) + geom_bar(mapping = aes(x = Cultivar_principale, fill = Cultivar_principale)) + 
          scale_y_continuous(breaks = scales::pretty_breaks()) + ggtitle("Presenza delle varie cultivar sul territorio") + 
          xlab("Cultivar") + ylab("Conta") + 
          theme(axis.title = element_text(face="bold", size = 13), axis.text.x = element_text(angle = 315, hjust = 0, size = 11)) + 
          scale_fill_manual(values = getPalette(length(unique(data()$Cultivar_principale))))
      })
      plotly::plotlyOutput("bar1")
    }
  })
  
  
  ##### Mappa Aziende ####
  
  

  #creo datmap1 senza le colonne UTM
  datmap1 = reactive({
    req(data())
    data() %>% dplyr::select(!starts_with("UTM"))
  })
  

  observeEvent(datmap1(), {
    updateSelectInput(session, "select3", choices = colnames(datmap1()))
    updateSelectInput(session, "filt_cult_mapp", choices = c("Tutte", unique(datmap1()$Cultivar_principale)), selected = "Tutte")
  })
  
  
  ###stampa mappa
  output$map1 = renderTmap({
    req(datmap1())
    data1 = data()
    data2 = datmap1()
    validate(need(input$filt_cult_mapp, "Seleziona qualcosa nel filtraggio delle cultivar. Se non si vuole filtrare, selezionare 'Tutte'"))
    if(!("Tutte" %in% input$filt_cult_mapp)){
      data1 = data1 %>% dplyr::filter(Cultivar_principale %in% input$filt_cult_mapp)
      data2 = data2 %>% dplyr::filter(Cultivar_principale %in% input$filt_cult_mapp)
    }
    colmap = Olv_select_col(data = data2, input = input$select3)
    make_tmap(data = data1, dotlegend = colmap)
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
    req(datadrupe())
    Olv_select_col(data = datadrupe(), input = input$selectx)
  })  
  
  observeEvent(datadrupe(), {
    updateSelectInput(session, "selectx", choices=colnames(datadrupe()))
    updateSelectInput(session, "selecty", choices=colnames(datadrupe()))
    updateSelectInput(session, "selectfill", choices=colnames(datadrupe()))
  })
  
  ###selezionare colonna Y da plottare
  showcolumny = reactive({
    req(datadrupe())
    Olv_select_col(data = datadrupe(), input = input$selecty)
  }) 
  
  ###selezionare colonna per il riempimento
  fillcolumn = reactive({
    req(datadrupe())
    Olv_select_col(data = datadrupe(), input = input$selectfill)
  }) 
  
  
  #aggiorna il selectinput , "selyearscatter" in base agli anni presenti e filtra
  observeEvent(datadrupe(), {
    updateSelectInput(session, "selyearscatter", choices = unique(datadrupe()$Anno))
  })
  dtplotyear2 = reactive({
    req(datadrupe())
    datadrupe() %>% dplyr::filter(Anno == input$selyearscatter)
  })
  
  
  ###scegliere anche il campionamento (scatter plot)
  dtdrupfilt2 = reactive({
    req(dtplotyear2())
    dplyr::filter(dtplotyear2(), N_campionamento == input$num2)
  })
  
  ###grafico classico (scatter plot)   , position = "jitter" , alpha = 0.7
  output$plotxy = plotly::renderPlotly({
    req(dtdrupfilt2())
    temp = ggplot(data = dtdrupfilt2()) +
      geom_count(mapping = aes_string(x = colnames(showcolumnx()), y = colnames(showcolumny()), colour = colnames(fillcolumn()))) +
      scale_size_continuous(range = c(3,9)) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fillcolumn()))))
  })
  
  
  
  ## Barplot 
  ###modificare la colonna campionamento con unite (R1_2020)
  dtnumyunite = reactive({
    req(datadrupe())
    datadrupe() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = TRUE)
    
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
    req(colorcamp())
    temp2=ggplot(data=colorcamp()) + 
      geom_col(mapping = aes_string(x = colnames(showcolumnx()), y = colnames(showcolumny()), fill = "N_campionamento"), position = position_dodge2(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank())
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })
  
  
  
  
  #### mappa (datadrupe)##
  

  mod_render_map_server("modulo_mappa_datadrupe", datamap = datadrupemap, datacol = drupe, extract_year = FALSE, extract_ncamp = TRUE)
  
  
  ################# Foto campioni ############
  
  
  #crea la tabella
  output$prov2 = DT::renderDT(dplyr::select(data(), c("Codice_azienda", "Cultivar_principale", "Azienda")), selection = "single", server = FALSE, rownames = FALSE)
  

  #aggiorna il selectinput "selyearfoto" in base agli anni presenti e seleziono
  observeEvent(datadrupe(), {
    updateSelectInput(session, "selyearfoto", choices = unique(datadrupe()$Anno))
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
    req(selprov())
    x = paste(selprov(), "foglie", sep="_")
    foglia = paste0(x,".jpg")
    tags$img(src = foglia, width = "75%", height = "75%")
  })
  
  #foto drupe
  output$phdrupa = renderUI({
    req(selprov())
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
    req(drupe(), oliocamp())
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
    updateSelectInput(session, "selaziendacalend", choices = c("Tutte", stringr::str_sort(unique(yearcalendar()$Codice_azienda))), selected = "Tutte")
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
    totspecial =  dplyr::left_join(tibble(day = rep(1:totaldays)), ggg, by = "day") %>% 
      dplyr::mutate(color = ifelse(campione == "Campionamento drupe e foglie", "#C7A76C", 
                                   ifelse(campione == "Campionamento olio", "#5CBD92", "#7DB0DD")))
    
    ncolors = totspecial$color %>% na.omit() %>% unique()
    
    if(length(ncolors) == 3){
      ncolors = ncolors[c(1,3,2)]
    }
    
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
    req(dataassaggi())
    dtassaggiyear = dataassaggi() %>% dplyr::filter(Anno == input$selyearscatterassagg)
    x = Olv_select_col(data = dataassaggi(), input = input$selectxassaggiscatt)
    y = Olv_select_col(data = dataassaggi(), input = input$selectyassaggiscatt)
    fill = Olv_select_col(data = dataassaggi(), input = input$selectfillassaggi)
    temp = ggplot(data = dtassaggiyear) +
      geom_count(mapping = aes_string(x = colnames(x), y = colnames(y), colour = colnames(fill))) +
       theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    if(is.double(dplyr::pull(dtassaggiyear, colnames(fill))) == TRUE){
      temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
    }
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = colnames(fill))))
  })
  
  #scale_size_continuous(range = c(2,9)) +
  
  ## Barplot 

  ###grafico a barre
  output$barplotassagg = plotly::renderPlotly({
    req(dataassaggi())
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
    req(dataassaggi())
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
  output$dtallegato = DT::renderDT({
    req(assaggidataph())
    dplyr::select(assaggidataph(), c("Azienda", "Codice_azienda", "Tipo_olio"))
    }, selection = "single", server = FALSE, rownames = FALSE, options = list("pageLength" = 15))
  
  
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
  
  
  #scegli i dati in base alla selezione
  datapoltot1 = reactive({
    req(poliftot())
    if(input$selfilepoltot == "foglie"){
      tempdata = poliftot()$Foglie
    } else if(input$selfilepoltot == "drupe"){
      tempdata = poliftot()$Drupe
    } else if(input$selfilepoltot == "olio"){
      tempdata = poliftot()$Olio
    } else if(input$selfilepoltot == "posa"){
      tempdata = poliftot()$Posa
    }else{
      tempdata = poliftot()$Sansa
    }
    #ATTENZIONE QUESTO VA FATTO DOPO L'UNIONE DI PIù ANNI (quelli che verranno). rbind funziona ma il dataEditR non so
    #tempdata$Anno = factor(tempdata$Anno, ordered = TRUE)
    return(tempdata)
  })
  
  #fare il join con data
  datapoltot = reactive({
    req(datapoltot1())
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale)
    x = dplyr::left_join(x = z, y = datapoltot1(), by = "Codice_azienda")
    if(input$selfilepoltot == "foglie" || input$selfilepoltot == "drupe"){
      x
    }else{
      x %>% dplyr::mutate(Codice_azienda = dplyr::case_when(
        Tipo_olio ==  "denocciolato" ~ "SA_02_den",
        TRUE ~ Codice_azienda
      )) 
    }
  })
  
  
  #summarizzo i dati
  datapoltot_summ1 = reactive({
    req(datapoltot1())
    
    if(input$selfilepoltot == "foglie"){
      datapoltot1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/g foglie)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else if(input$selfilepoltot == "drupe"){
      datapoltot1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/g drupe)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else if (input$selfilepoltot == "olio"){
      datapoltot1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Tipo_olio, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/kg olio)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else if(input$selfilepoltot == "posa"){
      datapoltot1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Tipo_olio, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/kg posa)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else{
      datapoltot1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Tipo_olio, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/kg sansa)", mean, na.rm = T)) %>% dplyr::ungroup()
    }
  })
  
  datapoltotmap = reactive({
    req(datapoltot_summ1())
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale, UTM_33T_E, UTM_33T_N)
    x = dplyr::right_join(x = z, y = datapoltot_summ1(), by = "Codice_azienda")
    if(input$selfilepoltot == "foglie" || input$selfilepoltot == "drupe"){
      x
    }else{
     x %>% dplyr::mutate(Codice_azienda = dplyr::case_when(
      Tipo_olio ==  "denocciolato" ~ "SA_02_den",
      TRUE ~ Codice_azienda
    )) 
    }
    
  })
  
  

  #fare il join con data per la mappa
  datapoltot_summ = reactive({
    req(datapoltotmap())
    datapoltotmap() %>% dplyr::select(!starts_with("UTM"))
  })
  

   #######polifenoli totali#####
  
  dataforselect_poltot = reactive({
    if(input$summpoltot == TRUE){
      datapoltot_summ() 
    } else{
      datapoltot()
    }
  })
  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatapoltot = reactive({
    req(datapoltot_summ())
    if(input$summpoltot == TRUE){
      data = datapoltot_summ() 
    } else{
      data = datapoltot()
    }
    data %>% dplyr::select(where(is.double))
  })
  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxpoltot", data = nadatapoltot, text_size = 1.1)


    
  output$tablepoltot = DT::renderDT({
    req(datapoltot_summ1())
    if(input$summpoltot == TRUE){
      data = datapoltot_summ1() 
    } else{
      data = datapoltot1()
    }
    data %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
  })



  observeEvent(dataforselect_poltot(), {
    updateSelectInput(session, "selectxtot", choices=colnames(dataforselect_poltot()))
    updateSelectInput(session, "selectfilltot", choices=colnames(dataforselect_poltot()))
    updateSelectInput(session, "selectytot", choices=colnames(dataforselect_poltot()))
    updateSelectInput(session, "selyearscattertot", choices = row.names(table(dplyr::select(dataforselect_poltot(), "Anno"))))
    updateSelectInput(session, "numtot", choices = unique(na.omit(dataforselect_poltot()$N_campionamento)))
  })
  

  


  ###grafico classico (scatter plot)   , position = "jitter" , alpha = 0.7
  output$totscatplot = plotly::renderPlotly({
    req(dataforselect_poltot())
    data = dataforselect_poltot() %>% dplyr::filter(Anno == input$selyearscattertot) %>% 
      dplyr::filter(N_campionamento == input$numtot)

    temp = ggplot(data) + 
      geom_count(aes_string(x = paste0("`",input$selectxtot, "`"), y = paste0("`",input$selectytot, "`"), colour = paste0("`",input$selectfilltot, "`")), 
                 size = 2) + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    if(is.double(dplyr::pull(data, input$selectfilltot)) == TRUE){
      temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
    }
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$selectfilltot)))
  })
  
  
  #########BARPLOT POLIFENOLI TOTALI#####################

  ###modificare la colonna campionamento con unite (R1_2020)
  datapoltotyearunite = reactive({
    req(datapoltot_summ())
    datapoltot_summ() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = FALSE)
  })
  

  ###aggiornare il checkbox in base al numero dei campionamenti
  observeEvent(datapoltotyearunite(), {
    updateCheckboxGroupInput(session, "checkcamptot", choices = unique(datapoltotyearunite()$N_campionamento),  selected = unique(datapoltotyearunite()$N_campionamento))
    updateSelectInput(session, "selectytotbar", choices = colnames(datapoltotyearunite()))
  })
  


  

  ###grafico a barre
  output$barplottot = plotly::renderPlotly({
    req(datapoltotyearunite())
    data = datapoltotyearunite() %>% dplyr::filter(N_campionamento %in% input$checkcamptot)
    data_err = datapoltot() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = FALSE) %>% 
      dplyr::filter(N_campionamento %in% input$checkcamptot)
    
    #con group risolvo il problema dell'unica barra di errore anche se ci sono due barre
    temp2=ggplot(data, aes_string(x = "Codice_azienda", y = paste0("`",input$selectytotbar, "`"), fill = paste0("`",input$selectfilltotbar, "`"), group = "N_campionamento")) +
      geom_col(position = position_dodge2( preserve = "single")) +
      stat_summary(data = data_err, geom = "errorbar", fun.data = mean_se,
                   position = position_dodge2(padding = 1.6, preserve = "single")) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank())
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
    
  })
  
  
  
  
  
  #################### Mappa Polifenoli totali ###########################################
  
  
  #mod_render_map_server("modulo_mappa_polifenoli", datamap = datapolifmap / datapolindmap, datacol =  datapolind, extract_year = FALSE, extract_ncamp = TRUE)
  
  
  
  
  observeEvent(datapoltot_summ(), {
    updateSelectInput(session, "colpoltotmap1", choices = colnames(datapoltot_summ()))
    updateSelectInput(session, "colpoltotmap2", choices = colnames(datapoltot_summ()))
    updateSelectInput(session, "yearpoltotmap1", choices = unique(na.omit(datapoltot_summ()$Anno)))
    updateSelectInput(session, "yearpoltotmap2", choices = unique(na.omit(datapoltot_summ()$Anno)))
    updateSelectInput(session, "numpoltotmap1", choices = unique(na.omit(datapoltot_summ()$N_campionamento)))
    updateSelectInput(session, "numpoltotmap2", choices = unique(na.omit(datapoltot_summ()$N_campionamento)))
  })
  
  
  

  #stampo mappa
  output$poltotmap1 = renderTmap({
    req(datapoltotmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapoltotmap() %>% dplyr::filter(Anno == input$yearpoltotmap1) %>% dplyr::filter(N_campionamento == input$numpoltotmap1) %>% 
      na.omit()
    colmap = dplyr::select(datamap,input$colpoltotmap1)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  
  
  #### seconda mappa
  
  output$poltotmap2 = renderTmap({
    req(datapoltotmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapoltotmap() %>% dplyr::filter(Anno == input$yearpoltotmap2) %>% dplyr::filter(N_campionamento == input$numpoltotmap2) %>%
      na.omit()
    colmap = dplyr::select(datamap, input$colpoltotmap2)
    make_tmap(data = datamap, dotlegend = colmap)
  })
  
  
  ##################################POLIFENOLI INDIVIDUALI##############################
  
  
  
  #scegli i dati in base alla selezione
  datapolind1 = reactive({
    req(polifind())
    if(input$selfilepolind == "foglie"){
      tempdata = polifind()$Foglie
      unit = " (ug/g)"
      start = 5
    } else if(input$selfilepolind == "drupe"){
      tempdata = polifind()$Drupe
      unit = " (ug/g)"
      start = 5
    } else if(input$selfilepolind == "olio"){
      tempdata = polifind()$Olio
      unit = " (mg/kg)"
      start = 6
    } else{ #posa
      tempdata = polifind()$Posa
      unit = " (mg/kg)"
      start = 6
    }
    #ATTENZIONE QUESTO VA FATTO DOPO L'UNIONE DI PIù ANNI (quelli che verranno). rbind funziona ma il dataEditR non so
    #tempdata$Anno = factor(tempdata$Anno, ordered = TRUE)
    
    #aggiunto unità di misura
    for(i in seq(start,length(tempdata))){
      colnames(tempdata)[i] = paste0(colnames(tempdata)[i], unit)}
    
    return(tempdata)
  })
  
  #fare il join con data
  datapolind = reactive({
    req(datapolind1())
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale, Areale) #, UTM_33T_E, UTM_33T_N
    x = dplyr::left_join(x = z, y = datapolind1(), by = "Codice_azienda")
    if(input$selfilepolind == "foglie" || input$selfilepolind == "drupe"){
      x
    }else{
      x %>% dplyr::mutate(Codice_azienda = dplyr::case_when(
        Tipo_olio ==  "denocciolato" ~ "SA_02_den",
        TRUE ~ Codice_azienda)) 
    }
  })
  
  
  #summarizzo i dati
  datapolind_summ1 = reactive({
    req(datapolind1())
    
    if(input$selfilepolind == "foglie" || input$selfilepolind == "drupe"){
      datapolind1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
        dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
    } else{  #olio e posa
      datapolind1() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno, Tipo_olio) %>% 
        dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
    }
  })
  

  datapolindmap = reactive({
    req(datapolind_summ1())
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale, Areale, UTM_33T_E, UTM_33T_N)
    x = dplyr::right_join(x = z, y = datapolind_summ1(), by = "Codice_azienda")
    if(input$selfilepolind == "foglie" || input$selfilepolind == "drupe"){
      x
    }else{
      x %>% dplyr::mutate(Codice_azienda = dplyr::case_when(
        Tipo_olio ==  "denocciolato" ~ "SA_02_den",
        TRUE ~ Codice_azienda))
    }
  })
  
  
  
  #fare il join con data per la mappa
  datapolind_summ = reactive({
    req(datapolindmap())
    datapolindmap() %>% dplyr::select(!starts_with("UTM"))
  })
  

  
  #aggiusto i data eliminando tutte le colonne non numeriche
  nadatapolind = reactive({
    req(datapolind_summ1())
    if(input$summpolind == TRUE){
      data = datapoltot_summ1() 
    } else{
      data = datapolind1()
    }
    data %>% dplyr::select(where(is.double))
  })

  
  
  output$tablepolind = DT::renderDT({
    req(datapolind_summ1())
    if(input$summpolind == TRUE){
      data = datapolind_summ1() 
    } else{
      data = datapolind1()
    }
    data %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
  })
  
  

  
  #creo il modulo per i NA
  mod_render_NAbox_server("naboxpolind", data = nadatapolind, text_size = 1.1)
  


  
  
  ###################SCATTER PLOT INDIVIDUALI#################

  dataforselect_polind = reactive({
    if(input$summpolind == TRUE){
      data = datapolind_summ() 
    } else{
      data = datapolind()
    }
  })
  
  observeEvent(dataforselect_polind(), {
    updateSelectInput(session, "selectxind", choices=colnames(dataforselect_polind()))
    updateSelectInput(session, "selectyind", choices=colnames(dataforselect_polind()))
    updateSelectInput(session, "selectfillind", choices=colnames(dataforselect_polind()))
    updateSelectInput(session, "selyearscatterind", choices = row.names(table(dplyr::select(dataforselect_polind(), "Anno"))))
    updateSelectInput(session, "numind", choices = unique(na.omit(dataforselect_polind()$N_campionamento)))
  })



  ####grafico classico (scatter plot)   , position = "jitter" , alpha = 0.7
  output$scatterindpol = plotly::renderPlotly({
    req(dataforselect_polind())
    data = dataforselect_polind() %>% dplyr::filter(Anno == input$selyearscatterind) %>% 
      dplyr::filter(N_campionamento == input$numind)
      
    temp = ggplot(data) +
      geom_count(aes_string(x = paste0("`",input$selectxind, "`"), y = paste0("`",input$selectyind, "`"), colour = paste0("`",input$selectfillind, "`")), 
                 size = 2) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    if(is.double(dplyr::pull(data, input$selectfillind)) == TRUE){
      temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
      }
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$selectfillind)))
  })


  ############# BOXPLOT INDIVIDUALI #########

  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearboxind", choices = unique(datapolind()$Anno))
    updateSelectInput(session, "numboxind", choices = unique(na.omit(datapolind()$N_campionamento)))
    
    updateSelectInput(session, "selectyboxind", choices = colnames(dplyr::select(datapolind(), where(is.double))))
    updateSelectInput(session, "selectfillboxind", choices = colnames(dplyr::select(datapolind(), where(is.character))))
  })
  
  output$boxplotindpol = renderPlotly({
    req(datapolind())
    data = datapolind() %>% dplyr::filter(Anno == input$selyearboxind) %>% dplyr::filter(N_campionamento == input$numboxind)

    temp = ggplot(data, aes_string(x = "Codice_azienda", y = paste0("`",input$selectyboxind, "`"), fill = paste0("`",input$selectfillboxind, "`"))) + 
      geom_boxplot() + geom_jitter(width = 0.3) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$selectfillboxind)))
  })
  
  
  ###############BARPLOT INDIVIDUALI##########



  ###modificare la colonna campionamento con unite (R1_2020)
  datapolindyearunite = reactive({
    req(datapolind_summ())
    datapolind_summ() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = FALSE)
    
  })
  
  
  observeEvent(datapolind_summ(), {
    updateSelectInput(session, "selectyindbar", choices=colnames(dplyr::select(datapolind_summ(), where(is.double))))
    updateCheckboxGroupInput(session, "checkcampind", choices = unique(datapolindyearunite()$N_campionamento),  selected = unique(datapolindyearunite()$N_campionamento))
  })



  ###grafico a barre
  output$barplotind = plotly::renderPlotly({
    req(datapolindyearunite())
    data = datapolindyearunite() %>% dplyr::filter(N_campionamento %in% input$checkcampind)
    data_err = datapolind() %>% tidyr::unite(col = N_campionamento, N_campionamento, Anno, remove = FALSE) %>% 
      dplyr::filter(N_campionamento %in% input$checkcampind)

    #con group risolvo il problema dell'unica barra di errore anche se ci sono due barre
    temp2=ggplot(data, aes_string(x = "Codice_azienda", y = paste0("`",input$selectyindbar, "`"), fill = paste0("`",input$selectfillindbar, "`"), group = "N_campionamento")) +
      geom_col(position = position_dodge2( preserve = "single")) +
      stat_summary(data = data_err, geom = "errorbar", fun.data = mean_se,
                   position = position_dodge2(padding = 1.6, preserve = "single")) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank())
    plotly::ggplotly(temp2) %>% plotly::layout(legend = list(title = list(text = "N_campionamento")))
  })


  
  ################# HEATMAP POLIFENOLI INDIVIDUALI ##################



  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti
  observeEvent(datapolind_summ(), {
    updateSelectInput(session, "selyearheatind", choices = unique(datapolind_summ()$Anno))
    updateSelectInput(session, "numheat", choices = unique(datapolind_summ()$N_campionamento))
  })



  dtheatsorted = reactive({
    req(datapolind_summ())
    data_pol = datapolind_summ()
    #rimuovo le unità di misura dai nomi
    names(data_pol) <- stringr::str_remove(names(data_pol), ' .*')
    sorder_data(
      data = data(),
      data2 = dplyr::select(data_pol, Codice_azienda, Azienda, Anno, N_campionamento, where(is.double)), #devo toglierlo perchè l'ho aggiunto prima
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
    len = dtheatsorted() %>% dplyr::select(where(is.double)) #-Anno non c'è perchè l'ho trasformato in factor
    sliderInput("slidercolheat", "Numero cluster:", min=2, max=length(len), value=2, step = 1)
  })


  #creo l'heatmap
  dataheat = reactive({
    req(dtheatsorted())
    if(input$selfilepolind == "foglie" || input$selfilepolind == "drupe"){
      unit = "ug/g"
    } else{  #olio e posa
      unit = "mg/kg"
    }
     
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
      unit_legend = unit,
      col_label_size = 12,
      bordi = c(4,2,2,15)
    )
  })


  observeEvent(input$updateheat,{
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, dataheat(), heatmap_id  = "heatmap_polind_output")
  })


  ###################### CORRELATION PLOT POLIFENOLI ####################################

  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datapolind_summ(), {
    updateSelectInput(session, "selyearcorrind", choices = unique(na.omit(datapolind_summ()$Anno)))
    updateSelectInput(session, "numcorr", choices = unique(na.omit(datapolind_summ()$N_campionamento)))
    
    #spider
    updateSelectInput(session, "selyearspiderpolind", choices = unique(na.omit(datapolind_summ()$Anno)))
    updateSelectInput(session, "selcodspiderpolind", choices = unique(datapolind_summ()$Codice_azienda))
    updateSelectInput(session, "selcampspiderpolind", choices = unique(na.omit(datapolind_summ()$N_campionamento)))
    updateSelectInput(session, "selcodspiderpolind2", choices = unique(datapolind_summ()$Codice_azienda))
    
    #più anni spider
    updateSelectInput(session, "selcodspidind", choices = unique(datapolind_summ()$Codice_azienda))
  })


  
  ###creo il corrplot
  output$corrplotind = plotly::renderPlotly({
    req(datapolind_summ())
    ###scegliere anno e  il campionamento (scatter plot)
    datatemp = datapolind_summ() %>% dplyr::filter(Anno == input$selyearcorrind) %>% dplyr::filter(N_campionamento == input$numcorr)

    temp = datatemp %>% dplyr::select(where(is.double))
    temp2 = round(stats::cor(temp, use = "na.or.complete"),1)
    par(xpd = TRUE)

    plot = ggcorrplot::ggcorrplot(temp2, hc.order = TRUE, type = "lower", outline.col = "white", show.diag = TRUE)
    plotly::ggplotly(plot)

  })
  

  

  ##### SPIDERPLOT polifenoli ind #####
  
  aziendaspidind = reactive({
    req(input$selcodspidind, datapolind_summ())
    datapolind_summ() %>% dplyr::filter(Codice_azienda == input$selcodspidind)
  })
  
  observeEvent(aziendaspidind(),{
    updateSelectInput(session, "selcampspidind", choices = unique(na.omit(aziendaspidind()$N_campionamento)))
  })
  
  
  output$spider_polind = renderPlot({
    req(datapolind_summ())
    
    ###Tra aziende
    if(input$type_spidind == "Tra aziende"){
      radardata = datapolind_summ() %>% dplyr::filter(Anno == input$selyearspiderpolind) %>% 
        dplyr::filter(N_campionamento == input$selcampspiderpolind)
      if(input$addcodspiderpolind == FALSE){
        radardata = radardata %>% dplyr::filter(Codice_azienda == input$selcodspiderpolind) %>%
          tibble::column_to_rownames("Codice_azienda") %>% dplyr::select(where(is.double))
        radr = (radardata+1) %>% log2() #%>% dplyr::mutate(across(.cols = everything(),  ~ ifelse(.x == "-Inf", 0, .x)))
        max = round(max(radr))
        while(max%%5 != 0){max = max+1}
        g2 = rbind("Max" = max, "Min" = 0, radr)
        lab = c(0, max*2/10, max*4/10, max*6/10, max*8/10, max)
        create_beautiful_radarchart(g2, 
                                    caxislabels = lab, 
                                    color = grDevices::hcl.colors(2, palette = "Dynamic"), 
                                    title = paste("Log2+1 dei polifenoli individuali di", input$selcodspiderpolind))
      }else{
        radardata = radardata %>% dplyr::filter(Codice_azienda == input$selcodspiderpolind | Codice_azienda == input$selcodspiderpolind2) %>%
          tibble::column_to_rownames("Codice_azienda") %>% dplyr::select(where(is.double))
        radr = (radardata+1) %>% log2() #%>% dplyr::mutate(across(.cols = everything(),  ~ ifelse(.x == "-Inf", 0, .x)))
        max = round(max(radr))
        while(max%%5 != 0){max = max+1}
        g2 = rbind("Max" = max, "Min" = 0, radr)
        lab = c(0, max*2/10, max*4/10, max*6/10, max*8/10, max)
        create_beautiful_radarchart(
          g2, 
          caxislabels = lab, 
          color = grDevices::hcl.colors(2, palette = "Dynamic"), 
          title = paste("Log2+1 dei polifenoli individuali di", input$selcodspiderpolind, "e", input$selcodspiderpolind2))
      }
    }else{
      ###Tra anni
      radardata = aziendaspidind() %>% dplyr::filter(N_campionamento == input$selcampspidind) %>%
        dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
        dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup() %>%
        tidyr::unite("Codice_azienda", Codice_azienda, Anno, sep = "_") %>%
        tibble::column_to_rownames("Codice_azienda") %>% dplyr::select(where(is.double))
      radr = (radardata +1)%>% log2() #%>% dplyr::mutate(across(.cols = everything(),  ~ ifelse(.x == "-Inf", 0, .x)))
      max = round(max(radr))
      while(max%%5 != 0){max = max+1}
      g2 = rbind("Max" = max, "Min" = 0, radr)
      lab = c(0, max*2/10, max*4/10, max*6/10, max*8/10, max)
      create_beautiful_radarchart(
        g2, 
        caxislabels = lab, 
        color = grDevices::hcl.colors(2, palette = "Dynamic"), 
        title = paste("Log2+1 dei polifenoli individuali di", input$selcodspidind))
    }


  })


  ######################### PCA #####################################################


  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(dataforselect_polind(), {
    updateSelectInput(session, "selyearpca", choices = unique(dataforselect_polind()$Anno))
    updateSelectInput(session, "numpca", choices = unique(dataforselect_polind()$N_campionamento))
  })

  #rimuovo le righe di NA se ci sono. pcadatina mi servirà dopo per il semi_join con data().
  pcadatina = reactive({
    req(dataforselect_polind())
    #se non summarizzo non posso avere i rownames uguali e quindi unisco cod_az con estrazione
    if(input$summpolind == TRUE){
      dataforselect_polind() %>% dplyr::filter(Anno == input$selyearpca) %>% dplyr::filter(N_campionamento == input$numpca) %>%
        stats::na.exclude()
    }else{
      dataforselect_polind() %>% dplyr::filter(Anno == input$selyearpca) %>% dplyr::filter(N_campionamento == input$numpca) %>% 
        tidyr::unite(col = Codice_azienda, Codice_azienda, Estrazione) %>% stats::na.exclude()
    }

  })

  pcadati = reactive({
    req(pcadatina())
    data = pcadatina() %>% tibble::column_to_rownames("Codice_azienda") %>%
      dplyr::select(where(is.double)) %>%  as.data.frame()
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
    req(pcadati())
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
    coll = pcadatina() %>% dplyr::select(!where(is.double)) %>% as.data.frame()
    tjoin = pcadati()$scores %>% as.data.frame() %>% tibble::rownames_to_column("Codice_azienda")
    pcadatainfo = dplyr::inner_join(tjoin, coll, by = "Codice_azienda") #%>% tibble::column_to_rownames("Codice_azienda")
    
    #qui con semi_join mi prendo solo le righe di data() presenti anche in pcadatina().
    if(input$selbiplotpolind == "Biplot"){
     temp = autoplot(pcadati(), data = pcadatainfo, shape = input$shpbiplot, colour = input$colbiplot, loadings = TRUE, loadings.colour = 'blue',
                    loadings.label = TRUE, loadings.label.size = 4, title = "Biplot") + theme(legend.title = element_blank())
    }else{
      temp = autoplot(pcadati(), data = pcadatainfo, shape = input$shpbiplot, colour = input$colbiplot, title = "Plot") +
        theme(legend.title = element_blank())
    }

    if (is.null(input$shpbiplot)){
      legtitle = input$colbiplot
    }else{
      legtitle = paste(input$colbiplot, ",", input$shpbiplot)
    }

    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = legtitle)))
  })
  
  ### plot 3D
  output$pca3dpolind = plotly::renderPlotly({
    req(pcadati())
    pc = pcadati()
    scoreind = pc$scores
    scoreindtb = scoreind %>% as.data.frame() %>% tibble::rownames_to_column("Codice_azienda")
    coll = pcadatina()  %>% dplyr::select(!where(is.double)) %>% as.data.frame()
    scoreindjoin = dplyr::left_join(x = scoreindtb, y = coll, by = "Codice_azienda")

    scoreindjoin %>% plotly::plot_ly(x = ~Comp.1, y = ~Comp.2, z= ~Comp.3, type = "scatter3d", mode = "markers", color = ~base::get(input$col3dind))

  })


  ##################### CLUSTERING polifenoli individuali ###################

  #aggiorna il selectinput , "selyearheatind" in base agli anni presenti e filtra
  observeEvent(datapolind(), {
    updateSelectInput(session, "selyearclustpolind", choices = na.omit(unique(datapolind()$Anno)), selected = na.omit(unique(datapolind()$Anno))[1])
    updateSelectInput(session, "numclustpolind", choices = row.names(table(dplyr::select(datapolind(), "N_campionamento"))))
  })
  
  dataclustpolind = reactive({
    req(datapolind())
    #filtro in base agli anni presenti e scelgo anche il num campionamento
    datapre = datapolind() %>% dplyr::filter(Anno %in% input$selyearclustpolind) %>% dplyr::filter(N_campionamento == input$numclustpolind)
    
    datapre = datapre %>% dplyr::select(input$selcolclustpolind, where(is.double)) %>% 
      dplyr::group_by(dplyr::across(input$selcolclustpolind)) %>% dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% stats::na.exclude()
    datapre %>% as.data.frame() %>% tibble::column_to_rownames(input$selcolclustpolind) %>% scale()
  })
  
  #grafici per la scelta del numero di cluster
  output$numclustergraphpolind = renderPlot({
    req(dataclustpolind())
    
    if(input$selclusthpolind == "K-means"){
      p1 = factoextra::fviz_nbclust(dataclustpolind(), stats::kmeans, method = "gap_stat")
      p2 = factoextra::fviz_nbclust(dataclustpolind(), stats::kmeans, method = "wss")
      p3 = factoextra::fviz_nbclust(dataclustpolind(), stats::kmeans, method = "silhouette")
    } else if(input$selclusthpolind == "PAM"){
      p1 = factoextra::fviz_nbclust(dataclustpolind(), cluster::pam, method = "gap_stat")
      p2 = factoextra::fviz_nbclust(dataclustpolind(), cluster::pam, method = "wss")
      p3 = factoextra::fviz_nbclust(dataclustpolind(), cluster::pam, method = "silhouette")
    } else{
      p1 = factoextra::fviz_nbclust(dataclustpolind(), cluster::clara, method = "gap_stat")
      p2 = factoextra::fviz_nbclust(dataclustpolind(), cluster::clara, method = "wss")
      p3 = factoextra::fviz_nbclust(dataclustpolind(), cluster::clara, method = "silhouette")
    }
    
    if(input$selclustmethodpolind == "Partizionale"){
      gridExtra::grid.arrange(p1, p2, p3, ncol = 2)
    } else{
      meth = c("single","complete","ward.D","ward.D2")
      d = stats::dist(dataclustpolind())
      par(mfrow=c(2,2))
      for(i in seq(1,4)){
        hs = stats::hclust(d, method = meth[i])
        plot(hs$height, pch=16, main = meth[i], ylab = "Height")
      }
    }
  })
  
  #data cluster
  output$plotclusterpolind = renderPlot({
    req(dataclustpolind())
    if(input$selclustmethodpolind == "Partizionale"){
      if(input$selclusthpolind == "K-means"){
        clust = stats::kmeans(dataclustpolind(), centers = input$selnumclustpolind, nstart = 25)
      } else if (input$selclusthpolind == "PAM"){
        clust = cluster::pam(dataclustpolind(), k = input$selnumclustpolind)
      } else {
        clust = cluster::clara(dataclustpolind(), k = input$selnumclustpolind)
      }
      factoextra::fviz_cluster(clust, data = dataclustpolind(), ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())
    } else {
      hcluster = factoextra::eclust(dataclustpolind(), "hclust", hc_method = input$selhclustmethpolind, k = input$selnumclustpolind)
      p1 = factoextra::fviz_dend(hcluster, palette = "jco", rect = TRUE, show_labels = FALSE)
      p2 = factoextra::fviz_silhouette(hcluster)
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })
  
  

  #################### Mappa Polifenoli individuali ###########################################



  observeEvent(datapolind_summ(), {
    updateSelectInput(session, "colpolindmap1", choices = colnames(datapolind_summ()))
    updateSelectInput(session, "colpolindmap2", choices = colnames(datapolind_summ()))
    updateSelectInput(session, "yearpolindmap1", choices = unique(na.omit(datapolind_summ()$Anno)))
    updateSelectInput(session, "yearpolindmap2", choices = unique(na.omit(datapolind_summ()$Anno)))
    
    updateSelectInput(session, "numpolindmap1", choices = unique(na.omit(datapolind_summ()$N_campionamento)))
    updateSelectInput(session, "numpolindmap2", choices = unique(na.omit(datapolind_summ()$N_campionamento)))
  })




  #stampo mappa
  output$polindmap1 = renderTmap({
    req(datapolindmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapolindmap() %>% dplyr::filter(Anno == input$yearpolindmap1) %>% dplyr::filter(N_campionamento == input$numpolindmap1) %>% 
      na.omit()
    colmap = dplyr::select(datapolind_summ(), input$colpolindmap1)
    make_tmap(data = datamap, dotlegend = colmap)
  })


  #### seconda mappa

  output$polindmap2 = renderTmap({
    req(datapolindmap())
    #filtra in base all'anno selezionato e il campionamento
    datamap = datapolindmap() %>% dplyr::filter(Anno == input$yearpolindmap2) %>% dplyr::filter(N_campionamento == input$numpolindmap2) %>%
      na.omit()
    colmap = dplyr::select(datapolind_summ(), input$colpolindmap2)
    make_tmap(data = datamap, dotlegend = colmap)
  })


  ################# Cromatogrammi polifenoli individuali ################


  datapolind_cromat = reactive({
    req(datapolind_summ())
    datapolind_summ() %>% na.omit()
  })
  #crea la tabella
  output$prov3 = DT::renderDT({
    req(datapolind_cromat())
    datapolind_cromat() %>% dplyr::select(c("Azienda", "Codice_azienda")) %>% unique() #ho messo unique perchè R1 e R2
    }, selection = "single", server = FALSE, rownames = FALSE, options = list("pageLength" = 15))


  #aggiorna il selectinput "selyearfoto" in base agli anni presenti e seleziono
  observeEvent(datapolind_cromat(), {
    updateSelectInput(session, "selyearcromatph", choices = unique(datapolind_cromat()$Anno))
    })
  
  #output per i conditionalPanels
  output$check_crompolind = reactive({
    if(input$campcromatph == "1_campionamento" && (input$selfilepolind == "olio" || input$selfilepolind == "posa")){
      "no"
    }else{"yes"}
  })
  outputOptions(output, 'check_crompolind', suspendWhenHidden = FALSE)
  
  
  


  #####selezionare la riga dell'azienda cromatogramma drupe
  selprovcromat = reactive({
    req(input$prov3_rows_selected)
    nroww=input$prov3_rows_selected
    x = dplyr::select(datapolind_cromat(), "Codice_azienda") %>% unique()
    z = paste("www/cromatogrammi", input$selyearcromatph, input$selfilepolind, input$campcromatph, sep = "/") #selyearfoto e campfoto
    paste(z, x[nroww,], sep = "/")
  })


  #foto cromatogramma drupe
  output$phcromat = renderUI({
    req(selprovcromat())
    croma = paste0(selprovcromat(),".png")
    tags$img(src = croma)
  })



  #crea la tabella dei polifenoli dell'azienda selezionata
  output$tabcromatpolind = DT::renderDT({
    req(input$prov3_rows_selected)
    nroww=input$prov3_rows_selected
    #vedo quale azienda ho selezionato. Mi serve per il filtro su datapolind() (ci sono i replicati non posso farlo direttamente)
    azienda1 = datapolind_cromat() %>% dplyr::select("Codice_azienda") %>% unique()
    azienda = azienda1[nroww,]
    
    ncamp = ifelse(input$campcromatph == "1_campionamento", "R1", "R2")
    #prendo un'azienda filtro e tolgo le colonne in più. Se è vuota, NULL
    datapolind() %>% dplyr::filter( N_campionamento == ncamp) %>% dplyr::filter(Codice_azienda  %in% azienda) %>% 
      dplyr::select(Estrazione, where(is.double))
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
    req(datalcxlc())
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
    req(lcwidepolif())
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
    req(datalcgraphcamp())
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
    req(datalcgraph())
    if(input$logscattlc == TRUE){
      datalogg = dplyr::mutate_if(datalcgraph(), is.numeric, log2)
      yname = "Log2 quantificazione (mg/Kg)"
    }else{
      datalogg = datalcgraph()
      yname = "Quantificazione (mg/Kg)"
      }
    #ora plotly mi mostra nel tooltip il log e non il valore normale (in barplot non succede). Per risolvere
    #creo una label in ggplot e poi la riporto nei tooltip in ggplotly
    pos_jitter = ifelse(input$lcdatatypescatt == "Cultivar principale", "jitter", "identity")
    size_points = ifelse(input$lcdatatypescatt == "Cultivar principale", 2, 3)
    if(input$lcdatatypescatt == "Polifenolo"){
      temp = ggplot(data = datalogg) +
        geom_count(mapping = aes_string(x = "Codice_azienda", y = input$lcselpolifscatt, shape = "Presenza", color = input$fillscattlc)) + #,color = grDevices::hcl.colors(length(na.omit(dplyr::select(datalcgraph(),input$lcselpolifscatt))), palette = "Dynamic")
        scale_shape_manual(values=c(10, 1, 16),drop = FALSE, labels = c("<LOQ", "Assente", "Presente")) +
        theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + ylab(paste(input$lcselpolifscatt, "(mg/Kg)"))
      if(is.double(dplyr::pull(datalogg, input$fillscattlc)) == TRUE){
        temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
      }
      plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = "Presenza")))
    }else{
      temp = ggplot(data = datalogg, aes_string(label = "Quantificazione")) +
        geom_count(mapping = aes_string(x = "Compounds", y = "Quantificazione", shape = "Presenza", color = input$fillscattlc), size = size_points, position = pos_jitter) + #, color = grDevices::hcl.colors(length(na.omit(datalcgraph()$Quantificazione)), palette = "Dynamic")
        scale_shape_manual(values=c(10, 1, 16), drop = FALSE, labels = c("<LOQ", "Assente", "Presente")) +
        theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(yname)
      if(is.double(dplyr::pull(datalogg, input$fillscattlc)) == TRUE){
        temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
      }
      plotly::ggplotly(temp, tooltip = c("Compounds", "Presenza", "label", input$fillscattlc)) %>% plotly::layout(legend = list(title = list(text = "Legenda")))
    }

  })


  # Barplot
  output$barplotlc = renderPlotly({
    req(datalcgraph())
    if(input$logscattlc == TRUE){
      datalogg = dplyr::mutate_if(datalcgraph(), is.numeric, log2)
      yname = "Log2 quantificazione (mg/Kg)"
    }else{
      datalogg = datalcgraph()
      yname = "Quantificazione (mg/Kg)"
      }

    if(input$lcdatatypescatt == "Polifenolo"){
      temp = ggplot(data=datalogg) +
        geom_col(mapping = aes_string(x = "Codice_azienda", y = input$lcselpolifscatt, fill = input$fillscattlc, linetype = "Presenza")) +
        theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(paste(input$lcselpolifscatt, "(mg/Kg)"))
      plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$fillscattlc)))
    }else{
      if(input$lcselpolbar != "Tutti"){
        data = datalogg %>% dplyr::filter(Compounds == input$lcselpolbar)
      }else{data = datalogg}

     temp = ggplot(data) +
      geom_col(mapping = aes_string(x = "Compounds", y = "Quantificazione", fill = input$fillscattlc, linetype = "Presenza"), position = input$bartypelc) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0), legend.title = element_blank()) + ylab(yname)
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
    req(lcwidepolif())
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
    req(lcheatsorted())
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
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, dataheatlc(), heatmap_id  = "heatmap_lc_output")
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
    req(data(), morfom())
    if(input$selfilemorfo == "foglie"){
      tempdata = morfom()$Foglie
    } else if(input$selfilemorfo == "drupe"){
      tempdata = morfom()$Drupe
    } else if(input$selfilemorfo == "endocarpo"){
      tempdata = morfom()$Endocarpo
    } else{
      tempdata = morfom()$Rapporti
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
  output$dtfotomorfo = DT::renderDT({
    req(data())
    dplyr::select(data(), c("Codice_azienda", "Cultivar_principale", "Azienda"))
    }, selection = "single", server = FALSE, rownames = FALSE)
  
  
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
      dplyr::summarise(dplyr::across(where(is.double), mean , na.rm = TRUE), n = dplyr::n()) %>%
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
    if(is.double(dplyr::pull(dt, colnames(fill))) == TRUE){
      temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
    }
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
    req(datamorfoheat())
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
    req(dtheatsortedmorfo())
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
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, dataheatmorfo(), heatmap_id  = "heatmap_morfo_output")
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
    req(pcadatimorfo())
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
    updateSelectInput(session, "selyearclustmorfo", choices = na.omit(unique(datamorfo()$Anno)), selected = na.omit(unique(datamorfo()$Anno))[1])
    updateSelectInput(session, "numclustmorfo", choices = row.names(table(dplyr::select(datamorfo(), "N_campionamento"))))
  })
  
  dataclustmorfo = reactive({
    req(datamorfo())
    #filtro in base agli anni presenti e scelgo anche il num campionamento
    datapre = datamorfo() %>% dplyr::filter(Anno %in% input$selyearclustmorfo) %>% dplyr::filter(N_campionamento == input$numclustmorfo)

    datapre = datapre %>% dplyr::select(input$selcolclustmorfo, where(is.double), -dplyr::any_of(c("Anno", colnames(dplyr::select(datapre, starts_with("ID")))))) %>%
      dplyr::group_by(dplyr::across(input$selcolclustmorfo)) %>% dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% stats::na.exclude()
    datapre %>% as.data.frame() %>% tibble::column_to_rownames(input$selcolclustmorfo) %>% scale()
    
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
    updateSelectInput(session, "selyearttestmorfo", choices = na.omit(unique(datamorfo()$Anno)), selected = na.omit(unique(datamorfo()$Anno))[1])
  })
  
  datamorfoyeartest = reactive({
    req(datamorfo())
    validate(need(input$selyearttestmorfo != "", "Seleziona almeno un anno."))
    datamorfo() %>% dplyr::filter(Anno %in% input$selyearttestmorfo)
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
    req(input$catvarttest)
    req(input$culttest1)
         #shp1 = datamorfo()[datamorfo()[[input$catvarttest]] %in% input$culttest1,] %>% dplyr::pull(input$numvarttest) %>% 
     # stats::shapiro.test()
    shp1 = datamorfoyeartest() %>% dplyr::filter(.data[[input$catvarttest]] %in% input$culttest1) %>%
      dplyr::pull(input$numvarttest) %>%  stats::shapiro.test()
    shp1$data.name = paste(input$culttest1)
    shp1 
    

  })
  
  output$shapiro1 = renderPrint({
    req(shapiro1data())
    shapiro1data()
  })
  
  shapiro2data = reactive({
    req(datamorfoyeartest())
    req(input$catvarttest)
    req(input$culttest2)
    shp2 = datamorfoyeartest() %>% dplyr::filter(.data[[input$catvarttest]] %in% input$culttest2) %>%
      dplyr::pull(input$numvarttest) %>%  stats::shapiro.test()
    shp2$data.name = paste(input$culttest2)
    shp2
  })
  
  output$shapiro2 = renderPrint({
    req(shapiro2data())
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
    req(datamorfo())
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
    req(chisqmorfo())
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
    req(shapiroanova1data())
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
    req(anova1morfo())
    anovasumm = summary(anova1morfo())
    if(round(anovasumm[[1]][["Pr(>F)"]][[1]], digits = 4) <= input$pvalanovamorfo ||  kruskmorfodata()$p.value <= input$pvalanovamorfo){
      "significativo"
    } else{
      "non significativo"
    }
  })
  
  #output per l'ui
  output$signiftestmorfoui = reactive({
    req(signiftestmorfo())
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
  
  
  
  ##### MAPPA METEO #####
  
  data_meteo = reactive({
    req(data())
    destpath = base::system.file(package = "OliveHealthR")
    destpath = paste0(destpath, "/data/precipitazioni_2020_2021.nc")
    nc_meteofile = ncdf4::nc_open(destpath)
    
    makedata_meteo(ncfile = nc_meteofile, dati_aziende = data())
  })
  

  
  observeEvent(data_meteo(),{
    #updateSelectInput(session, "varmeteo", choices = names(data_meteo()))
    if(input$type_mapmeteo == "Animata"){
      updateSelectInput(session, "selyearmeteo", choices = c("Tutti",unique(lubridate::year(data_meteo()[[1]]$Tempo))))
    }else{
      updateSelectInput(session, "selyearmeteo", choices = unique(lubridate::year(data_meteo()[[1]]$Tempo)))
    }
    updateSelectInput(session, "selcod_plotmeteo", choices = unique(data_meteo()[[1]]$Codice_azienda), selected = unique(data_meteo()[[1]]$Codice_azienda)[1])
    
    updateSelectInput(session, "selcod_plotmeteo2", choices = unique(data_meteo()[[1]]$Codice_azienda), selected = unique(data_meteo()[[1]]$Codice_azienda)[1])

    updateSelectInput(session, "selyearplotmeteo", choices = c("Tutti",unique(lubridate::year(data_meteo()[[1]]$Tempo))))
  })

  
  output$mapmeteo2 = renderTmap({
    req(data_meteo(), input$varmeteo)
    
    g3 = data_meteo()[[input$varmeteo]] %>% dplyr::filter(lubridate::year(Tempo) == input$selyearmeteo) %>% 
      dplyr::group_by(Codice_azienda) %>% dplyr::summarise(Misura = mean(Misura)) 
    data2 = dplyr::left_join(g3, data(), by = "Codice_azienda")
   
    #utmcoord23 = sf::st_as_sf(data2, coords = c("UTM_33T_E", "UTM_33T_N" ), crs= 32633)
    
    namevar = ifelse(input$varmeteo == "tp", "Precipitazioni totali (mm)", 
                     ifelse(input$varmeteo == "swvl2", "Volume d'acqua nel suolo (7-28cm) (m3/m-3)", 
                            "Volume d'acqua nel suolo (28-100cm) (m3/m-3)"))
    
    
    utmcoord23 = sf::st_as_sf(data2, coords = c("UTM_33T_E", "UTM_33T_N" ), crs= 32633)
    sf::st_crs(campania) = 32633
    
    tm_shape(campania)+ tm_polygons(col= "provincia", alpha = 0.8) + tm_shape(utmcoord23) +
      tm_dots(col = colnames(dplyr::select(data2, Misura)), scale = 1.5, title = namevar, popup.vars = TRUE)
  })
  
  
  output$mapmeteo = renderImage({
    req(data_meteo(), input$varmeteo)
    data2 = dplyr::left_join(data_meteo()[[input$varmeteo]], data(), by = "Codice_azienda")
    if(input$selyearmeteo != "Tutti"){
      data2 = data2 %>% dplyr::filter(lubridate::year(Tempo) == input$selyearmeteo)
    }

    utmcoord23 = sf::st_as_sf(data2, coords = c("UTM_33T_E", "UTM_33T_N" ), crs= 32633)

    namevar = ifelse(input$varmeteo == "tp", "Precipitazioni totali (mm)", 
                          ifelse(input$varmeteo == "swvl2", "Volume d'acqua nel suolo (7-28cm) (m3/m-3)", 
                                 "Volume d'acqua nel suolo (28-100cm) (m3/m-3)"))
    

    #utm_long = utmcoord23 %>% tidyr::pivot_longer(cols = 2:9, names_to = "month", values_to = namevar)
    #utm_long$month = factor(utm_long$month, ordered =T, levels = unique(utm_long$month))
    sf::st_crs(campania) = 32633
    anim = tm_shape(campania)+ tm_polygons(col= "provincia", alpha = 0.8) + tm_shape(utmcoord23) +
      tm_dots(col = "Misura", scale = 4.5, title = namevar) + tm_facets(along= "Tempo", free.coords = FALSE)

    outfile <- tempfile(fileext='.gif')
    
    tmap_animation(anim, "outfile.gif",delay = 130)
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
  }, deleteFile = TRUE)
  
  
  
  ####grafici
  output$ggline_meteo = renderPlotly({
    req(data_meteo(), input$varmeteo)
    #validate(need(input$selcod_plotmeteo != "", "Seleziona almeno un'azienda."))
    label_misura = ifelse(input$varmeteo == "tp", "Precipitazioni totali (mm)", 
                    ifelse(input$varmeteo == "swvl2", "Volume d'acqua nel suolo (7-28cm) (m3/m-3)", 
                           "Volume d'acqua nel suolo (28-100cm) (m3/m-3)"))
    
       if(input$meteoplot_tipoconf == "Tra aziende"){
      meteo = data_meteo()[[input$varmeteo]] %>% dplyr::filter(Codice_azienda %in% input$selcod_plotmeteo)
      if(input$selyearplotmeteo != "Tutti"){
        meteo = meteo %>% dplyr::filter(lubridate::year(Tempo) == input$selyearplotmeteo)
      }
      
      temp = ggplot(data = meteo, aes(x = Tempo, y = Misura)) +
        geom_line(aes(color = Codice_azienda, group = Codice_azienda)) +
        geom_point(aes(color = Codice_azienda, group = Codice_azienda)) + ylab(label_misura)
      plotly::ggplotly(temp)
      
    }else{
      meteo = data_meteo()[[input$varmeteo]] %>% dplyr::filter(Codice_azienda %in% input$selcod_plotmeteo2)
      #confronto tra aziende: scelgo l'azienda e mi mostra gli anni (o tutti o solo alcuni)
      meteo =  cbind(meteo, Year = as.factor(lubridate::year(meteo$Tempo)))
      meteo = cbind(meteo, Mese = lubridate::month(meteo$Tempo, label = T))
      
      temp = ggplot(data = meteo, aes(x = Mese, y = Misura,color = Year, group = Year)) +
        geom_line() + geom_point() +ylab(label_misura)
      plotly::ggplotly(temp)
    }
    
  })
  
  
  output$plotline_animated = renderImage({
    req(data_meteo(), input$varmeteo)
    validate(need(input$selcod_plotmeteo != "", "Seleziona almeno un'azienda."))
    meteo = data_meteo()[[input$varmeteo]] %>% dplyr::filter(Codice_azienda  %in% input$selcod_plotmeteo)
    if(input$selyearplotmeteo != "Tutti"){
      meteo = meteo %>% dplyr::filter(lubridate::year(Tempo) == input$selyearplotmeteo)
    }
    
    h = ggplot(data = meteo, aes(x = Tempo, y = Misura)) +
      geom_line(aes(color = Codice_azienda, group = Codice_azienda)) + 
      gganimate::transition_reveal(Tempo)
    
    outfile <- tempfile(fileext='.gif')
    gganimate::anim_save("outfile.gif", gganimate::animate(h, height = 650, width =900,nframes = 100))
    
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
  }, deleteFile = TRUE)
  
  
  ##### confronti  #####
  
  observeEvent(input$conf_type1,{
    if(input$conf_type1 == "ind"){
      updateRadioGroupButtons(session, "conf_type2", 
                              choiceValues = list("camp", "prec"),
                              choiceNames = list(
                                paste(icon("circle"), HTML("<b>&nbsp;Schede campionamento</b>")),
                                paste(icon("circle"), HTML("<b>&nbsp;Precipitazioni</b>"))
                              ), justified = TRUE, status = "primary")
    }else{
      updateRadioGroupButtons(session, "conf_type2", 
                        choiceValues = list("camp", "prec", "ind"),
                        choiceNames = list(
                          paste(icon("circle"), HTML("<b>&nbsp;Schede campionamento</b>")),
                          paste(icon("circle"), HTML("<b>&nbsp;Precipitazioni</b>")),
                          paste(icon("circle"), HTML("<b>&nbsp;Polifenoli individuali</b>"))
                        ), justified = TRUE, status = "primary")
    }
  })
  
  
  conf_type = reactive({
    req(input$conf_type1, input$conf_type2)
    paste0(input$conf_type1, input$conf_type2)
  })
  
  
  observeEvent(conf_type(),{
    if(input$conf_type2 == "ind" || input$conf_type1 == "ind"){
      updateSelectInput(session, "conf_selpoltot", choices = c("Foglie", "Drupe", "Olio", "Posa"))
    }else{
      updateSelectInput(session, "conf_selpoltot", choices = c("Foglie", "Drupe", "Olio", "Posa", "Sansa"))
    }
  })
  

  #scegli i dati in base alla selezione
  poltot_conf_notsumm = reactive({
    req(poliftot())
    poliftot()[[input$conf_selpoltot]]
  })
  
  
  poltot_conf = reactive({
    req(poltot_conf_notsumm())
    
    if(input$conf_selpoltot == "Foglie"){
      temp = poltot_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/g foglie)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else if(input$conf_selpoltot == "Drupe"){
      temp = poltot_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/g drupe)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else if (input$conf_selpoltot == "Olio"){
      temp = poltot_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Tipo_olio, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/kg olio)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else if(input$conf_selpoltot == "Posa"){
      temp = poltot_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Tipo_olio, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/kg posa)", mean, na.rm = T)) %>% dplyr::ungroup()
    } else{
      temp = poltot_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Tipo_olio, Anno) %>% 
        dplyr::summarise(dplyr::across("Polifenoli (mg/kg sansa)", mean, na.rm = T)) %>% dplyr::ungroup()
    }
    
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale)
    dplyr::right_join(x = z, y = temp, by = "Codice_azienda")
    
  })
  
  #polind non mediato
  polind_conf_notsumm = reactive({
    req(polifind())
    if(req(input$conf_selpoltot) != "Sansa"){
    #scegli i dati polifenoli individuali in base alla selezione
    datapolind1 = polifind()[[input$conf_selpoltot]]
    if(input$conf_selpoltot == "Foglie" || input$conf_selpoltot == "Drupe"){
      unit = " (ug/g)"
      start = 5
    }else{
      unit = " (mg/kg)"
      start = 6
    }
    for(i in seq(start,length(datapolind1))){
      colnames(datapolind1)[i] = paste0(colnames(datapolind1)[i], unit)
    }
    datapolind1
    }else{
      return(NULL)
    }

    
  })
  
  
  

  dataconf = reactive({
    req(conf_type(), poltot_conf(), drupe(), data_meteo())
    validate(need(input$conf_selyear != "", "Seleziona almeno un anno."))
    
    ###### prima variabile Polifenoli Totali
    if(input$conf_type1 == "tot"){
      if(conf_type() == "totcamp"){
        #### totali + campionamento
        x = poltot_conf() %>% dplyr::left_join(drupe(), by = c("Codice_azienda","Anno", "N_campionamento")) %>% 
          dplyr::filter(Anno %in% input$conf_selyear)
        
      }else if(conf_type() == "totprec"){
        #### totali + precipitazioni
        meteo = data_meteo()[[input$varmeteo_conf]] %>% dplyr::mutate(Anno = factor(lubridate::year(Tempo))) %>% 
          dplyr::filter(Anno %in% input$conf_selyear) %>% 
          dplyr::group_by(Codice_azienda, Anno) %>% dplyr::summarise(Misura = mean(Misura)) %>% 
          dplyr::rename(Misura_precipitazione = Misura)
        x = poltot_conf() %>% dplyr::filter(Anno %in% input$conf_selyear) %>% dplyr::left_join(meteo, by = c("Codice_azienda", "Anno"))
        
      }else if(conf_type() == "totind"){
        req(polind_conf_notsumm())
        #### totali + individuali
        if(input$conf_selpoltot == "Foglie" || input$conf_selpoltot == "Drupe"){
          polindsumm = polind_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
            dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
          x = poltot_conf() %>% dplyr::left_join(polindsumm, by = c("Codice_azienda","Anno", "N_campionamento")) %>% 
            dplyr::filter(Anno %in% input$conf_selyear)
        } else{  #olio e posa
          polindsumm = polind_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno, Tipo_olio) %>% 
            dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
          x = poltot_conf() %>% dplyr::left_join(polindsumm, by = c("Codice_azienda","Anno", "N_campionamento", "Tipo_olio")) %>% 
            dplyr::filter(Anno %in% input$conf_selyear)
        }
      }
    }

    
    ##### prima variabile Individuali
    if(input$conf_type1 == "ind"){
      z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale)
      
      #summarizzo i polind e unisco con data
      if(input$conf_selpoltot == "Foglie" || input$conf_selpoltot == "Drupe"){
        polindsumm = polind_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno) %>% 
          dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
        
        polindsumm = dplyr::right_join(x = z, y = polindsumm, by = "Codice_azienda")
        
        x = poltot_conf() %>% dplyr::left_join(polindsumm, by = c("Codice_azienda","Anno", "N_campionamento")) %>% 
          dplyr::filter(Anno %in% input$conf_selyear)
      } else{  #olio e posa
        polindsumm = polind_conf_notsumm() %>% dplyr::group_by(Codice_azienda, N_campionamento, Anno, Tipo_olio) %>% 
          dplyr::summarise(dplyr::across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
        polindsumm = dplyr::right_join(x = z, y = polindsumm, by = "Codice_azienda")
      }
      
      if(conf_type() == "indcamp"){
        #### individuali + campionamento
        x = polindsumm %>% dplyr::left_join(drupe(), by = c("Codice_azienda","Anno", "N_campionamento")) %>% 
          dplyr::filter(Anno %in% input$conf_selyear)
        
      }else if(conf_type() == "indprec"){
   
        ## individuali + precipitazioni
        meteo = data_meteo()[[input$varmeteo_conf]] %>% dplyr::mutate(Anno = factor(lubridate::year(Tempo))) %>% 
          dplyr::filter(Anno %in% input$conf_selyear) %>%
          dplyr::group_by(Codice_azienda, Anno) %>% dplyr::summarise(Misura = mean(Misura)) %>% 
          dplyr::rename(Misura_precipitazione = Misura)
        x = polindsumm %>% dplyr::filter(Anno %in% input$conf_selyear) %>% dplyr::left_join(meteo, by = c("Codice_azienda", "Anno"))
      }
    }
    
    

    if(input$conf_selpoltot == "Foglie" || input$conf_selpoltot == "Drupe"){
      x
    }else{
      x %>% dplyr::mutate(Codice_azienda = dplyr::case_when(
        Tipo_olio ==  "denocciolato" ~ "SA_02_den",
        TRUE ~ Codice_azienda
      ))
    }
    
  })
  
  
  output$dt_conf = renderDT({
    req(dataconf())
    dataconf()
  })
  
  #download dati conf
  output$down_integrazione <- downloadHandler(
    filename = function() {
      paste0("Confronto_", conf_type(),"_",input$conf_selpoltot, "_",Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(dataconf(), file)
    }
  )

  #### grafici confronti #####
  
  observeEvent(dataconf(),{
    updateSelectInput(session, "scattx_conf", choices = colnames(dataconf()))
    updateSelectInput(session, "scatty_conf", choices = colnames(dataconf()))
    updateSelectInput(session, "scattfill_conf", choices = colnames(dataconf()))
    updateSelectInput(session, "scattsize_conf", choices = c("Nessuna", colnames(dplyr::select(dataconf(), where(is.double)))))
    updateSelectInput(session, "scattnum_conf", choices = unique(na.omit(dataconf()$N_campionamento)), selected = "R1")
    updateSelectInput(session, "scattshape_conf", choices = c("Nessuna", colnames(dplyr::select(dataconf(), !where(is.double)))))
    
    updateSelectInput(session, "selcult_scatt_conf", choices = unique(na.omit(dataconf()$Cultivar_principale)), 
                      selected =unique(na.omit(dataconf()$Cultivar_principale))[1])
    
    updateSelectInput(session, "corrnum_conf", choices = unique(na.omit(dataconf()$N_campionamento)), selected = unique(na.omit(dataconf()$N_campionamento))[1])
  })
  
  #### scatterplot
  output$scatterconf = plotly::renderPlotly({
    req(dataconf())
    validate(need(input$scattnum_conf != "", "Seleziona almeno un campionamento"))
    
    data = dataconf()  %>% dplyr::filter(N_campionamento %in% input$scattnum_conf)
    
    #jitter se c'è almeno una colonna char o fct
    if(TRUE %in% (c("character", "factor") %in% sapply(dplyr::select(data, input$scattx_conf, input$scatty_conf), class))){
      pos_jitter = "jitter"
    }else{
      pos_jitter = "identity"
    }
    
    
    if(input$typescatt_conf == "Filtra per cultivar"){
      data = data %>% dplyr::filter(Cultivar_principale %in% input$selcult_scatt_conf)
    }
    
    if(input$scattshape_conf == "Nessuna") shape_col = NULL else{shape_col = input$scattshape_conf}
    
    if(input$scattsize_conf == "Nessuna"){
      temp = ggplot(data, aes_string(x = paste0("`",input$scattx_conf, "`"), y = paste0("`",input$scatty_conf, "`"))) +
        geom_count(aes_string(colour = paste0("`",input$scattfill_conf, "`"),
                              shape = shape_col, label = "Anno"), position = pos_jitter,
                   size = 2) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    }else{
      temp = ggplot(data, aes_string(x = paste0("`",input$scattx_conf, "`"), y = paste0("`",input$scatty_conf, "`"))) +
        geom_count(aes_string(label = "Anno",
                              colour = paste0("`",input$scattfill_conf, "`"), size = paste0("`",input$scattsize_conf, "`"),
                              shape = shape_col), position = pos_jitter)  
    }
    


    if(is.double(dplyr::pull(data, input$scattfill_conf)) == TRUE){
      temp = temp + scale_colour_gradient(high = "#132B43", low = "#56B1F7")
    }
    
    if(input$scatt_fit_conf == TRUE){
      temp = temp + geom_smooth(method = input$scatt_selmodfit_conf, aes_string(colour = paste0("`",input$scattfill_conf, "`")), se = T)
    }
    
    if(input$scatt_dens_conf == TRUE){
      temp = temp + geom_density_2d(aes_string(linetype = paste0("`",input$scattfill_conf, "`"), col = paste0("`",input$scattfill_conf, "`")))
    }
    

    
    
    if(input$scattfacet_conf == TRUE && length(input$conf_selyear) > 1){
      temp = temp + theme(axis.text.x = element_text(angle = 315, hjust = 0, margin=margin(t=20)),legend.title = element_blank()) + 
        facet_grid(rows = vars(Anno), scales = "free", switch = "x")
      plotly::ggplotly(temp, height = 600) %>% plotly::layout(legend = list(title = list(text = input$scattfill_conf)))
    }else{
      temp = temp + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())

      plotly::ggplotly(temp, height = 600) %>% plotly::layout(legend = list(title = list(text = input$scattfill_conf)))
    }

  })
  
  

  ###correlation plot
  output$corrplotconf = plotly::renderPlotly({
    req(dataconf())
    validate(need(input$conf_type2 != "camp", "Non è possibile calcolare la correlazione per questo confronto."))
    validate(need(input$corrnum_conf != "", "Scegli almeno un campionamento."))
    ###scegliere anno e  il campionamento (scatter plot)
    datatemp = dataconf() %>% dplyr::filter(N_campionamento %in% input$corrnum_conf) %>% 
      dplyr::select(where(is.double))
    temp2 = round(stats::cor(datatemp, use = "na.or.complete"),1)
    par(xpd = TRUE)
    
    plot = ggcorrplot::ggcorrplot(temp2, hc.order = TRUE, type = "lower", outline.col = "white", show.diag = TRUE)
    plotly::ggplotly(plot)
    
  })
  
  ##### test d'ipotesi confronti ####
 
  #dati non mediati
  dataconf_notsumm1 = reactive({
    req(conf_type(), poltot_conf_notsumm(), drupe(), data_meteo())
    z = data() %>% dplyr::select(Codice_azienda, Provincia, Azienda, Cultivar_principale)
    
    if(input$conf_type1 == "tot"){
      poltot = dplyr::right_join(x = z, y = poltot_conf_notsumm(), by = "Codice_azienda")
      
      if(conf_type() == "totcamp"){
        x = poltot %>% dplyr::left_join(drupe(), by = c("Codice_azienda","Anno", "N_campionamento")) %>% 
          dplyr::filter(Anno %in% input$conf_selyear)
      }else if (conf_type() == "totprec"){
        ## totali + precipitazioni
        meteo = data_meteo()[[input$varmeteo_conf]] %>% dplyr::filter(lubridate::year(Tempo) %in% input$conf_selyear) %>% 
          dplyr::group_by(Codice_azienda) %>% dplyr::summarise(Misura = mean(Misura)) %>% 
          dplyr::rename(Misura_precipitazione = Misura)
        
        x = poltot %>% dplyr::filter(Anno %in% input$conf_selyear) %>% dplyr::left_join(meteo, by = "Codice_azienda")
      }else if (conf_type() == "totind"){
        req(polind_conf_notsumm())
        ## totali + individuali
        if(input$conf_selpoltot == "Foglie" || input$conf_selpoltot == "Drupe"){
          
          x = poltot %>% dplyr::left_join(polind_conf_notsumm(), by = c("Codice_azienda","Anno", "N_campionamento", "Estrazione")) %>% 
            dplyr::filter(Anno %in% input$conf_selyear)
        } else{  #olio e posa
          x = poltot %>% dplyr::left_join(polind_conf_notsumm(), by = c("Codice_azienda","Anno", "N_campionamento", "Tipo_olio","Estrazione")) %>% 
            dplyr::filter(Anno %in% input$conf_selyear)
        }
      }
      
      ##### prima variabile Individuali
    } else if(input$conf_type1 == "ind"){
      req(polind_conf_notsumm())
      polind = dplyr::right_join(x = z, y = polind_conf_notsumm(), by = "Codice_azienda")

      if(conf_type() == "indcamp"){
        x = polind %>% dplyr::left_join(drupe(), by = c("Codice_azienda","Anno", "N_campionamento")) %>%
          dplyr::filter(Anno %in% input$conf_selyear)

      }else if(conf_type() == "indprec"){
        ## individuali + precipitazioni
        meteo = data_meteo()[[input$varmeteo_conf]] %>% dplyr::filter(lubridate::year(Tempo) %in% input$conf_selyear) %>%
          dplyr::group_by(Codice_azienda) %>% dplyr::summarise(Misura = mean(Misura)) %>%
          dplyr::rename(Misura_precipitazione = Misura)

        x = polind %>% dplyr::filter(Anno %in% input$conf_selyear) %>% dplyr::left_join(meteo, by = "Codice_azienda")
      }
    }

    if(input$conf_selpoltot == "Foglie" || input$conf_selpoltot == "Drupe"){
      x
    }else{
      x %>% dplyr::mutate(Codice_azienda = dplyr::case_when(
        Tipo_olio ==  "denocciolato" ~ "SA_02_den",
        TRUE ~ Codice_azienda
      ))
    }
  })
  
  observeEvent(dataconf_notsumm1(),{
    updateSelectInput(session, "conf_selcamp_test", choices = na.omit(unique(dataconf_notsumm1()$N_campionamento)), 
                      selected = na.omit(unique(dataconf_notsumm1()$N_campionamento))[1])
  })
  
  dataconf_notsumm = reactive({
    req(dataconf_notsumm1())
    validate(need(input$conf_selcamp_test != "", "Seleziona almeno un campionamento"))
    dataconf_notsumm1() %>% dplyr::filter(N_campionamento %in% input$conf_selcamp_test)
  })

  ######################### correlation test ____________________________
  
  #aggiorna il selectinput in base ai double presenti
  observeEvent(dataconf_notsumm(), {
    updateSelectInput(session, "corrtest1_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.double), -Anno)))
    updateSelectInput(session, "corrtest2_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.double), -Anno)),
                      selected = colnames(dplyr::select(dataconf_notsumm(), where(is.double), -Anno))[2])
    
    updateSelectInput(session, "corrtestfill_conf", choices = colnames(dplyr::select(dataconf_notsumm(), !where(is.double)))) #is.character
    
  })
  
  #test normalità con shapiro test
  output$shapirocorr1_conf = renderPrint({
    req(dataconf_notsumm())
    shp1 = dataconf_notsumm() %>% dplyr::pull(input$corrtest1_conf) %>% stats::shapiro.test()
    shp1$data.name = paste(input$corrtest1_conf)
    shp1
  })
  
  output$shapirocorr2_conf = renderPrint({
    req(dataconf_notsumm())
    shp2 = dataconf_notsumm() %>% dplyr::pull(input$corrtest2_conf) %>% stats::shapiro.test()
    shp2$data.name = paste(input$corrtest2_conf)
    shp2
  })
  
  #selectcorrtest
  
  #T-test
  output$corrtest_conf = renderPrint({
    req(dataconf_notsumm())
    x = dataconf_notsumm() %>% dplyr::pull(input$corrtest1_conf)
    y = dataconf_notsumm() %>% dplyr::pull(input$corrtest2_conf)
    
    test = stats::cor.test(x, y, method = input$selectcorrtest_conf)
    test$data.name = paste(input$corrtest1_conf, "and", input$corrtest2_conf)
    test
  })
  
  
  #scatterplot
  output$scattcorrtest_conf = plotly::renderPlotly({
    req(dataconf_notsumm())

    #voglio un singol fit (linea della regressione lineare) o più fit?
    if(input$numfitcorrtest_conf == FALSE){
      temp = ggplot(data = dataconf_notsumm(), 
                    mapping = aes_string(x = paste0("`",input$corrtest1_conf, "`"), y = paste0("`",input$corrtest2_conf, "`"))) + 
        geom_point(mapping = aes_string(color = paste0("`",input$corrtestfill_conf, "`"))) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + 
        geom_smooth(method = input$selmod_fit_conf, mapping = aes_string(color = paste0("`",input$corrtestfill_conf, "`"), fill = paste0("`",input$corrtestfill_conf, "`")), se = input$selsecorrtest_conf)
    } else{
      temp = ggplot(data = dataconf_notsumm(),
                    mapping = aes_string(x = paste0("`",input$corrtest1_conf, "`"), y = paste0("`",input$corrtest2_conf, "`"))) +
        geom_point(mapping = aes_string(color = paste0("`",input$corrtestfill_conf, "`"))) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + 
        geom_smooth(method=input$selmod_fit_conf, se = input$selsecorrtest_conf)
      
    }
    
    plotly::ggplotly(temp) 
  })
  
  
  
  ################ T-Test ___________________________________
  
  ###### t-test 
  #aggiorna il selectinput "selvarttest" in base ai double presenti
  observeEvent(dataconf_notsumm(), {
    updateSelectInput(session, "catvarttest_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.character))))
    updateSelectInput(session, "numvarttest_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.double), -Anno)))
  })
  
  #culttest1 e 2
  observeEvent(input$catvarttest_conf, {
    updateSelectInput(session, "culttest1_conf", choices = unique(dplyr::select(dataconf_notsumm(), input$catvarttest_conf)))
    updateSelectInput(session, "culttest2_conf", choices = unique(dplyr::select(dataconf_notsumm(), input$catvarttest_conf)))
  })
  
  
  #creo la variabile dei dati con le due opzioni
  datattest_conf = reactive({
    req(dataconf_notsumm())
    dataconf_notsumm() %>% dplyr::filter(.data[[input$catvarttest_conf]] %in% c(input$culttest1_conf, input$culttest2_conf))
    #datamorfo()[datamorfo()[[input$catvarttest]] %in% c(input$culttest1, input$culttest2),]
    
  })
  
  
  #test normalità con shapiro test
  shapiro1data_conf = reactive({
    req(dataconf_notsumm())
    req(input$catvarttest_conf)
    req(input$culttest1_conf)
    #shp1 = datamorfo()[datamorfo()[[input$catvarttest]] %in% input$culttest1,] %>% dplyr::pull(input$numvarttest) %>% 
    # stats::shapiro.test()
    shp1 = dataconf_notsumm() %>% dplyr::filter(.data[[input$catvarttest_conf]] %in% input$culttest1_conf) %>%
      dplyr::pull(input$numvarttest_conf) %>%  stats::shapiro.test()
    shp1$data.name = paste(input$culttest1_conf)
    shp1 
    
  })
  
  output$shapiro1_conf = renderPrint({
    req(shapiro1data_conf())
    shapiro1data_conf()
  })
  
  shapiro2data_conf = reactive({
    req(dataconf_notsumm())
    req(input$catvarttest_conf)
    req(input$culttest2_conf)
    shp2 = dataconf_notsumm() %>% dplyr::filter(.data[[input$catvarttest_conf]] %in% input$culttest2_conf) %>%
      dplyr::pull(input$numvarttest_conf) %>%  stats::shapiro.test()
    shp2$data.name = paste(input$culttest2_conf)
    shp2
  })
  
  output$shapiro2_conf = renderPrint({
    req(shapiro2data_conf())
    shapiro2data_conf()
  })
  
  
  #output per l'ui
  output$shapttestmorfoui_conf = reactive({
    if(shapiro1data_conf()$p.value < 0.05 && shapiro2data_conf()$p.value < 0.05){
      "distribuzione normale"
    }else{"distribuzione non normale"}
  })
  outputOptions(output, 'shapttestmorfoui_conf', suspendWhenHidden = FALSE)
  
  
  #test varianza F-test
  output$vartest1_conf = renderPrint({
    req(datattest_conf())
    num = datattest_conf() %>% dplyr::pull(input$numvarttest_conf)
    cat = datattest_conf() %>% dplyr::pull(input$catvarttest_conf)
    var1 = stats::var.test(num ~ cat)
    var1$data.name = paste(input$numvarttest_conf, "~", input$catvarttest_conf)
    var1
  })
  
  
  #Boxplot
  output$boxttest_conf = plotly::renderPlotly({
    req(datattest_conf())

    temp = ggplot(data = datattest_conf(), 
                  mapping = aes_string(x = paste0("`",input$catvarttest_conf, "`"), y = paste0("`",input$numvarttest_conf, "`"), fill = paste0("`",input$catvarttest_conf,"`"))) + 
      geom_boxplot() + geom_jitter(width = 0.3) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$catvarttest_conf)))
  })
  
  #T-test
  output$ttest1_conf = renderPrint({
    req(datattest_conf())
    num = datattest_conf() %>% dplyr::pull(input$numvarttest_conf)
    cat = datattest_conf() %>% dplyr::pull(input$catvarttest_conf)
    if(input$selectttest_conf == "T-test"){
      test = stats::t.test(num ~ cat, var.equal = input$selvarequal_conf)
      test$data.name = paste(input$numvarttest_conf, "~", input$catvarttest_conf)
    } else{
      test = stats::wilcox.test(num ~ cat)
      test$data.name = paste(input$numvarttest_conf, "~", input$catvarttest_conf)
    }
    test
  })
  
  
  ############################# ANOVA ____________
  #aggiorna il selectinput "selvarttest" in base ai double presenti
  observeEvent(dataconf_notsumm(), {
    updateSelectInput(session, "anovanum_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.double), -Anno)))
    updateSelectInput(session, "anovacat_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.character), -c(Azienda, N_campionamento))), selected = "Cultivar_principale")
    updateSelectInput(session, "anovacat2_conf", choices = colnames(dplyr::select(dataconf_notsumm(), where(is.character), -c(Azienda, N_campionamento))))
  })
  
  observeEvent(input$selectanovatest_conf,{
    if(input$selectanovatest_conf == "Two-way ANOVA"){
      updateAwesomeRadio(session, "selectanovatest2_conf", choices = "ANOVA", selected = "ANOVA")
    }else{
      updateAwesomeRadio(session, "selectanovatest2_conf", choices = c("ANOVA", "Kruskal-Wallis"))
    }
  })
  
  #test normalità con shapiro test
  shapiroanova1data_conf = reactive({
    req(dataconf_notsumm())
    shp1 = dataconf_notsumm() %>% dplyr::pull(input$anovanum_conf) %>% stats::shapiro.test()
    shp1$data.name = paste(input$anovanum_conf)
    shp1
  })
  
  output$shapiroanova1_conf = renderPrint({
    req(shapiroanova1data_conf())
    shapiroanova1data_conf()
  })
  
  
  #output per l'ui
  output$shapanovamorfoui_conf = reactive({
    if(shapiroanova1data_conf()$p.value < 0.05){
      "distribuzione normale"
    }else{"distribuzione non normale"}
  })
  outputOptions(output, 'shapanovamorfoui_conf', suspendWhenHidden = FALSE)
  
  
  
  #Boxplot anova
  output$boxanova_conf = plotly::renderPlotly({
    req(dataconf_notsumm())

    temp = ggplot(data = dataconf_notsumm(), 
                  mapping = aes_string(x = paste0("`",input = input$anovacat_conf, "`"), y = paste0("`",input$anovanum_conf, "`"), fill = paste0("`",input = input$anovacat_conf,"`"))) + 
      geom_boxplot() + geom_jitter(width = 0.3) + theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank())
    plotly::ggplotly(temp) %>% plotly::layout(legend = list(title = list(text = input$anovacat_conf)))
  })
  
  
  anova1morfo_conf = reactive({
    req(dataconf_notsumm())
    var_numerica = dataconf_notsumm() %>% dplyr::pull(input$anovanum_conf)
    var_categorica = dataconf_notsumm() %>% dplyr::pull(input$anovacat_conf)
    if(input$selectanovatest_conf == "Two-way ANOVA" ){
      var_categorica1 = dataconf_notsumm() %>% dplyr::pull(input$anovacat_conf)
      var_categorica2 = dataconf_notsumm() %>% dplyr::pull(input$anovacat2_conf)
      if(input$anova2typemorfo_conf == "Modello additivo"){
        stats::aov(var_numerica ~ var_categorica1 + var_categorica2)
      }else{
        stats::aov(var_numerica ~ var_categorica1 * var_categorica2) 
      }
    }else{
      stats::aov(var_numerica ~ var_categorica)
    }
  })
  
  output$anova1morfoprint_conf = renderPrint({
    summary(anova1morfo_conf())
  })
  
  
  
  #kruskal-wallis data
  kruskmorfodata_conf = reactive({
    req(dataconf_notsumm())
    var_numerica = dataconf_notsumm() %>% dplyr::pull(input$anovanum_conf)
    var_categorica = dataconf_notsumm() %>% dplyr::pull(input$anovacat_conf)
    kru = stats::kruskal.test(var_numerica ~ var_categorica)
    kru$data.name = paste(input$anovanum_conf, "by", input$anovacat_conf)
    kru
  })
  #kruskal-wallis
  output$kruskmorfo_conf = renderPrint({
    kruskmorfodata_conf()
  })
  
  #variabile che mi dice se il test è significativo
  signiftestmorfo_conf = reactive({
    req(anova1morfo_conf())
    anovasumm = summary(anova1morfo_conf())
    if(round(anovasumm[[1]][["Pr(>F)"]][[1]], digits = 4) <= input$pvalanovamorfo_conf ||  kruskmorfodata_conf()$p.value <= input$pvalanovamorfo_conf){
      "significativo"
    } else{
      "non significativo"
    }
  })
  
  #output per l'ui
  output$signiftestmorfoui_conf = reactive({
    req(signiftestmorfo_conf())
    signiftestmorfo_conf()
  })
  outputOptions(output, 'signiftestmorfoui_conf', suspendWhenHidden = FALSE)
  
  #post hoc tukey o dunn
  posthocmorfo_conf = reactive({
    req(anova1morfo_conf())
    if(signiftestmorfo_conf() == "significativo"){
      if(input$selectanovatest2_conf == "Kruskal-Wallis"){
        var_numerica = dataconf_notsumm() %>% dplyr::pull(input$anovanum_conf)
        var_categorica = dataconf_notsumm() %>% dplyr::pull(input$anovacat_conf)
        FSA::dunnTest(var_numerica ~ var_categorica, method = "bh")
      } else{
        anova1morfo_conf() %>% stats::TukeyHSD()
      }
    } 
  })
  
  #mi serve perchè altrimenti quando passo da un file morfo all'altro mi si riduce la dimensione del grafico
  cdata <- session$clientData
  
  #grafico post-hoc
  output$posthocmorfograph_conf = plotly::renderPlotly({
    req(posthocmorfo_conf())
    if(input$selectanovatest2_conf == "Kruskal-Wallis"){
      data = as.data.frame(posthocmorfo_conf()$res) %>% dplyr::select("Comparison", "P.adj") %>% tibble::as_tibble() %>% 
        tidyr::separate(Comparison, c("Cultivar_2", "Cultivar_1"), sep = " - " )
    } else{
      #trasformo in df, prendo solo la colonna di p.adj, prendo i rownames e trasformo in tibble
      data = as.data.frame(posthocmorfo_conf()[1:1]) %>% dplyr::select(ends_with("adj")) %>% rownames_to_column("Cultivar_principale") %>% tibble::as_tibble() 
      #divido in due colonne le cultivar, rinomino la colonna dei p.adj e creo la colonna p_value
      data = data %>% tidyr::separate(Cultivar_principale, c("Cultivar_1", "Cultivar_2"), sep = "-") %>% dplyr::rename("P.adj" = ends_with("adj"))
    }
    
    data = data %>% dplyr::mutate(p_value = case_when(round(P.adj, digits = 4) > input$pvalanovamorfo_conf ~ "non significativo",
                                                      round(P.adj, digits = 4) <= input$pvalanovamorfo_conf ~ "significativo"))
    
    temp = ggplot2::ggplot(data, aes_string(x = "Cultivar_1", y = "Cultivar_2", fill = "p_value")) + geom_tile(colour = "black") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0),legend.title = element_blank()) + scale_fill_manual(values = c("significativo" = "#f8766d", "non significativo" = "#00bfc4"))
    plotly::ggplotly(temp, width = cdata$output_posthocmorfograph_conf_width, height = cdata$output_posthocmorfograph_conf_height)
  })
  
  
  
}
