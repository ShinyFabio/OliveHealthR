#' mod_update_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param original_data put the internal/default data
#' @param type_data character. Type of data that is used. Could be "azienda", "camp_drupe", "polifenoli"....(to be updated)
#' @param confronto_aziende The updated dataframe of dati_monitoraggio. If type_data != "azienda", you have to use this param (simply write: confronto_aziende = data). It's used for a codice_azienda control (e.g. if in drupe there is a codice_azienda that there isn't in data(), the app returns an error)
#'
#' @noRd 
#'
#' @import shiny
#' @import rhandsontable
#' @importFrom DataEditR dataEditUI dataEditServer
#' @importFrom shinyWidgets sendSweetAlert awesomeRadio
#' @importFrom shinyBS bsModal
#' @importFrom shinycssloaders withSpinner
#' @importFrom readr read_delim locale
#' @importFrom janitor remove_empty
#' @importFrom dplyr bind_rows
#' 
#' @examples \dontrun{
#' #### in Server ###
#' drupe = mod_update_data_server("updatadrupe", original_data = drupe2, type_data = "camp_drupe", confronto_aziende = data)
#' 
#' #drupe will be the drupe() file used in plots, tables etc.
#' 
#' ### in UI ###
#'  mod_update_data_ui("updatadrupe")
#' 
#' }
#' 


mod_update_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("upaziendedata"), HTML("&nbsp;Aggiorna i dati"), icon("file-upload"), class = "btn-success", width = "180px", style='padding:10px; font-size:130%; font-weight: bold;' ),
    shinyBS::bsModal(ns("upaziendetab"), "Aggiorna i dati", trigger = ns("upaziendedata"), size = "large",
                     fluidPage(
                       fluidRow(
                         column(3, fileInput(ns("fileupaziendedata"), "Carica il file csv")),
                         column(3, awesomeRadio(ns("selupdtaziende"), "Dati da mostrare", choices = c("solo nuovi", "entrambi"))),
                         conditionalPanel(condition = "output.type_data_ui != 'azienda'", ns = ns,
                           column(3, br(), actionButton(ns("button_test"), HTML("&nbsp;Esegui controllo"), icon("spell-check"), class = "btn-info", width = "170px", style='padding:10px; font-size:110%; font-weight: bold;'))
                         ),
                         column(3, br(), actionButton(ns("mergeaziende"), HTML("&nbsp;Unisci/Rimuovi"), icon("edit"),class = "btn-success", width = "170px", style='padding:10px; font-size:110%; font-weight: bold;'))
                       ),
                       conditionalPanel(
                         condition = "input.selupdtaziende == 'solo nuovi'", ns = ns,
                         DataEditR::dataEditUI(ns("edit-1"))
                       ),
                       conditionalPanel(
                         condition = "input.selupdtaziende == 'entrambi'", ns = ns,
                         rhandsontable::rHandsontableOutput(ns("dttable2"))
                       )
                     )
                     
    )
  )
}

#' mod_update_data Server Functions
#'
#' @noRd 
mod_update_data_server <- function(id, original_data, type_data, confronto_aziende){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$type_data_ui <- reactive(type_data)
    outputOptions(output, "type_data_ui", suspendWhenHidden = FALSE)
    
    userFile <- reactive({
      # If no file is selected, don't do anything
      # input$fileupaziendedata == ns("fileupaziendedata")
      validate(need(input$fileupaziendedata, message = FALSE))
      input$fileupaziendedata
    })
    
    #carico il nuovo csv file
    dataupdated = reactive({
       readr::read_delim(userFile()$datapath, delim = ";", col_names = TRUE,na = "", local = readr::locale(decimal_mark = ",", date_format = "%d/%m/%Y", encoding = "windows-1252")) %>% 
          janitor::remove_empty("rows")
    })
    
    #creo la tabella del nuovo file che posso editare e la salvo come "data_to_edit"
    data_to_edit <- DataEditR::dataEditServer("edit-1", data = reactive(dataupdated()))
    
    #DataEditR mi trasforma il tibble in un dataframe facendomi perdere tutte le informazioni sulle colonne
    #compresa la data. Quando si ritrasforma in tibble, legge le date come char. e quindi il bind non va.
    #dunque trasformo il data.frame in un tibble e trasformo la colonna Data_camp (se presente) nel formato data.
    data_to_edit_tibble = reactive({
      x = data_to_edit() %>% as_tibble()
      if("Data_campionamento" %in% colnames(x)){
        x$Data_campionamento =  as.Date(x$Data_campionamento, "%Y-%m-%d")
        return(x)
      }else{
       return(x) 
      }
      
    })
    
    #faccio un rbind tra i data originali e i nuovi ( mi serve per il rhandsontable)
    databinded = reactive({
      dplyr::bind_rows(original_data(), data_to_edit_tibble())
    })
    
    #creo una tabella di prova con l'unione dei dati
    output$dttable2 = rhandsontable::renderRHandsontable({
      databinded() %>% rhandsontable::rhandsontable(readOnly = TRUE)
    })
    
    #inizializzo un counter che ad ogni click aumenta di 1
    counter <- reactiveVal(0)
    #increment per click
    observeEvent(input$mergeaziende,{
      counter(counter() + 1)
    })
    

    
    #se è stato caricato il nuovo file ed è stato cliccato sul bottone, unisci, altrimenti dati originali
    final_data = reactive({
      if(!is.null(input$fileupaziendedata) && counter() > 0){ 
        dplyr::bind_rows(original_data(), data_to_edit_tibble())
      }else{
        original_data()
      }
    })
    
    #avvisa se ci sono più colnames dei dati originali (problema nei nomi delle colonne)
    observe({
      if(length(databinded()) > length(original_data())){
        shinyWidgets::sendSweetAlert(session = session, title = "Attenzione!", type = "warning",
                                     text = "Alcuni nomi delle colonne non coincidono. Controllare i nomi delle colonne prima di procedere con l'unione.")
      }
    })
    
    #avvisa quando è stato fatto il rbind
    observe({
      if(counter() == 1) {
        showNotification("Nuovi dati aggiunti.", type = "message")
      }
    })
    

    #avvisa quando sono stati cancellati i nuovi file e resetta il counter a 0
    observe({
      #WHEN you want the count reset
      if(counter() > 1) {
        counter(0)
        showNotification("Nuovi dati rimossi.", type = "message")
      }
    })
    
    observeEvent(input$button_test,{
      if(type_data != "azienda"  && !is.null(input$fileupaziendedata)){ 
        #se è stato caricato il nuovo file ed è stato cliccato sul bottone, unisci, altrimenti dati originali
        test_data = reactive(dplyr::bind_rows(original_data(), data_to_edit_tibble()))
       if(FALSE %in% (test_data()$Codice_azienda %in% confronto_aziende()$Codice_azienda)){
        shinyWidgets::sendSweetAlert(session = session, title = "Attenzione!", type = "warning",
                                     text = "Mancata corrispondenza con i Codici_azienda. Hai aggiornato anche i dati delle aziende?")
        
      }else{
        shinyWidgets::sendSweetAlert(session = session, title = "Ok!", type = "success",
                                     text = "I codici azienda coincidono.")
        } 
      }
    })
    
    #ritorna il dataframe che verrà usato nell'app
    return(reactive({
      x = final_data()
      if(type_data == "camp_drupe"){
        x$Indice_maturazione = factor(x$Indice_maturazione, levels = c(0:8), ordered = TRUE)
        x$Fase_fenologica = factor(x$Fase_fenologica, levels = c(51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89), ordered = TRUE)
      }else if(type_data == "polifenoli"){
        x$Presenza_larve = readr::parse_factor(as.character(x$Presenza_larve), levels = c("0","1","2"), ordered = TRUE)
        
      }
      return(x)
      
      }))
    
  })
}

## To be copied in the UI
# mod_update_data_ui("mod_update_data_ui_1")

## To be copied in the server
# mod_update_data_server("mod_update_data_ui_1")
