#' Render an UI for NA values.
#'
#' @description A shiny Module that render an UI interface with informations about NA. It renders a valuebox with the count of NA and if there are any NA, it renders also a button that if clicked will show a VIM plot.
#'
#'
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data A simple dataframe with only <double> columns. You can use dplyr::select(where(is.double) & -Anno).
#' 
#' @noRd 
#' @importFrom VIM aggr 
#' @import shinydashboard
#' @import shiny
#' 
#' 
#' @examples \dontrun{
#' 
#' ##### in SERVER #####
#'  #aggiusto i data eliminando tutte le colonne non numeriche
#'  nadatapolind = reactive({
#'   req(datapolind())
#'     data = datapolind() %>% dplyr::select(where(is.double) & -Anno)
#'       return(data)
#'   })
#'         
#' #creo il modulo per i NA
#' mod_render_NAbox_server("naboxpolind", data = nadatapolind)
#' 
#' ###### in UI #####
#' mod_render_NAbox_ui("naboxpolind")
#' }

mod_render_NAbox_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      fluidRow(
        column(width = 3, shinydashboard::valueBoxOutput(ns("namorfobox"), width = 12)),
        column(width = 3, br(), uiOutput(ns("namorfobuttui")))),
      fluidRow(
        conditionalPanel(
          condition = "input.namorfobutt != 0",
          plotOutput(ns("namorfoplot"))))
    )
    
  )
}
    
#' render_NAbox Server Function
#' 
#' @import shiny
#' @import shinydashboard
#' @importFrom VIM aggr
#' @noRd 
mod_render_NAbox_server <- function(id, data){
  
  moduleServer(id,
               function(input, output, session){
                 ns <- session$ns
                
                 output$namorfobox = shinydashboard::renderValueBox({
                   natot = sum(is.na(data()))
                   tot = sum(table(is.na(data())))
                   if(natot > 0){
                     shinydashboard::valueBox(value = p(paste0(natot,"/", tot), style = "color:white; font-size:100%;"), h4("Missing data",  style = "color:white"), icon = icon("exclamation-circle"), color = "yellow")
                   } else{
                     shinydashboard::valueBox(value = p(paste0(natot,"/", tot), style = "color:white; font-size:100%;"), h4("Missing data",  style = "color:white"), icon = icon("check"))
                   }
                 })
                 
                 output$namorfobuttui = renderUI({
                     if (sum(is.na(data())) > 0){
                       actionButton("namorfobutt", label = strong("Mostra"), class = "btn btn-warning btn-lg")
                     }
                   })

                    output$namorfoplot = renderPlot({
                     if(sum(is.na(data()) > 0)){
                       VIM::aggr(data(), cex.axis = .9, numbers = T, oma = c(12,5,3,2))
                     }
                   })

                 
               }
                 
                 )



}
    
## To be copied in the UI
# mod_render_NAbox_ui("nome-modulo")
    
## To be copied in the server
# Module_Server("nome-modulo", data = nadatamorfo) #non usare () in data o non funziona

 
