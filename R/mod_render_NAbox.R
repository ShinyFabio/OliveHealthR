#' Render an UI for NA values.
#'
#' @description A shiny Module that renders an UI interface with informations about NA. It renders a valuebox with the count of NA and if there are any NA, it renders also a button that if clicked will show a VIM plot.
#'
#'
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data A simple dataframe with only <double> columns. You can use dplyr::select(where(is.double) & -Anno).
#' @param margins Margins of the plot. The default is c(12,5,3,2) where c(bottom, left, right, up). Increase bottom if labels are cutted.
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
        column(width = 3, br(), 
               conditionalPanel(condition = "output.check_na == 'yes'", ns = ns,
                 actionButton(ns("namorfobutt"), label = strong("Mostra"), class = "btn btn-warning btn-lg")
               ))
        ),
      fluidRow(
        column(12,
        conditionalPanel(
          condition = "input.namorfobutt != 0", ns = ns,
          plotOutput(ns("namorfoplot"), height = "600px")))
    ))
    
  )
}
    
#' render_NAbox Server Function
#' 
#' @import shiny
#' @import shinydashboard
#' @importFrom VIM aggr
#' @noRd 
mod_render_NAbox_server <- function(id, data, text_size = 0.9, margins = c(12,5,3,2)){
  
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
                 
                 # output$namorfobuttui = renderUI({
                 #     if (sum(is.na(data())) > 0){
                 #       actionButton("namorfobutt", label = strong("Mostra"), class = "btn btn-warning btn-lg")
                 #     }
                 #   })
                 # 
                 
                 output$check_na = reactive({
                   if (sum(is.na(data())) > 0){
                     "yes"
                   }else{"no"}
                 })
                 outputOptions(output, 'check_na', suspendWhenHidden = FALSE)

                 output$namorfoplot = renderPlot({
                   if(sum(is.na(data()) > 0)){
                     VIM::aggr(data(), cex.axis = text_size, numbers = T, oma = margins)
                   }
                 })
                 
                 
               }
               
  )
  
}
    
## To be copied in the UI
# mod_render_NAbox_ui("nome-modulo")
    
## To be copied in the server
# mod_render_NAbox_server("nome-modulo", data = nadatamorfo) #non usare () in data o non funziona

 
