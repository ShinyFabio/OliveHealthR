#' mod_render_map UI Function
#'
#' @description A shiny Module that renders two map.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param datamap A dataframe (eg. datadrupemap()) joined with the data() so that contains also the coordinates.
#' @param datacol A dataframe (e.g. datadrupe()) not joined with data().
#' @param extract_year Logical value. If TRUE will be created a column "Anno" from "Data_campionamento" using lubridate::year(), otherwise will be used a pre-existing column "Anno".
#' @param extract_ncamp Logical value. If TRUE data will be filtered by "N_campionamento". 
#'
#' @noRd 
#'
#' @import shiny
#' @import tmap
#' @import tmaptools
#' @importFrom shinycssloaders withSpinner
mod_render_map_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    sidebarLayout(
      sidebarPanel(width = 3,
        div(actionButton(ns("updatemap_mod"), "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
        conditionalPanel(condition = "input.updatemap_mod != 0", ns = ns,
          hr(),
          selectInput(ns("selectcolmap1"), "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
          selectInput(ns("selyearmap1"), "Seleziona l'anno", choices = "", multiple = FALSE),

          uiOutput(ns("ncamp_mod1")),

          br(), 
          hr(),
          br(),
          div(actionButton(ns("addmap_mod2"), label = "Aggiungi seconda mappa"), align = "center"),
        ),
        conditionalPanel(condition = "input.addmap_mod2 != 0", ns = ns,
          br(),
          selectInput(ns("selectcolmap2"), "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
          selectInput(ns("selyearmap2"), "Seleziona l'anno", choices = "", multiple = FALSE),
          uiOutput(ns("ncamp_mod2"))
        )
      ), 
      
      mainPanel(
        conditionalPanel(condition = "input.updatemap_mod != 0", ns = ns,
                         shinycssloaders::withSpinner(tmapOutput(ns("mappa1")), image =  "www/running_olive.gif")),
        conditionalPanel(condition = "input.addmap_mod2 != 0", ns = ns,
                         hr(),
                         shinycssloaders::withSpinner(tmapOutput(ns("mappa2")), image =  "www/running_olive.gif"))
      )
    ) #end of sidebarlayout
    
    
  )
}
    
#' mod_render_map Server Functions
#'
#' @noRd 
#' 
#' @importFrom dplyr mutate select filter
#' @importFrom lubridate year

mod_render_map_server <- function(id, datamap, datacol, extract_year, extract_ncamp){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$ncamp_mod1 = renderUI({
      
      if(extract_ncamp == TRUE){
        selectInput(ns("num1map"), "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
      }
    })

    
    observeEvent(datacol(), {
      updateSelectInput(session, "selectcolmap1", choices = colnames(datacol()))
    })
    
    
    #crea la colonna anno #dtdrupanno
    datayear = reactive({
      req(datamap())
      if(extract_year == TRUE){
        datamap() %>% dplyr::mutate(Anno = lubridate::year(Data_campionamento))
      }else{
        datamap()
      }
    })
    
    
    #aggiorna il selectinput "selyear" in base agli anni presenti
    observeEvent(datayear(), {
      updateSelectInput(session, "selyearmap1", choices = row.names(table(dplyr::select(datayear(), Anno))))
    })
    
    
    #stampo mappa
    output$mappa1 = renderTmap({
      req(datayear())
      #filtra in base all'anno selezionato e il campionamento
      datamap1 = datayear() %>% dplyr::filter(Anno == input$selyearmap1)
      if(extract_ncamp == TRUE){
        datamap = datamap1 %>% dplyr::filter(N_campionamento == input$num1map)
      }else{
        datamap = datamap1
      }
      
      colmap = Olv_select_col(data = datacol(), input = input$selectcolmap1)
      make_tmap(data = datamap, dotlegend = colmap)
    })
    
    
    #### seconda mappa
    
    
    output$ncamp_mod2 = renderUI({
      
      if(extract_ncamp == TRUE){
        selectInput(ns("num2map"), "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
      }
    })
    
    
    observeEvent(datacol(), {
      updateSelectInput(session, "selectcolmap2", choices = colnames(datacol()))
    })
    
    #aggiorna il selectinput "selyear" in base agli anni presenti
    observeEvent(datayear(), {
      updateSelectInput(session, "selyearmap2", choices = row.names(table(dplyr::select(datayear(), Anno))))
    })
    
    
    output$mappa2 = renderTmap({
      req(datayear())
      #filtra in base all'anno selezionato e il campionamento
      datamap1 = datayear() %>% dplyr::filter(Anno == input$selyearmap2)
      if(extract_ncamp == TRUE){
        datamap = datamap1 %>% dplyr::filter(N_campionamento == input$num2map)
      }else{
        datamap = datamap1
      }
      
      colmap = Olv_select_col(data = datacol(), input = input$selectcolmap2)
      make_tmap(data = datamap, dotlegend = colmap)
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_render_map_ui("mod_render_map_ui_1")
    
## To be copied in the server
# mod_render_map_server("mod_render_map_ui_1")
