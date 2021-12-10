#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @importFrom plotly plotlyOutput
#' @import tmap
#' @import tmaptools
#' @import htmltools
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs hidden
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$head(tags$style(type = 'text/css', ".navbar-default{display:none;}")), #rimuove la barra "Home | Codice"
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    shinyjs::useShinyjs(),
    
    # Your application UI logic 
    navbarPage(title = "OliveHealthR", theme = shinytheme("spacelab"), id = "navb1", 
                #margin-right:auto; margin-left:auto;
      #tags$head(tags$style(type = "text/css", ".container-fluid{padding-left:0px; padding-right:0px ; }")),         
      tabPanel(title = "Welcome", value = "panel1",
               tags$img(src = "www/Photo_2.jpg", width = "100%"),
               h2(strong("Cos'è OliveHealthR?"), style = "text-align: center"),
               br(), br(),
               fluidRow(
                 column(1, offset=1,tags$img(src = "www/OliveHealthRfavicon.png", width = "173", height = "200")),
                 column(width = 6, offset = 1,
                   p(h4("OliveHealthR è un software che svolge analisi sui dati provenienti dal progetto OliveHealth. L'obiettivo principale del progetto è quello di identificare le
                 componenti salutistiche (es. polifenoli) in prodotti della filiera olivicola (quali foglie, drupe e
                 olio) correlandole alla geo-localizzazione di ciascun appezzamento. Sulla base di queste informazioni
                 verranno prodotti dataset delle variabili rappresentative delle principali caratteristiche fenotipiche,
                 biochimiche e genetiche associate all’ulivo dalle quali nascerà un database messo a disposizione ai
                 produttori olivicoli campani.", "Per maggiori informazioni sul progetto clicca",
                                      strong(a(href = "https://olivehealth.it", "qui", .noWS = "after"), .noWS = "after"),
                                      ".", style = "font-family:'Quicksand', Sans-serif; font-weight: 400; line-height: 1.5; text-align: justify;"
                   )),
                 
                 ),
                 column(1, offset = 1, tags$img(src = "www/Olivehealt_Logo.png", width = "200", height = "200"))
               ), 
                        
               fluidRow(column(1, offset = 5, 
                               br(),
                               actionButton("jumpToP2", label = HTML("&nbsp;VAI!"), icon("rocket"), class = "btn btn-primary btn-lg", width = "200px", style='padding:10px; font-size:200%; font-weight: bold;')
               )
               ), 
                        
               
               p(h4("Questo progetto è stato finanziato da:")),
               fluidRow(
                 column(1, tags$img(src = "www/Logo_AprolCampania.jpg", width = "225", height = "150")),
                 column(2, offset = 4,  tags$img(src = "www/CNR.png", width = "150", height = "155")),
                 column(3, offset = 2, tags$img(src = "www/stringa-2.png", width = "423", height = "110"))
               )
      ), 
               
               ##### Lista menuItem #### 
               tabPanel(title = "Codice", value = "panel2", 
                        shinyBS::bsModal("keybutton_modal", "Inserisci codice", trigger = "keybutton", size = "small",
                                         br(),textInput("ins_passw", label = "", placeholder = "Scrivi qui il codice.")),
                        dashboardPage(
                          dashboardHeader(title = "OliveHealthR", 
                            tags$li(class = "dropdown",
                              actionBttn("keybutton", icon = icon("key"), style = "stretch", size = "lg" , color = "primary"),
                              actionBttn("jumptohome", icon = icon("home"), style = "stretch", size = "lg" , color = "primary")
                              )),
                          dashboardSidebar(
                            sidebarMenu(
                              menuItem("File", tabName = "filemenuitem", icon = icon("file-import")),
                              menuItem("Azienda", tabName = "azienda", icon = icon("id-card")),
                              menuItem("Campionamento azienda", tabName = "campazienda", icon = icon("file-alt"),
                                       menuSubItem("Drupe e foglie", tabName = "drupleafsub"),
                                       menuSubItem("Olio", tabName = "oliosub"),
                                       menuSubItem("Calendario campionamenti", tabName = "calendar"),
                                       menuSubItem("Analisi sensoriali", tabName = "assaggsub")),
                              menuItem("Analisi laboratorio", tabName = "anlab", icon = icon("flask"),
                                       menuSubItem("Polifenoli totali", tabName = "totpolsub"),
                                       menuSubItem("Polifenoli individuali", tabName = "inpolsub"),
                                       shinyjs::hidden(menuSubItem("Polifenoli LCxLC", tabName = "lcpolsub")),
                                       menuSubItem("Analisi morfometrica", tabName = "anmorfosub")),
                              menuItem("Integrazione dati", tabName = "integrdati", icon = icon("vials"))
                            )
                          ), 
                          dashboardBody(
                            tabItems(
                              
                              ##### tabitem File ####
                              tabItem(tabName = "filemenuitem",
                                fluidRow(
                                  #schede aziende
                                  column(3, 
                                    box(width = NULL, status = "primary", title = h3(strong("Aziende"), style = "color: white; display:inline; margin-top: 0px;margin-bottom: 0px;"), solidHeader = T,
                                        uiOutput("valbox_aziende")
                                    )
                                  ),
                                  
                                  #schede campionamento
                                  column(3,
                                    box(width = NULL, status = "primary", title = h3(strong("Schede campionamento"), style = "color: white; display:inline; margin-top: 0px;margin-bottom: 0px;"), solidHeader = T,
                                        uiOutput("valbox_drupecamp"),
                                        uiOutput("valbox_oliocamp"),
                                        uiOutput("valbox_assaggi")
                                        )
                                  ),
                                  #polifenoli
                                  column(3,
                                         box(width = NULL, status = "primary", title = h3(strong("Polifenoli"), style = "color: white; display:inline; margin-top: 0px;margin-bottom: 0px;"), solidHeader = T,
                                             uiOutput("valbox_poltot"),
                                             uiOutput("valbox_polind")
                                         )
                                  ),
                                  #morfometria
                                  column(3,
                                         box(width = NULL, status = "primary", title = h3(strong("Morfometria"), style = "color: white; display:inline; margin-top: 0px;margin-bottom: 0px;"), solidHeader = T,
                                         uiOutput("valbox_morfo"))
                                  )
                                ), #end of fluidrow
                                fluidRow(
                                  column(4, offset = 4, style = "text-align:center;", br(), br(), br(), br(), br(),
                                         actionButton("load_files", label = HTML("&nbsp;Carica tutti dati!"), icon("file-upload"), class = "btn btn-primary btn-lg", width = "300px", style='padding:10px; font-size:200%; font-weight: bold;')
                                  )
                                )

                              ),
                              
                              ##### tabItem Home ####
                              tabItem(tabName = "azienda",
                                tabsetPanel(
                                  
                                  tabPanel("Tabella",
                                    box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                        DT::DTOutput("content")
                                    ), 
                                    mod_update_data_ui("updataaziende")
                                  ), 
                                  
                                  
                                  tabPanel("Cultivar",
                                    box(width=NULL, status = "primary", h4(htmlOutput("numcult"))),
                                    br(),
                                    fluidRow(
                                      column(2,
                                             box(width = NULL, status = "primary",
                                                 radioButtons("selplotcult", label = h4("Tipo di grafico"), choices = list("Grafico a torta" = 1, "Grafico a barre" = 2), selected = 2))
                                      ),
                                      column(width=10, box(width=NULL, status = "primary", shinycssloaders::withSpinner(image = "www/running_olive.gif", uiOutput("cultplot"))))
                                    )
                                  ),
                                  
                                  tabPanel("Mappa",
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                        div(actionButton("update", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                        hr(), 
                                        selectInput("select3", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE)
                                      ),
                                      mainPanel(width = 9,
                                        conditionalPanel(condition = "input.update != 0",
                                          shinycssloaders::withSpinner(tmapOutput("map1")),
                                        )
                                      )
                                    )
                                  )
                                ) #end of tabset
                              ),
                              
                              
                              ##### TabItem Campionamento Azienda ####
                              
                              ##### campionamento foglie e drupe #########################################
                              tabItem(tabName = "drupleafsub",
                                tabBox(width = 12,
                                  tabPanel(
                                    tagList(shiny::icon("table"), HTML("&nbsp;Tabella")),
                                    #qui aggiungere la tabella drupe foglie
                                    fluidPage(
                                      box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                          DT::DTOutput("tabledrupscheda")),
                                      #mod_render_NAbox_ui("naboxpoltot")
                                      mod_update_data_ui("updatadrupe")
                                      
                                    )
                                    
                                  ),
                                  
                                  tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")),
                                    #qui i grafici drupe foglie
                                    sidebarLayout(
                                      sidebarPanel(width = 2,
                                        selectInput("selectx", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                        selectInput("selecty", "Seleziona la colonna Y", choices = "", multiple = FALSE)
                                      ),
                                      
                                      mainPanel(width = 10,
                                        tabsetPanel(
                                          tabPanel("Scatter plot",
                                            br(),
                                            box(width=NULL, status = "primary",
                                              fluidRow(
                                                column(3, selectInput("selyearscatter", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                column(3, selectInput("num2", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)),
                                                column(3, selectInput("selectfill", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                              )),
                                            
                                            shinycssloaders::withSpinner(plotly::plotlyOutput("plotxy"), image = "www/running_olive.gif")
                                          ),
                                          
                                          tabPanel("Barplot",
                                            fluidRow(column(width = 2, br(),
                                                            box(width = NULL, status = "primary", checkboxGroupInput("checkcamp", "Seleziona campionamento", choices = ""))
                                                           ),
                                                     column(width = 10, br(), 
                                                            box(width=NULL, status = "primary", 
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput("barplot1"), image = "www/running_olive.gif"))
                                                     )
                                            )
                                            
                                          )#end of tabpanel
                                        )#end of tabset
                                      )#end of mainpanel
                                    )#end of sidebarlayout
                                  ), #end of tabpanel grafici
                                  
                                  
                                  tabPanel(tagList(shiny::icon("images"), HTML("&nbsp;Galleria")),
                                    #qui le foto drupe foglie
                                    fluidPage(
                                      fluidRow(
                                        column(width=4, 
                                          fluidRow(
                                            column(6,
                                              box(width = NULL, status = "primary",
                                                radioGroupButtons(inputId = "campfoto", label = "Numero campionamento",  choices = c("1" = "1_campionamento", "2" = "2_campionamento"),
                                                                  individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                              )
                                            ),
                                            
                                            column(6, box(width=NULL, status = "primary", 
                                                          selectInput("selyearfoto", "Seleziona l'anno", choices = "", multiple = FALSE)
                                            ))
                                          ),
                                          
                                          fluidRow(column(12, box(width=NULL, status = "primary", style = "overflow-x: scroll;", DT::DTOutput("prov2"))))
                                        ),
                                        conditionalPanel(condition = "input.prov2_rows_selected == 0",
                                                         p(strong(h4("Per favore seleziona un'azienda dalla tabella", align = "center")))
                                        ),
                                        
                                        conditionalPanel(condition = "input.prov2_rows_selected != 0",
                                                         column(width = 4, box(width=NULL, status = "primary", title = "Foglie", align= "center", uiOutput("phfoglia"))),
                                                         column(width = 4, box(width=NULL, status = "primary", title = "Drupe",align = "center", uiOutput("phdrupa")))
                                        )
                                      ) #end of fluidRow
                                    )
                                  ), #end of tabpanel foto
                                  
                                  
                                  tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")),
                                    mod_render_map_ui("modulo_mappa_datadrupe")
                                  ) #end of tabpanel mappa drupe foglie
                                  
                                  
                                ) #end of tabBox
                              ), #end of tabitem drupe foglie
                              
                              
                              ##### campionamento olio #########################################
                              tabItem(tabName = "oliosub",
                                tabBox(width = 12,
                                  tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")),
                                    #qui aggiungere la tabella drupe foglie
                                    fluidPage(
                                      box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                          DT::DTOutput("tableolioscheda")),
                                      #mod_render_NAbox_ui("naboxpoltot")
                                    )
                                  ),
                                  
                                  tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")),
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 3,
                                        div(actionButton("updateoilmap", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                        conditionalPanel(condition = "input.updateoilmap != 0",
                                          hr(),
                                          selectInput("selcoloilmap", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                          selectInput("selyearoilmap", "Seleziona l'anno", choices = "", multiple = FALSE)
                                        )
                                      ),
                                      mainPanel(width = 9,
                                        conditionalPanel(condition = "input.updateoilmap != 0",
                                          shinycssloaders::withSpinner(tmapOutput("mapolio"))),
                                      )
                                    )
                                    
                                    )
                                )
                              ),
                              
                              ##### Calendario #########
                              tabItem(tabName = "calendar",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    awesomeRadio("selfilecalend", "Seleziona il tipo di campionamento", choices = ""),
                                    conditionalPanel(condition = "input.selfilecalend == 'Drupe e foglie'",
                                      awesomeRadio("selcampcalend", "Seleziona il numero di campionamento", choices = c("Entrambi", "R1", "R2")),
                                    ),
                                    selectInput("selyearcalend", "Seleziona l'anno", choices = "", multiple = FALSE),
                                    selectInput("selaziendacalend", "Scegli l'azienda", choices = "", multiple = FALSE),
                                  ),
                                  mainPanel(width = 10,
                                     shinycssloaders::withSpinner(image = "www/running_olive.gif",plotOutput("yearcalendar", height = "700px"))
                                  )
                                )
                              ),
                              
                              
                              ###### Assaggi sensoriali ########
                              tabItem(tabName = "assaggsub",
                                tabBox(width = 12,
                                  #tabella
                                  tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")),
                                    box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                        DT::DTOutput("tableassaggischeda")),
                                  ),
                                  
                                  #grafici
                                  tabPanel(
                                    tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")),
                                    tabsetPanel(
                                      
                                      tabPanel("Scatter plot",
                                        sidebarLayout(
                                          sidebarPanel(width = 2,
                                            selectInput("selectxassaggiscatt", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                            selectInput("selectyassaggiscatt", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                          ), 
                                        
                                          mainPanel(width = 10,
                                            br(),
                                            box(width=NULL, status = "primary",
                                              fluidPage(
                                                fluidRow(
                                                  column(3, selectInput("selyearscatterassagg", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                  column(3, selectInput("selectfillassaggi", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                              ))),
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("scattplotassagg"))
                                          )
                                        )
                                      ), #end of tabpanel scatter plot
                                      
                                      
                                      tabPanel("Barplot",
                                        sidebarLayout(
                                          sidebarPanel(width = 2,
                                            awesomeRadio("barplotassaggi", "Tipo di barplot", choices = c("Affiancato" = "dodge", "Impilato" = "stack")),
                                            selectInput("selyearbarassagg", "Seleziona l'anno", choices = "", multiple = FALSE),
                                          ),
                                          
                                          mainPanel(width = 10,
                                            br(),
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("barplotassagg"))
                                          )
                                        )
                                      ), #end of tabpanel barplot
                                      
                                      tabPanel("Spiderplot",
                                        sidebarLayout(
                                          sidebarPanel(width = 2,
                                            selectInput("selyearspiderassaggi", "Seleziona l'anno", choices = "", multiple = FALSE),
                                            selectInput("selcodspiderassaggi1", "Seleziona un'azienda", choices = "", multiple = FALSE),
                                            awesomeCheckbox("addcodspiderassaggi", "Aggiungi seconda azienda", value = FALSE),
                                            conditionalPanel(condition = "input.addcodspiderassaggi == true",
                                              selectInput("selcodspiderassaggi2", "Seleziona un'azienda", choices = "", multiple = FALSE)
                                            )
                                          ),
                                          
                                          mainPanel(width = 10,
                                            div(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotOutput("assaggispider", width = "100%",height = "600px")), align = "center"),
                                          )
                                        )
                                      ) #end of tabpanel spiderplot
                                      
                                      
                                    ) #end of tabsetpanel
                                  ), #end of tabpanel grafici
                                  
                                      
                                      

                                  #galleria
                                  tabPanel(tagList(shiny::icon("images"), HTML("&nbsp;Galleria")),
                                    fluidPage(
                                      fluidRow(
                                        column(width=4, 
                                               fluidRow(
                                                 column(6, box(width=NULL, status = "primary", 
                                                               selectInput("selyearallegatph", "Seleziona l'anno", choices = "", multiple = FALSE))
                                                 )
                                               ),
                                               
                                               fluidRow(column(12, box(width=NULL, status = "primary", DT::DTOutput("dtallegato"))))
                                        ),
                                        
                                        column(width = 8,
                                               column(width = 6, offset = 3,
                                                 conditionalPanel(condition = "input.dtallegato_rows_selected == 0",
                                                   box(width = NULL, background = "yellow", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 27px"), h4(strong("Seleziona un'azienda dalla tabella"), style = "color: white"), style = "text-align: justify;  text-align: center;"),
                                                 )
                                               ),
                                               
                                               conditionalPanel(condition = "input.dtallegato_rows_selected != 0",
                                                 uiOutput("phallegati")
                                               )
                                        )
                                      ) #end of fluidRow
                                    )
                                    
                                  ),
                                  
                                  #mappa
                                  tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")),
                                    mod_render_map_ui("modulo_mappa_assaggi")
                                    ) #end of tabpanel mappa assaggi
                                  

                                ) #end of tabBox
                              ), #end of tabItem
                              
                            
                          
                              
                              
                              ##### TabItem Analisi Laboratorio #####
                              
                              # Tab polifenoli totali ----------------------------------------------------------------
                              tabItem(tabName = "totpolsub",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    radioGroupButtons("selfilepoltot", "Seleziona i polifenoli da analizzare", 
                                                      choiceValues = list("foglie", "drupe", "olio", "posa", "sansa"),
                                                      choiceNames = list(
                                                        paste(shiny::icon("leaf",  style='font-size:16px;'), HTML("<b style=font-size:16px>&nbsp;Foglie</b>")),
                                                        paste(tags$img(src = "www/olive_icon2.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Drupe</b>")),
                                                        paste(tags$img(src = "www/olive_oil.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Olio</b>")),
                                                        paste(tags$img(src = "www/posa.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Posa</b>")),
                                                        paste(tags$img(src = "www/sansa3.png", height = "23px", width = "23px"), HTML("<b style=font-size:16px>&nbsp;Sansa</b>"))),
                                                      direction = "vertical", justified = TRUE, status = "primary"
                                    ),
                                    hr(),
                                    conditionalPanel(condition = "input.tabboxpoltot  == 'tabdtpoltot' || (input.tabboxpoltot == 'tabpoltotgraph' && input.boxpoltotgraph  == 'boxscattpoltot')",
                                      materialSwitch(inputId = "summpoltot", label = "Sintetizza i dati", value = TRUE, status = "primary")
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabboxpoltot  == 'tabpoltotgraph'",
                                      h4(strong("Impostazioni grafici")),
                                      #scatterplot
                                      conditionalPanel(condition = "input.boxpoltotgraph == 'boxscattpoltot'",
                                        selectInput("selectxtot", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                        selectInput("selectytot", "Seleziona la colonna Y", choices = "", multiple = FALSE)
                                      ),
                                      #barplot
                                      conditionalPanel(condition = "input.boxpoltotgraph == 'boxbarpoltot'",
                                        selectInput("selectxtotbar", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                        selectInput("selectytotbar", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                        selectInput("selectfilltotbar", "Colonna da usare come riempimento", choices = c("N_campionamento", "Cultivar_principale"), selected = "N_campionamento"),
                                        hr(),
                                        checkboxGroupInput("checkcamptot", "Seleziona campionamento", choices = "")
                                      )
                                    ),
                                    
                                    # MAPPA __________________________________________________________
                                    conditionalPanel(condition = "input.tabboxpoltot == 'tabpoltotmap'",
                                      div(actionButton("updatepoltotmap", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                      conditionalPanel(condition = "input.updatepoltotmap != 0", 
                                        hr(),
                                        selectInput("colpoltotmap1", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                        selectInput("yearpoltotmap1", "Seleziona l'anno", choices = "", multiple = FALSE),
                                        selectInput("numpoltotmap1", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                        hr(),
                                        div(actionButton("addmappoltot2", label = "Aggiungi seconda mappa"), align = "center"),
                                      ),
                                      conditionalPanel(condition = "input.addmappoltot2 != 0",
                                        br(),
                                        selectInput("colpoltotmap2", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                        selectInput("yearpoltotmap2", "Seleziona l'anno", choices = "", multiple = FALSE),
                                        selectInput("numpoltotmap2", "Scegli il numero di campionamento", choices = "", multiple = FALSE),                                                     
                                      )
                                    )
                                  ), 
                                  
                                  mainPanel(width = 10,
                                    tabBox(id = "tabboxpoltot", width = NULL,
                                           
                                      tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), value = "tabdtpoltot",
                                               fluidPage(
                                                 box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                                     DT::DTOutput("tablepoltot")),
                                                 mod_update_data_ui("updatapol"),
                                                 br(),
                                                 mod_render_NAbox_ui("naboxpoltot")
                                                 
                                               )
                                      ), #end of tabpanel tabella
                                      
                                      
                                      tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")), value = "tabpoltotgraph",
                                               tabsetPanel(id = "boxpoltotgraph",
                                                 
                                                           
                                                 tabPanel("Scatter plot", value = "boxscattpoltot",
                                                   box(width=NULL, status = "primary",
                                                     fluidRow(
                                                       column(3, selectInput("selyearscattertot", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                       column(3, selectInput("numtot", "Scegli il numero di campionamento", choices = "", multiple = FALSE)),
                                                       column(3, selectInput("selectfilltot", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                                     )),
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("totscatplot"))
                                                 ), #end of tabpanel scatter plot
                                                 
                                                 
                                                 
                                                 tabPanel("Barplot", value = "boxbarpoltot",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("barplottot"))
                                                 ) #end of tabpanel barplot
                                                 
                                               ) #end of tabsetpanel grafici
                                      ), #end of tabpanel grafici
                                      
                                      #tabpanel mappa
                                      tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")), value = "tabpoltotmap",
                                        conditionalPanel(condition = "input.updatepoltotmap != 0", 
                                          shinycssloaders::withSpinner(tmapOutput("poltotmap1"))),
                                        conditionalPanel(condition = "input.addmappoltot2 != 0", 
                                          hr(),
                                          shinycssloaders::withSpinner(tmapOutput("poltotmap2")))
                                      )
                                      
                                    ) #end of tabbox poltot
                                  ) #end of mainpanel
                                )#end of sidebarlayout
                                
                                
                              ), #end of tabitem "totpolsub"
                              
                              # Tab polifenoli individuali ----------------------------------------------------------------------------
                              tabItem(tabName = "inpolsub",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    radioGroupButtons("selfilepolind", "Seleziona i polifenoli da analizzare", 
                                                      choiceValues = list("foglie", "drupe", "olio", "posa"),
                                                      choiceNames = list(
                                                        paste(shiny::icon("leaf",  style='font-size:16px;'), HTML("<b style=font-size:16px>&nbsp;Foglie</b>")),
                                                        paste(tags$img(src = "www/olive_icon2.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Drupe</b>")),
                                                        paste(tags$img(src = "www/olive_oil.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Olio</b>")),
                                                        paste(tags$img(src = "www/posa.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Posa</b>"))
                                                        ),
                                                      direction = "vertical", justified = TRUE, status = "primary"
                                    ),
                                    hr(),
                                    conditionalPanel(
                                      condition = "input.tabboxpolind  == 'tabdtpolind' || input.tabboxpolind == 'tabpolindpca' || 
                                      (input.tabboxpolind == 'tabpolindgraph' && input.boxpolindgraph  == 'boxscattpolind' )",
                                      materialSwitch(inputId = "summpolind", label = "Sintetizza i dati", value = TRUE, status = "primary")
                                    ), 

                                    conditionalPanel(
                                      condition = "input.tabboxpolind  == 'tabpolindgraph'",
                                      h4(strong("Impostazioni grafici")),
                                      
                                      # Scatterplot ______________________________________________________
                                      conditionalPanel(condition = "input.boxpolindgraph == 'boxscattpolind'",
                                                       selectInput("selectxind", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                                       selectInput("selectyind", "Seleziona la colonna Y", choices = "", multiple = FALSE)
                                      ),
                                      
                                      # Boxplot ______________________________________________________
                                      conditionalPanel(condition = "input.boxpolindgraph == 'boxboxplotpolind'",
                                                       selectInput("selectyboxind", "Seleziona un polifenolo", choices = "", multiple = FALSE)
                                      ),
                                      
                                      # Barplot ____________________________________________________________
                                      conditionalPanel(condition = "input.boxpolindgraph == 'boxbarpolind'",
                                        selectInput("selectyindbar", "Seleziona un polifenolo", choices = "", multiple = FALSE),
                                        selectInput("selectfillindbar", "Colonna da usare come riempimento", choices = c("N_campionamento", "Cultivar_principale"), selected = "N_campionamento"),
                                        hr(),
                                        checkboxGroupInput("checkcampind", "Seleziona campionamento", choices = "")
                                      ),
                                    
                                    # Heatmap _______________________________________________________
                                    conditionalPanel(condition = "input.boxpolindgraph == 'boxheatpolind'",
                                      div(actionButton("updateheat", label = "Carica!", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:150%; font-weight: bold;'), align= "center"),
                                      br(),
                                      h4(strong("Dati")),
                                      selectInput("numheat", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                      selectInput("selyearheatind", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      
                                      hr(),
                                      h4(strong("Preprocessing")),
                                      selectInput("selscaleheat", "Scala i dati:", choices = c("No" = "none", "Per riga" = "row", "Per colonna" = "column"), selected = "none"),
                                      hr(),
                                      h4(strong("Opzioni dendrogramma")),
                                      
                                      ###voglio il dendrograma su riga o colonna o entrambi?
                                      h5(strong("Scegli dove mostrare il dendrogramma")),
                                      fluidRow(
                                        column(6, materialSwitch(inputId = "rowdend", label = "Riga",  value = TRUE, status = "primary", width = "90%")),
                                        column(6, materialSwitch(inputId = "columndend", label = "Colonna",  value = TRUE, status = "primary", width = "90%"))
                                      ),
                                      selectInput("selectannot", "Colonna annotazione:", choices = c("Provincia", "Cultivar_principale", "Areale")),
                                      
                                      conditionalPanel(condition = "input.rowdend == 1 || input.columndend == 1",
                                        selectInput("seldistheatpol", "Funzione di distanza:", choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski"), selected = "euclidean"),
                                        selectInput("selhclustheatpol", "Metodo clustering:", choices = c("ward.D", "ward.D2", "single", "complete", "average" , "mcquitty", "median", "centroid"), selected = "complete"),
                                      ),
                                      
                                      conditionalPanel(condition = "input.rowdend == 0",
                                                       h5(strong("Ordinare i dati per annotazione?")),
                                                       awesomeCheckbox("heatsort", label = "Ordina", value = TRUE)
                                      ),
                                      
                                      conditionalPanel(condition = "input.rowdend == 1",
                                                       hr(),
                                                       h4(strong("Dendrogramma su riga")),
                                                       sliderInput("sliderrowheat", "Numero cluster:", min = 2, max = 10, value = 2),
                                      ),
                                      
                                      conditionalPanel(condition = "input.columndend == 1",
                                                       hr(),
                                                       h4(strong("Dendrogramma su colonna")),
                                                       uiOutput("sliderheatcol"),
                                      )
                                        
                                    ), #end of conditional heatmap
                                      
                                    conditionalPanel(condition = "input.boxpolindgraph == 'boxspiderpolind'",
                                      selectInput("selyearspiderpolind", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("selcampspiderpolind", "Seleziona il numero di campionamento", choices = "", multiple = FALSE),
                                      selectInput("selcodspiderpolind", "Seleziona un'azienda", choices = "", multiple = FALSE),
                                      awesomeCheckbox("addcodspiderpolind", "Aggiungi seconda azienda", value = FALSE),
                                      conditionalPanel(condition = "input.addcodspiderpolind == true",
                                                       selectInput("selcodspiderpolind2", "Seleziona un'azienda", choices = "", multiple = FALSE)
                                      )
                                    ),
                                      
                                    # Correlation _________________________________________________________________
                                    conditionalPanel(
                                      condition = "input.boxpolindgraph == 'boxcorrpolind'",
                                      selectInput("selyearcorrind", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numcorr", "Scegli il numero di campionamento", choices = "", multiple = FALSE)
                                    ),
                                    ), #end of conditional panel dei grafici
                                    
                                    
                                      
                                    # PCA ____________________________________________________________________
                                    conditionalPanel(
                                      condition = "input.tabboxpolind == 'tabpolindpca'",
                                      selectInput("selyearpca", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numpca", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                      radioGroupButtons(inputId = "selcorpca", label = "Matrice:", choices = c("Correlazione" = TRUE, "Covarianza" = FALSE), 
                                                        individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                    ),
                                    
                                    
                                    # MAPPA __________________________________________________________
                                    conditionalPanel(condition = "input.tabboxpolind == 'tabpolindmap'",
                                      div(actionButton("updatepolindmap", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                      conditionalPanel(condition = "input.updatepolindmap != 0", 
                                        hr(),
                                        selectInput("colpolindmap1", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                        selectInput("yearpolindmap1", "Seleziona l'anno", choices = "", multiple = FALSE),
                                        selectInput("numpolindmap1", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                        hr(),
                                        div(actionButton("addmappolind2", label = "Aggiungi seconda mappa"), align = "center"),
                                      ),
                                      conditionalPanel(condition = "input.addmappolind2 != 0",
                                        br(),
                                        selectInput("colpolindmap2", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                        selectInput("yearpolindmap2", "Seleziona l'anno", choices = "", multiple = FALSE),
                                        selectInput("numpolindmap2", "Scegli il numero di campionamento", choices = "", multiple = FALSE),                                                     
                                      )
                                    )
                                      
                                  ), #end of sidebarpanel
                                  
                                  
                                  
                                  mainPanel(width = 10,
                                    tabBox(id = "tabboxpolind", width = NULL,
                                           
                                      tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), value = "tabdtpolind", 
                                        fluidPage(
                                          box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                              DT::DTOutput("tablepolind")),
                                          mod_render_NAbox_ui("naboxpolind")
                                        )
                                      ), #end of tabpanel tabella
                                      
                                      
                                      
                                      tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")), value = "tabpolindgraph",
                                        tabsetPanel(id = "boxpolindgraph",

                                          tabPanel("Scatter plot", value = "boxscattpolind",
                                            box(width=NULL, status = "primary",
                                              fluidRow(
                                                column(3, selectInput("selyearscatterind", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                column(3, selectInput("numind", "Scegli il numero di campionamento", choices = "", multiple = FALSE)),
                                                column(3, selectInput("selectfillind", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                              )),
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("scatterindpol"))
                                          ), #end of tabpanel scatterplot

                                          tabPanel("Boxplot", value = "boxboxplotpolind",
                                                   box(width=NULL, status = "primary",
                                                       fluidRow(
                                                         column(3, selectInput("selyearboxind", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                         column(3, selectInput("numboxind", "Scegli il numero di campionamento", choices = "", multiple = FALSE)),
                                                         column(3, selectInput("selectfillboxind", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                                       )),
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("boxplotindpol"))
                                          ), 
                                          
                                          tabPanel("Barplot", value = "boxbarpolind",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("barplotind"))
                                          ), #end of tabpanel barplot
                                                 
                                                 
                                                 
                                          tabPanel("Heatmap", value = "boxheatpolind",
                                            InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_polind_output", layout = "1|(2-3)", width1 = 850, height1 = 550)
                                          ),
                                                 

                                          tabPanel("Correlation Plot", value = "boxcorrpolind",
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("corrplotind"))
                                          ), #end of tabpanel correlation
                                          
                                          
                                          tabPanel(
                                            "Spiderplot",
                                            value = "boxspiderpolind",
                                            div(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotOutput("spider_polind", width = "100%",height = "600px")), align = "center"),
                                          )
                                                 
                                        ) #end of tabsetpanel
                                      ), #end of tabPanel Grafici
                                      
                                      
                                      
                                      tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")), value = "tabpolindpca",
                                        tabsetPanel(
                                          
                                          tabPanel("Plot",
                                                   br(),
                                                   fluidPage(
                                                     fluidRow(
                                                       column(3, box(width = NULL, status = "primary",
                                                                   awesomeRadio("selbiplotpolind", "Seleziona tipo di grafico", choices = c("Biplot", "Plot")))),
                                                       column(4, box(width = NULL, status = "primary", 
                                                                   selectInput("colbiplot", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Cultivar_principale", "Azienda", "Provincia", "Areale")))),
                                                       column(4, box(width = NULL, status = "primary",
                                                                   awesomeCheckboxGroup("shpbiplot", "Aggiungi geometria", choices = "Provincia", inline = TRUE)
                                                     ))),
                                                   fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("biplot", height = "500px"))))
                                          ),
                                          
                                          tabPanel("Screeplot",
                                            br(),
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("screeplot", width = "75%"))
                                          ),
                                                             
                                          tabPanel("Loadings",
                                            fluidPage(
                                              br(),
                                              fluidRow(column(4, box(width = NULL, status = "primary", uiOutput("sliderpc")))),
                                              fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("loadings")))
                                            )
                                          ),
                                          
                                          tabPanel("Plot 3D",
                                            fluidPage(
                                              br(),
                                              fluidRow(
                                                column(4, box(width = NULL, status = "primary", 
                                                              selectInput("col3dind", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Provincia", "Cultivar_principale", "Areale"))))
                                              ),
                                              fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("pca3dpolind", height = "500px")))
                                            )
                                          )
                                        ) #end of tabsetpanel
                                      ), #end of tabpanel PCA
                                      
                                      
                                      
                                      #tab galleria (ovvero i cromatogrammi)
                                      tabPanel(tagList(shiny::icon("images"), HTML("&nbsp;Galleria")), value = "tabpolindcroma",
                                        fluidPage(
                                          fluidRow(
                                            column(4, 
                                              fluidRow(
                                                column(6,
                                                  box(width = NULL, status = "primary",
                                                    radioGroupButtons(inputId = "campcromatph", label = "Numero campionamento",  choices = c("1" = "1_campionamento", "2" = "2_campionamento"),
                                                                      individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                                  )
                                                ), 
                                                column(6, box(width=NULL, status = "primary", 
                                                              selectInput("selyearcromatph", "Seleziona l'anno", choices = "", multiple = FALSE))
                                                )
                                              ), 
                                                   
                                              fluidRow(column(12, box(width=NULL, status = "primary", DT::DTOutput("prov3"))))
                                            ),
                                            
                                            
                                            
                                            column(8,
                                              
                                              column(6, offset = 3,
                                                conditionalPanel(condition = "input.prov3_rows_selected == 0",
                                                box(width = NULL, background = "yellow", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 27px"), h4(strong("Seleziona un'azienda dalla tabella"), style = "color: white"), style = "text-align: justify;  text-align: center;")
                                              ),
                                                #R1 e olio/posa
                                                conditionalPanel(condition = "input.prov3_rows_selected != 0 && output.check_crompolind == 'no'",
                                                  box(width = NULL, background = "yellow", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 27px"), h4(strong("I dati sull'olio e sulla posa riguardano solamente il secondo campionamento."), style = "color: white"), style = "text-align: justify;  text-align: center;")
                                                )
                                              ),
                                              
                                              #non R1 e olio/posa  
                                              conditionalPanel(condition = "input.prov3_rows_selected != 0 && output.check_crompolind == 'yes'",
                                                          
                                                  fluidRow(box(width=NULL, status = "primary", title = "Cromatogramma", align= "center", uiOutput("phcromat"))),
                                                  fluidRow(
                                                    box(width=NULL, status = "primary", title = "Polifenoli",align = "center", style = "overflow-x: scroll;",
                                                        DTOutput("tabcromatpolind")
                                                    )
                                                  )
                                              ) #end of conditional panel !=0
                                              
                                              
                                            ) #end of column 8
                                            
                                          ) #end of fluidRow
                                        )
                                        
                                        
                                      ), 
                                      
                                      
                                      tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")), value = "tabpolindmap",
                                        conditionalPanel(condition = "input.updatepolindmap != 0", 
                                          shinycssloaders::withSpinner(tmapOutput("polindmap1"))),
                                        conditionalPanel(condition = "input.addmappolind2 != 0", 
                                          hr(),
                                          shinycssloaders::withSpinner(tmapOutput("polindmap2")))
                                      )
                                      
                                      
                                      
                                      
                                    )
                                  )
                                )

                              ), #end of tabitem "inpolsub"
                              
                           
                              # ####### Tab polifenoli LCxLC ##########
                              tabItem(tabName = "lcpolsub",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    radioGroupButtons("selfilepollc", "Seleziona i polifenoli da analizzare",
                                      choiceValues = list("foglie", "drupe", "olio", "posa", "sansa"),
                                      choiceNames = list(
                                        paste(shiny::icon("leaf",  style='font-size:16px;'), HTML("<b style=font-size:16px>&nbsp;Foglie</b>")),
                                        paste(tags$img(src = "www/olive_icon2.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Drupe</b>")),
                                        paste(tags$img(src = "www/olive_oil.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Olio</b>")),
                                        paste(tags$img(src = "www/posa.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Posa</b>")),
                                        paste(tags$img(src = "www/sansa3.png", height = "23px", width = "23px"), HTML("<b style=font-size:16px>&nbsp;Sansa</b>"))),
                                      direction = "vertical", justified = TRUE, status = "primary"
                                    ),
                                    hr(),

                                    # Tabella
                                    conditionalPanel(condition = "input.tabboxlcxlc == 'tabdtlc'",
                                      awesomeRadio("dttypelc", "Tipo di visualizzazione", choices = c("Wide", "Long"))
                                      ),

                                    #Grafici
                                    conditionalPanel(condition = "input.tabboxlcxlc == 'tabgraphlc'",
                                      h4(strong("Impostazioni grafici")),

                                      #scatterplot e barplot
                                      conditionalPanel(condition = "input.boxlcgraph == 'tabpanscattlc' || input.boxlcgraph == 'tabpanbarlc'",
                                        awesomeRadio("lcdatatypescatt", "Filtra i dati per:", choices = c("Azienda", "Polifenolo", "Cultivar principale")),
                                        conditionalPanel(
                                          condition = "input.lcdatatypescatt == 'Azienda'",
                                          selectInput("lcselaziendascatt", "Scegli l'azienda", choices = "")
                                        ),
                                        conditionalPanel(
                                          condition = "input.lcdatatypescatt == 'Azienda' || input.lcdatatypescatt == 'Cultivar principale'",
                                          awesomeCheckbox("logscattlc", "Scala logaritmica", value = TRUE)
                                        ),
                                        conditionalPanel(
                                          condition = "input.lcdatatypescatt == 'Polifenolo'",
                                          selectInput("lcselpolifscatt", "Scegli il polifenolo", choices = "")
                                        ),
                                        conditionalPanel(condition = "input.lcdatatypescatt == 'Cultivar principale'",
                                          selectInput("lcselcultscatt", "Scegli la cultivar principale", choices = ""),
                                          awesomeCheckbox("sintscattlc", "Sintetizza i dati", value = TRUE)
                                        ),
                                        selectInput("numscattlc", "Scegli il numero di campionamento", choices = ""),
                                        selectInput("fillscattlc", "Scegli colonna riempimento", choices = "")
                                      ),

                                      conditionalPanel(condition = "input.boxlcgraph == 'tabpanbarlc' && input.lcdatatypescatt == 'Cultivar principale'",
                                      awesomeRadio("bartypelc", "Tipo di barplot", choices = c("Affiancato" = "dodge", "Impilato" = "stack")),
                                      selectInput("lcselpolbar", "Polifenoli da mostrare", choices = "")
                                      ),

                                      # HEATMAP
                                      conditionalPanel(condition = "input.boxlcgraph == 'tabpanheatlc'",

                                        div(actionButton("updateheatlc", label = "Carica!", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:150%; font-weight: bold;'), align= "center"),
                                        br(),
                                        h4(strong("Dati")),
                                        selectInput("numheatlc", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                        selectInput("cultheatlc", "Filtra per cultivar", choices = "", multiple = FALSE),
                                        #selectInput("selyearheatmorfo", "Seleziona l'anno", choices = "", multiple = FALSE),
                                        h4(strong("Preprocessing")),
                                        selectInput("selscaleheatlc", "Scala i dati:", choices = c("No" = "none", "Per riga" = "row", "Per colonna" = "column"), selected = "none"),
                                        hr(),
                                        h4(strong("Opzioni dendrogramma")),

                                        ###voglio il dendrograma su riga o colonna o entrambi?
                                        h5(strong("Scegli dove mostrare il dendrogramma")),
                                        fluidRow(
                                          column(6, materialSwitch(inputId = "rowdendlc", label = "Riga",  value = TRUE, status = "primary", width = "90%")),
                                          column(6, materialSwitch(inputId = "columndendlc", label = "Colonna",  value = TRUE, status = "primary", width = "90%"))
                                        ),
                                        selectInput("selectannotlc", "Colonna annotazione:", choices = c("Provincia", "Cultivar_principale")),
                                        conditionalPanel(condition = "input.rowdendlc == 1 || input.columndendmorfo == 1",
                                                         selectInput("seldistheatlc", "Funzione di distanza:", choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski"), selected = "euclidean"),
                                                         selectInput("selhclustheatlc", "Metodo clustering:", choices = c("ward.D", "ward.D2", "single", "complete", "average" , "mcquitty", "median", "centroid"), selected = "complete"),
                                        ),

                                        conditionalPanel(condition = "input.rowdendlc == 0",
                                                         h5(strong("Ordinare i dati per annotazione?")),
                                                         awesomeCheckbox("heatsortlc", label = "Ordina", value = TRUE)
                                        ),

                                        conditionalPanel(condition = "input.rowdendlc == 1",
                                                         hr(),
                                                         h4(strong("Dendrogramma su riga")),
                                                         sliderInput("sliderrowheatlc", "Numero cluster:", min = 2, max = 10, value = 2),
                                        ),

                                        conditionalPanel(condition = "input.columndendlc == 1",
                                                         hr(),
                                                         h4(strong("Dendrogramma su colonna")),
                                                         uiOutput("slidercolheatlc"),
                                        )
                                      )



                                    ), #end of conditionalpanel grafici

                                    # PCA
                                    conditionalPanel(condition = "input.tabboxlcxlc == 'tabpcalc'",
                                      #selectInput("selyearpca", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numcamppcalc", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                      awesomeCheckbox("scalepcalc", "Scala i dati", value = TRUE)

                                    )
                                  ), #end of sidebarpanel



                                  mainPanel(width=10,
                                    tabBox(id = "tabboxlcxlc", width=NULL,

                                      tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), value = "tabdtlc",
                                               box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                                                   DT::DTOutput("dtlcxlc")),
                                               mod_render_NAbox_ui("naboxlc")
                                      ),


                                      ### Galleria
                                      tabPanel(tagList(shiny::icon("images"), HTML("&nbsp;Galleria")), value = "tablccroma",
                                               fluidPage(
                                                 fluidRow(
                                                   column(4,
                                                          fluidRow(
                                                            column(6,
                                                                   box(width = NULL, status = "primary",
                                                                     radioGroupButtons("ncampcromatlc", "Numero campionamento",  choices = c("1" = "1_campionamento", "2" = "2_campionamento"),
                                                                                       individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                                                   )
                                                            )
                                                          ),

                                                          fluidRow(column(12, box(width=NULL, status = "primary", style = "overflow-x: scroll;", DT::DTOutput("dtfotolc"))))
                                                   ),
                                                   conditionalPanel(condition = "input.dtfotolc_rows_selected == 0",
                                                                    p(strong(h4("Per favore seleziona un'azienda dalla tabella", align = "center")))
                                                   ),

                                                   conditionalPanel(condition = "input.dtfotolc_rows_selected != 0",
                                                                    column(8,
                                                                      fluidRow(
                                                                        box(width=NULL, status = "primary", title = "Cromatogramma", align= "center", uiOutput("phcromatlc"))
                                                                      ),
                                                                      fluidRow(
                                                                        column(10, offset = 1,
                                                                          box(width=NULL, status = "primary",title = "Polifenoli",align = "center",
                                                                            DTOutput("poliffotolc"))
                                                                               )
                                                                      )
                                                                    )
                                                   )
                                                 ) #end of fluidRow
                                               )
                                      ), #end of tabpanel Galleria


                                      ##### Grafici
                                      tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")), value = "tabgraphlc",
                                        tabsetPanel(id = "boxlcgraph",

                                          tabPanel("Scatter plot", value = "tabpanscattlc",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("scatterlc", height = "550px"))
                                          ),

                                          #barplot
                                          tabPanel("Barplot", value = "tabpanbarlc",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("barplotlc", height = "550px"))
                                          ),

                                          tabPanel("Heatmap", value = "tabpanheatlc",
                                            br(),
                                            InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_lc_output", layout = "1|(2-3)", width1 = 1150, height1 = 700)
                                          )
                                        )

                                        ), # end of tabpanel grafici


                                      ###### PCA
                                      tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")), value = "tabpcalc",
                                               tabsetPanel(

                                                 tabPanel("Plot",
                                                          br(),
                                                          fluidPage(
                                                          fluidRow(
                                                            column(3,box(width = NULL, status = "primary",
                                                                    awesomeRadio("selbiplotlc", "Seleziona tipo di grafico", choices = c("Biplot", "Plot"))
                                                              )),
                                                            column(4, box(width = NULL, status = "primary",
                                                                          selectInput("colbiplotlc", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Cultivar_principale", "Areale", "Provincia")))),
                                                            column(4, box(width = NULL, status = "primary",
                                                                          awesomeCheckboxGroup("shpbiplotlc", "Aggiungi geometria", choices = "Provincia", inline = TRUE)
                                                            ))
                                                          ),
                                                          fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("biplotlc", height = "500px"))))
                                                 ),

                                                 tabPanel("Screeplot",
                                                          br(),
                                                          shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("screeplotlc", width = "75%"))
                                                 ),


                                                 tabPanel("Loadings",
                                                          br(),
                                                          fluidPage(
                                                          fluidRow(column(4, box(width = NULL, status = "primary", uiOutput("sliderpclc")))),
                                                          fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("loadingslc", height = "550px"))))
                                                 ),



                                                 tabPanel("Plot 3D",
                                                          br(),
                                                          fluidPage(
                                                          fluidRow(
                                                            column(4, box(width = NULL, status = "primary",
                                                                          selectInput("col3dlc", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Provincia", "Cultivar_principale", "Areale"))))
                                                          ),
                                                          fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("pca3dlc", height = "500px"))))
                                                 )

                                               ) #end of tabsetpanel
                                      ) #end of tabpanel PCA



                                    ) #end of tabbox
                                  ) #end of mainpanel

                                )
                              ), #end of tabitem "lcpolsub"


                              ##### Tab morfometria #####
                              tabItem(tabName = "anmorfosub",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    radioGroupButtons("selfilemorfo", "Seleziona la morfometria da analizzare", 
                                      choiceValues = list("foglie", "drupe", "endocarpo", "rapporti"),
                                      choiceNames = list(
                                        paste(shiny::icon("leaf",  style='font-size:16px;'), HTML("<b style=font-size:16px>&nbsp;Foglie</b>")),
                                        paste(tags$img(src = "www/olive_icon2.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Drupe</b>")),
                                        paste(tags$img(src = "www/seed_icon2.png", height = "20px", width = "20px"), HTML("<b style=font-size:16px>&nbsp;Endocarpo</b>")),
                                        paste(tags$img(src = "www/ratio_icon2.png", height = "19px", width = "19px"), HTML("<b style=font-size:16px>&nbsp;Rapporti</b>"))),
                                      direction = "vertical", justified = TRUE, status = "primary"
                                      ),
                                    hr(),
                                    
                                    # Tabella
                                    conditionalPanel(condition = "input.tabboxmorfo == 'tabdtmor'",
                                      h4(strong("Parametri sintesi")),
                                      selectInput("selyeardtmorfo", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      numericInput("selroundmorfo", "Numero di digits", value = 3),
                                      hr(),
                                      materialSwitch(inputId = "summarizetab", label = "Sintetizza i dati", value = TRUE, status = "primary"),
                                      conditionalPanel(condition = "input.summarizetab == true",
                                        awesomeCheckboxGroup("selectdtmorfo", "Seleziona la colonna da usare per la sintesi", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale", "N_campionamento"), selected = "Codice_azienda")
                                      )
                                    ),
                                    

                                    # Grafici
                                    conditionalPanel(condition = "input.tabboxmorfo  == 'tabpanmorfograph'",
                                    h4(strong("Impostazioni grafici")),
                                    #boxplot e barplot
                                    conditionalPanel(
                                      condition = "input.boxmorfograph == 'tabpanboxmorfo' || input.boxmorfograph == 'tabpanbarmorfo'",
                                      selectInput("selectxmorfobb", "Seleziona la colonna X", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE),
                                      selectInput("selectymorfobb", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                      selectInput("selectfillmorfobb", "Colonna da usare come riempimento", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE)

                                    ),
                                    
                                    #scatterplot
                                    conditionalPanel(
                                      condition = "input.boxmorfograph == 'tabpanscattmorfo'",
                                      materialSwitch(inputId = "summarizescatt", label = "Sintetizza i dati", value = TRUE, status = "primary"),
                                      conditionalPanel(condition = "input.summarizescatt == true",
                                                       selectInput("selectsummscatt", "Seleziona la colonna da usare per la sintesi", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = TRUE),
                                                       hr(),
                                      ),
                                      selectInput("selectxmorfoscatt", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                      selectInput("selectymorfoscatt", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                      selectInput("selectfillmorfoscatt", "Colonna da usare come riempimento", choices = "", multiple = FALSE)
                                     ),
                                    
                                    #IOC
                                    conditionalPanel(
                                      condition = "input.boxmorfograph == 'tabpaniocmorfo'",
                                      radioButtons("selplotioc", label = h4("Tipo di grafico"), choices = list("Grafico a torta" = 1, "Grafico a barre" = 2), selected = 2),
                                      selectInput("selectfillmorfoioc", "Misura IOC", choices = "", multiple = FALSE),
                                      conditionalPanel(condition = "input.selplotioc == 2",
                                        selectInput("selectxmorfoioc", "Seleziona la colonna X", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE),
                                        awesomeRadio("iocselfreq", "Frequenza", choices = c("Frequenza assoluta", "Frequenza relativa"))
                                      ),
                                      
                                      div(actionButton("addiocmorfo2", label = "Aggiungi secondo grafico"), align = "center"),
                                      conditionalPanel(condition = "input.addiocmorfo2 != 0",
                                        br(),
                                        radioButtons("selplotioc2", label = h4("Tipo di grafico"), choices = list("Grafico a torta" = 1, "Grafico a barre" = 2), selected = 2),
                                        selectInput("selectfillmorfoioc2", "Misura IOC", choices = "", multiple = FALSE),
                                        conditionalPanel(condition = "input.selplotioc2 == 2",
                                          selectInput("selectxmorfoioc2", "Seleziona la colonna X", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE),
                                          awesomeRadio("iocselfreq2", "Frequenza", choices = c("Frequenza assoluta", "Frequenza relativa"))
                                        )
                                      )
                                    ), 
                                    
                                    # HEATMAP
                                    conditionalPanel(
                                      condition = "input.boxmorfograph == 'tabpanheatmorfo'",
                                      
                                      div(actionButton("updateheatmorfo", label = "Carica!", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:150%; font-weight: bold;'), align= "center"),
                                      br(),
                                      h4(strong("Dati")),
                                      selectInput("numheatmorfo", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                      selectInput("selyearheatmorfo", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      hr(),
                                      h4(strong("Opzioni dendrogramma")),
                                      
                                      ###voglio il dendrograma su riga o colonna o entrambi?
                                      h5(strong("Scegli dove mostrare il dendrogramma")),
                                      fluidRow(
                                        column(6, materialSwitch(inputId = "rowdendmorfo", label = "Riga",  value = TRUE, status = "primary", width = "90%")),
                                        column(6, materialSwitch(inputId = "columndendmorfo", label = "Colonna",  value = TRUE, status = "primary", width = "90%"))
                                      ),
                                      selectInput("selectannotmorfo", "Colonna annotazione:", choices = c("Provincia", "Cultivar_principale")),
                                      conditionalPanel(condition = "input.rowdendmorfo == 1 || input.columndendmorfo == 1",
                                        selectInput("seldistheatmorfo", "Funzione di distanza:", choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski"), selected = "euclidean"),
                                        selectInput("selhclustheatmorfo", "Metodo clustering:", choices = c("ward.D", "ward.D2", "single", "complete", "average" , "mcquitty", "median", "centroid"), selected = "complete"),
                                      ),
                                      
                                      conditionalPanel(condition = "input.rowdendmorfo == 0",
                                                       h5(strong("Ordinare i dati per annotazione?")),
                                                       awesomeCheckbox("heatsortmorfo", label = "Ordina", value = TRUE)
                                      ),
                                      
                                      conditionalPanel(condition = "input.rowdendmorfo == 1",
                                                       hr(),
                                                       h4(strong("Dendrogramma su riga")),
                                                       sliderInput("sliderrowheatmorfo", "Numero cluster:", min = 2, max = 10, value = 2),
                                      ),
                                      
                                      conditionalPanel(condition = "input.columndendmorfo == 1",
                                                       hr(),
                                                       h4(strong("Dendrogramma su colonna")),
                                                       uiOutput("sliderheatcolmorfo"),
                                      )
                                     ),
                                    
                                    conditionalPanel(condition = "input.boxmorfograph == 'tabpancorrmorfo'",
                                      selectInput("selyearcorrmorfo", "Seleziona l'anno", choices = "", multiple = FALSE)
                                      )
                                    

                                    ), #end of conditionapanel dei grafici
                                    
                                    
                                    # PCA
                                    conditionalPanel(
                                    condition = "input.tabboxmorfo == 'tabpcamor'",
                                    selectInput("selyearpcamorfo", "Seleziona l'anno", choices = "", multiple = FALSE),
                                    selectInput("numpcamorfo", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                    materialSwitch(inputId = "summarizepcamorfo", label = "Sintetizza i dati", value = TRUE, status = "primary"),
                                    radioGroupButtons(inputId = "selcorpcamorfo", label = "Matrice:", choices = c("Correlazione" = TRUE, "Covarianza" = FALSE),
                                                      individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))

                                    ),

                                    #  Clustering
                                    conditionalPanel(
                                      condition = "input.tabboxmorfo == 'tabclustmor'",
                                      selectInput("selyearclustmorfo", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numclustmorfo", "Scegli il numero di campionamento", choices = "", multiple = FALSE),
                                      #materialSwitch(inputId = "summarizeclustmorfo", label = "Sintetizza i dati", status = "primary", value = TRUE),
                                      radioGroupButtons("selclustmethod", "Tipo di clustering", choices = c("Gerarchico", "Partizionale"), justified = TRUE, status = "primary"),
                                      conditionalPanel(
                                        condition = "input.selclustmethod == 'Partizionale'",
                                        selectInput("selclusthmorfo", "Seleziona l'algoritmo di clustering", choices = c("K-means", "PAM", "Clara"), selected = "K-means", multiple = FALSE),
                                      ) 
                                    ),
                                    
                                    
                                    

                                    #Test d'ipotesi
                                    conditionalPanel(condition = "input.tabboxmorfo == 'tabpanmorfotest'",
                                      h4(strong("Opzioni test")),
                                      selectInput("selyearttestmorfo", "Scegli l'anno", choices = "", multiple = FALSE),
                                      hr(),
                                      
                                      #T-test
                                      conditionalPanel(condition = "input.boxmorfotest == 'tabpanttestmorfo'",
                                       selectInput("catvarttest", "Variabile categorica", choices = "", multiple = FALSE),
                                      selectInput("culttest1", "Variabile dipendente", choices = "", multiple = FALSE),
                                      selectInput("culttest2", "Fattore esplicativo", choices = "", multiple = FALSE),
                                      selectInput("numvarttest", "Variabile numerica da confrontare", choices = "", multiple = FALSE),
                                      awesomeRadio("selectttest", "Tipo di test", choices = c("T-test", "Wilcoxon-Mann-Whitney")),
                                      conditionalPanel(condition = "input.selectttest == 'T-test'",
                                        awesomeCheckbox("selvarequal", "Omoschedasticità", value = TRUE)
                                      ) 
                                      ),
                                      
                                      #Test correlazione
                                      conditionalPanel(
                                        condition = "input.boxmorfotest == 'tabpancorrtestmorfo'",
                                        selectInput("corrtest1", "Variabile dipendente", choices = "", multiple = FALSE),
                                        selectInput("corrtest2", "Fattore esplicativo", choices = "", multiple = FALSE),
                                        awesomeRadio("selectcorrtest", "Tipo di test", choices = c("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman")),
                                        hr(),
                                        selectInput("corrtestfill", "Colonna riempimento", choices = "", multiple = FALSE),
                                        awesomeCheckbox("numfitcorrtest", "Singolo fit", value = TRUE),
                                        awesomeCheckbox("selsecorrtest", "Intervallo di confidenza", value = TRUE),
                                        
                                      ),
                                      
                                      #Test indipendenza
                                      conditionalPanel(
                                        condition = "input.boxmorfotest == 'tabpanchisqtestmorfo'",
                                        selectInput("chisqtest1", "Variabile descrittiva", choices = c("Codice_azienda", "Provincia", "Cultivar_principale"), multiple = FALSE, selected = "Cultivar_principale"),
                                        selectInput("chisqtest2", "Variabile categorica", choices = "", multiple = FALSE),
                                        awesomeRadio("selectchisqtest", "Tipo di test", choices = c("Test esatto di Fisher", "Test d'indipendenza Chi-quadro")),
                                        awesomeCheckbox("simulatechisq", "Simulazione p-value (Monte Carlo)", value = TRUE),
                                        awesomeRadio("pvalchisqmorfo", "Livello di significatività", choices = c(0.01, 0.05, 0.1), selected = 0.05),
                                        selectInput("chisqtestfilt", "Filtrare i dati (opzionale)", choices = "", multiple = TRUE)
                                        
                                        
                                      ),
                                      
                                      #Test Anova
                                      conditionalPanel(condition = "input.boxmorfotest == 'tabpananovamorfo'",
                                        awesomeRadio("selectanovatest", "Tipo di test", choices = c("One-way ANOVA", "Two-way ANOVA")),

                                        conditionalPanel(condition = "input.selectanovatest == 'One-way ANOVA'",
                                          awesomeRadio("selectanovatest2", "Tipo di test", choices = c("ANOVA", "Kruskal-Wallis")),
                                        ),
                                        awesomeRadio("pvalanovamorfo", "Livello di significatività", choices = c(0.01, 0.05, 0.1), selected = 0.05),
                                        selectInput("anovanum", "Variabile numerica", choices = "", multiple = FALSE),
                                        selectInput("anovacat", "Variabile categorica", choices = "", multiple = FALSE),
                                        conditionalPanel(condition = "input.selectanovatest == 'Two-way ANOVA'",
                                          selectInput("anovacat2", "Seconda variabile categorica", choices = "", multiple = FALSE),
                                          awesomeRadio("anova2typemorfo", "Tipo di ANOVA", choices = c("Modello additivo", "Modello con interazione"))
                                        )
                                      ),
                                      
                                    ),

                                    # Mappa
                                    conditionalPanel(
                                      condition = "input.tabboxmorfo == 'tabmapmor'",
                                      div(actionButton("upmapmorfo", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                      conditionalPanel(
                                        condition = ("input.upmapmorfo != 0"),
                                        br(),
                                        selectInput("mapxmorfomap1", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                        selectInput("selyearmorfomap1", "Seleziona l'anno", choices = "", multiple = FALSE),  
                                        selectInput("nummorfomap1", "Scegli il numero di campionamento", choices = "", multiple = FALSE), 
                                        
                                        div(actionButton("addmapmorfo2", label = "Aggiungi seconda mappa"), align = "center"),
                                        conditionalPanel(
                                          condition = ("input.addmapmorfo2 != 0"),
                                          br(),
                                          selectInput("mapxmorfomap2", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                          selectInput("selyearmorfomap2", "Seleziona l'anno", choices = "", multiple = FALSE),  
                                          selectInput("nummorfomap2", "Scegli il numero di campionamento", choices = "", multiple = FALSE)
                                        )
                                      )
                                    )
                                    
                                  ), #end of sidebarpanel
                                  
                                  
                                  #### mainpanel ####
                                  mainPanel(width = 10,
                                    tabBox(id = "tabboxmorfo", width=NULL,
                                      tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), value = "tabdtmor",
                                               fluidPage(
                                                 fluidRow(box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                                   DT::DTOutput("dtmorfo"))),
                   
                                               mod_render_NAbox_ui("naboxmorfo")
                                               )
                                               ),
                                      
                                      tabPanel(tagList(shiny::icon("images"), HTML("&nbsp;Galleria")), value = "tabmorfomor",
                                        fluidPage(
                                          fluidRow(
                                            column(width=4, 
                                              fluidRow(
                                                column(6, box(width = NULL, status = "primary",
                                                  radioGroupButtons( 
                                                    inputId = "campfotomorfo", label = "Numero campionamento", 
                                                    choices = c("1" = "1_campionamento", "2" = "2_campionamento"), individual = TRUE, 
                                                    checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), 
                                                                     no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))
                                                  ))
                                                ),
                                                column(6, box(width=NULL, status = "primary", 
                                                  selectInput("selyearfotomorfo", "Seleziona l'anno", choices = "", multiple = FALSE))
                                                )
                                              ),
                                              
                                              fluidRow(column(12, box(width=NULL, status = "primary", style = "overflow-x: scroll;", DT::DTOutput("dtfotomorfo"))))
                                            ),
                                            
                                            column(width = 8,
                                                   
                                              column(width = 6, offset = 3,
                                                conditionalPanel(condition = "input.dtfotomorfo_rows_selected == 0",
                                                  box(width = NULL, background = "yellow", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 27px"), h4(strong("Seleziona un'azienda dalla tabella"), style = "color: white"), style = "text-align: justify;  text-align: center;"),
                                                ),
                                                
                                                conditionalPanel(condition = "input.dtfotomorfo_rows_selected != 0 && input.campfotomorfo == '1_campionamento'",
                                                  box(width = NULL, background = "yellow", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 27px"), h4(strong("Nessuna foto in archivio. I dati sulla morfometria riguardano solamente il secondo campionamento."), style = "color: white"), style = "text-align: justify;  text-align: center;"),
                                                )
                                              ),
                                              
                                              conditionalPanel(
                                                condition = "input.dtfotomorfo_rows_selected != 0 && input.campfotomorfo == '2_campionamento'",
                                                uiOutput("phmorfo")
                                              )
                                            )
                                          ) #end of fluidRow
                                        )
                                      ), #end of tabpanel Foto

                                      
                                      tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")), value = "tabpanmorfograph",
                                               
                                        tabsetPanel(id = "boxmorfograph",
                                          tabPanel("Boxplot", value = "tabpanboxmorfo",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("boxmorfo"))
                                               ),
                                      
                                          tabPanel("Barplot", value = "tabpanbarmorfo",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("barmorfo"))
                                               ),
                                          
                                          tabPanel("Scatter plot", value = "tabpanscattmorfo",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("scattmorfo"))
                                                   ),
                                          
                                          tabPanel("IOC", value = "tabpaniocmorfo",
                                                   shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("iocmorfo")),
                                                   
                                                   conditionalPanel(condition = "input.addiocmorfo2 != 0",
                                                     hr(),
                                                     shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("iocmorfo2")),
                                                   )
                                          ), 
                                          
                                          tabPanel("Heatmap", value = "tabpanheatmorfo",
                                            br(),
                                            InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_morfo_output", layout = "1|(2-3)", width1 = 850, height1 = 600)
                                          ),
                                          tabPanel("Correlation plot", value = "tabpancorrmorfo",
                                            br(),
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("corrplotmorfo"))
                                          )
                                        )
                                      ),
                                      
                                      #pca
                                      tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")), value = "tabpcamor",
                                            tabsetPanel(
                                              
                                              tabPanel("Plot",
                                                       br(),
                                                       fluidPage(
                                                         fluidRow(
                                                           column(3, box(width = NULL, status = "primary",
                                                                         awesomeRadio("selbiplotmorfo", "Seleziona tipo di grafico", choices = c("Biplot", "Plot")))),
                                                           column(3, box(width = NULL, status = "primary",
                                                                         selectInput("colbiplotmorfo", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Provincia", "Cultivar_principale", "Areale")))),
                                                           column(3, box(width = NULL, status = "primary",
                                                                         awesomeCheckboxGroup(inputId = "shpbiplotmorfo", label = "Aggiungi geometria", choices = "Provincia", inline = TRUE)
                                                           ))
                                                           
                                                         ),
                                                         fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("biplotmorfo", height = "500px"))))
                                              ),

                                              tabPanel("Screeplot",
                                                br(),
                                                shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("screeplotmorfo", width = "75%"))
                                              ),

                                              tabPanel("Loadings",
                                                br(),
                                                fluidPage(
                                                  fluidRow(column(4, box(width = NULL, status = "primary",
                                                                       uiOutput("sliderpcmorfo")))),
                                                  fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("loadingsmorfo"))))
                                                ),


                                              tabPanel("Plot 3D",
                                                       br(),
                                                       fluidPage(
                                                         fluidRow(
                                                           column(4, box(width = NULL, status = "primary",
                                                                       selectInput("col3dmorfo", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Provincia", "Cultivar_principale", "Areale"))))
                                                         ), 
                                                         fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("pca3dmorfo", height = "500px"))))
                                              )
                                            )
                                            
                                      ), #end of tabpanel PCA

                                      
                                      # Clustering
                                      tabPanel(tagList(tags$img(src = "www/clustering_icon.png", height = "16px", width = "16px"), HTML("&nbsp;Clustering")), value = "tabclustmor",
                                        tabsetPanel(
                                          tabPanel("Numero cluster",
                                            br(),
                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotOutput("numclustergraph", height = "800px"))
                                          ),
                                                  
                                          tabPanel("Cluster plot",
                                            br(),
                                            fluidPage(
                                              fluidRow(
                                                column(4, 
                                                  box(width = NULL, status = "primary",
                                                    sliderInput("selnumclustmorfo", "Numero cluster:", min = 1, max = 10, value = 2))),
                                                
                                                column(4, 
                                                  conditionalPanel(condition = "input.selclustmethod == 'Gerarchico'",
                                                    box(width=NULL, status = "primary",
                                                      selectInput("selhclustmeth", "Seleziona metodo agglomerazione", choices = c("single", "complete", "ward.D", "ward.D2"), selected = "ward.D2"))
                                                  )
                                                )
                                              ), 
                                              fluidRow(shinycssloaders::withSpinner(image = "www/running_olive.gif", plotOutput("plotclustermorfo", height = "600px")))
                                            )
                                          )
                                          
                                        )
                                      ), #end of tabpanel clustering

                                      
                                      
                                      #tabpanel test d'ipotesi
                                      tabPanel(tagList(shiny::icon("clipboard-check"), HTML("&nbsp;Test d'ipotesi")), value = "tabpanmorfotest",
                                          tabsetPanel(id = "boxmorfotest",
                                            
                                            # Test di correlazione
                                            tabPanel("Test di correlazione", value = "tabpancorrtestmorfo",
                                                     br(), 
                                                     fluidPage(
                                                       fluidRow(
                                                         column(6, box(width = NULL, title = strong("Test sulla normalità"), status = "primary", verbatimTextOutput("shapirocorr1"))),
                                                         column(6, box(width = NULL, title = strong("Test sulla normalità"), status = "primary", verbatimTextOutput("shapirocorr2")))
                                                       ),
                                                       
                                                       fluidRow(
                                                         column(6,
                                                                conditionalPanel(condition = "input.corrtest1 != input.corrtest2",
                                                                                 box(width = NULL, title = strong("Test statistico"), status = "primary", verbatimTextOutput("corrtest"))),
                                                                conditionalPanel(condition = "input.corrtest1 == input.corrtest2",
                                                                                 box(width = NULL, title = strong("Test statistico"), status = "warning", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 20px; color: rgb(243,156,18)"), 
                                                                                     h5(strong("Errore. La variabile dipendente e il fattore esplicativo devono essere diversi.", style = "color: rgb(243,156,18)")), style = "text-align: justify;  text-align: center;"))
                                                         ),
                                                         column(6, box(width = NULL, title = strong("Grafico"), status = "primary", 
                                                                       shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("scattcorrtest"))))
                                                       )
                                                     ),
                                            ), #end of tabpanel
                                            
                                            
                                            tabPanel("Confronto tra due gruppi", value = "tabpanttestmorfo",
                                              br(),
                                              fluidPage(
                                                fluidRow(
                                                  column(6, box(width = NULL, title = strong("Test sulla normalità"), status = "primary", verbatimTextOutput("shapiro1"))),
                                                  column(6, box(width = NULL, title = strong("Test sulla normalità"), status = "primary", verbatimTextOutput("shapiro2")))
                                                ),
                                                fluidRow(
                                                  conditionalPanel(condition = "input.culttest1 != input.culttest2 && (output.shapttestmorfoui == 'distribuzione normale' || input.selectttest == 'Wilcoxon-Mann-Whitney')",
                                                    column(6,box(width = NULL, title = strong("Test sulla varianza"), status = "primary", verbatimTextOutput("vartest1"))),
                                                    column(6,box(width = NULL, title = strong("Test statistico"), status = "primary", verbatimTextOutput("ttest1")))
                                                  ),
                                                conditionalPanel(condition = "input.culttest1 == input.culttest2",
                                                  column(6, box(width = NULL, title = strong("Test sulla varianza"), status = "warning", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 20px; color: rgb(243,156,18)"), 
                                                                h5(strong("Errore. La variabile dipendente e il fattore esplicativo devono essere diversi.", style = "color: rgb(243,156,18)")), style = "text-align: justify;  text-align: center;")),
                                                  column(6, box(width = NULL, title = strong("Test statistico"), status = "warning", tags$i(class = "fas fa-exclamation-triangle", style="font-size: 20px; color: rgb(243,156,18)"), 
                                                                h5(strong("Errore. La variabile dipendente e il fattore esplicativo devono essere diversi.", style = "color: rgb(243,156,18)")), style = "text-align: justify;  text-align: center;"))
                                                  ),
                                                conditionalPanel(
                                                  condition = "input.culttest1 != input.culttest2 && output.shapttestmorfoui == 'distribuzione non normale' && input.selectttest == 'T-test'",
                                                  column(6, offset = 3,
                                                    box(width = NULL, status = "warning", style = "text-align: justify;  text-align: center;", br(),
                                                    tags$i(class = "fas fa-exclamation-triangle", style="font-size: 25px; color: rgb(243,156,18)"), 
                                                    h4(strong("Una o più variabili non seguono una distribuzione normale. Scegli il Wilcoxon-Mann-Whitney test.", style = "color: rgb(243,156,18)")),
                                                    br()
                                                    )
                                                  )
                                                )
                                                ), 
                                                
                                                fluidRow(
                                                  column(8, offset = 2, box(width = NULL, title = strong("Grafico"), status = "primary", 
                                                                            shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("boxttest"))))
                                                )
                                              ) #end of fluidpage
                                          ), #end of tabpanel conf 2 gruppi
                                          
                                          
                                          
                                          # Confronto tra più gruppi
                                          tabPanel("Confronto tra più gruppi", value = "tabpananovamorfo",
                                            fluidPage(
                                              fluidRow(
                                                column(12, box(width = NULL, title = strong("Boxplot"), status = "primary", 
                                                               shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("boxanova"))))
                                              ), 
                                                     
                                              fluidRow(
                                                column(6, box(width = NULL, title = strong("Test sulla normalità"), status = "primary", verbatimTextOutput("shapiroanova1"))),
                                                       
                                                conditionalPanel(condition = "(output.shapanovamorfoui == 'distribuzione normale' && (input.selectanovatest2 == 'ANOVA' || input.selectanovatest2 == 'Kruskal-Wallis')) || (output.shapanovamorfoui == 'distribuzione non normale' && input.selectanovatest2 == 'Kruskal-Wallis')",
                                                         
                                                  conditionalPanel(condition = "input.selectanovatest2 == 'ANOVA'",
                                                    column(6, box(width = NULL, title = strong("Test ANOVA"), status = "primary", verbatimTextOutput("anova1morfoprint")))
                                                  ), 
                                                  
                                                  conditionalPanel(condition = "input.selectanovatest2 == 'Kruskal-Wallis'",
                                                    column(6, box(width = NULL, title = strong("Test Kruskal-Wallis"), status = "primary", verbatimTextOutput("kruskmorfo")))
                                                  )
                                                ), 
                                                       
                                                conditionalPanel(condition = "output.shapanovamorfoui == 'distribuzione non normale' && input.selectanovatest2 == 'ANOVA'",
                                                  column(6, box(width = NULL, title = strong("Test ANOVA"), status = "warning", style = "text-align: justify;  text-align: center;", 
                                                           tags$i(class = "fas fa-exclamation-triangle", style="font-size: 25px; color: rgb(243,156,18)"), 
                                                           h4(strong("La variabile numerica non segue la distribuzione normale. Scegli il test Kruskal-Wallis.", style = "color: rgb(243,156,18)")),
                                                  ))
                                                )
                                              ), 
                                                     
                                                     
                                                     conditionalPanel(condition = "(output.shapanovamorfoui == 'distribuzione normale' && (input.selectanovatest2 == 'ANOVA' || input.selectanovatest2 == 'Kruskal-Wallis')) || (output.shapanovamorfoui == 'distribuzione non normale' && input.selectanovatest2 == 'Kruskal-Wallis')",
                                                       fluidRow(
                                                         column(10,offset = 1,
                                                           box(width = NULL, title = strong("Post-hoc"), status = "primary", style = "text-align: justify;  text-align: center;",
                                                               conditionalPanel(condition = "output.signiftestmorfoui == 'significativo'",

                                                                 conditionalPanel(condition = "input.selectanovatest2 == 'ANOVA'",
                                                                   h4(strong("Tukey HSD"))), 
                                                                 
                                                                 conditionalPanel(condition = "input.selectanovatest2 == 'Kruskal-Wallis'",
                                                                   h4(strong("Dunn Test")),
                                                                 ),  
                                                                 shinycssloaders::withSpinner(image = "www/running_olive.gif", plotly::plotlyOutput("posthocmorfograph"))
                                                               ), 
                                                               
                                                               conditionalPanel(condition = "output.signiftestmorfoui == 'non significativo'",
                                                                 tags$i(class = "fas fa-exclamation-triangle", style="font-size: 25px; color: rgb(243,156,18)"), 
                                                                 h4(strong("Il p-value è maggiore del livello di significatività impostato. Non ci sono differenze significative tra le variabili.", style = "color: rgb(243,156,18)"))
                                                               )
                                                           )
                                                         )
                                                       ) #end of fluidrow post-hoc
                                                     )
                                              
                                                   ) #end of fluidpage
                                          ), #end of tabpanel più gruppi
                                          

                                          
                                          
                                          #Test d'indipendenza
                                          tabPanel("Test d'indipendenza", value = "tabpanchisqtestmorfo",
                                            br(),
                                            fluidPage(
                                              fluidRow(
                                                column(5, box(width = NULL, title = strong("Tabella di contingenza"), status = "primary", tableOutput("contingtablemorfo"))),
                                                column(6,
                                                  fluidRow(
                                                    box(width = NULL, title = strong("Test"), status = "primary", verbatimTextOutput("chisqmorfoprint"))
                                                  ),
                                            
                                                  fluidRow(
                                                    conditionalPanel(condition = "output.signiftestchisqmorfoui == 'significativo'",
                                                    #box(width = NULL, title = strong("Grafico residui"), status = "primary", style = "text-align: justify;  text-align: center;",
                                                    shinycssloaders::withSpinner(image = "www/running_olive.gif", plotOutput("plotresidchisq"))
                                                     #   )
                                                        
                                                    ),
                                                    conditionalPanel(condition = "output.signiftestchisqmorfoui == 'non significativo'",
                                                      box(width = NULL, status = "warning", style = "text-align: justify;  text-align: center;",
                                                        tags$i(class = "fas fa-exclamation-triangle", style="font-size: 25px; color: rgb(243,156,18)"), 
                                                        h4(strong("Il p-value è maggiore del livello di significatività impostato. Le variabili sono indipendenti.", style = "color: rgb(243,156,18)"))
                                                      )
                                                    )
                                                  ) #end of fluidrow grafico
                                                )
                                              ) #end of first fluidrow
                                            )
                                          )  
                                      ) #end of tabset panel
                                    ),
                                      
                                      
                                      #tabpanel mappa
                                      tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")), value = "tabmapmor",
                                              conditionalPanel(condition = ("input.upmapmorfo != 0"),
                                                shinycssloaders::withSpinner(tmapOutput("mapmorfo1")),
                                              conditionalPanel(condition = "input.addmapmorfo2 != 0",
                                                hr(),
                                                shinycssloaders::withSpinner(tmapOutput("mapmorfo2"))
                                              )
                                              ))
                                    ) #end of tabBox
                                    
                                  ) #end of mainpanel
                                ) #end of sidebarlayout
                                
                              ), #end of tabitem morfo
                              
                              ##### Tab integrazione dati ####
                              tabItem(tabName = "integrdati",
                                tabBox(width = 12,
                                  
                                  tabPanel("Dati meteo",
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 2,
                                        fileInput("file_ncdf", "File meteo (.nc)", accept = ".nc"),
                                        selectInput("varmeteo", "Variabile meteo", choices = ""),
                                        conditionalPanel(
                                          condition = "input.tabsetmeteo == 'tabpanmeteomap'",
                                          awesomeRadio("type_mapmeteo", "Tipo di mappa", choices = c("Statica","Animata")),
                                          selectInput("selyearmeteo", "Seleziona anno", choices = "")
                                        ),
                                        conditionalPanel(
                                          condition = "input.tabsetmeteo == 'tabpanplotomap'",
                                          awesomeRadio("meteoplot_tipoconf", "Tipo di confronto", choices = c("Tra anni", "Tra aziende"))
                                        )

                                      ),
                                      mainPanel(
                                        width = 10,
                                        tabsetPanel(id = "tabsetmeteo",
                                          tabPanel("Mappa", value = "tabpanmeteomap",
                                            conditionalPanel(condition = "input.type_mapmeteo == 'Animata'",
                                              shinycssloaders::withSpinner(imageOutput("mapmeteo"), image = "www/running_olive.gif")
                                            ),
                                            conditionalPanel(
                                              condition = "input.type_mapmeteo == 'Statica'",
                                              shinycssloaders::withSpinner(tmapOutput("mapmeteo2"))
                                            )
                                          ),
                                          
                                          tabPanel("Plot", value = "tabpanplotomap",
                                            box(width=NULL, status = "primary",
                                                fluidRow(
                                                  conditionalPanel(
                                                    condition = "input.meteoplot_tipoconf == 'Tra aziende'",
                                                    column(2, awesomeRadio("type_lineplot", "Tipo di grafico", choices = c("Statico","Animato"))),
                                                    column(3, selectInput("selyearplotmeteo", "Seleziona anno", choices = "Tutti")),
                                                    column(3, selectInput("selcod_plotmeteo", "Scegli una o più aziende", choices = "", multiple = TRUE))
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.meteoplot_tipoconf == 'Tra anni'",
                                                    column(3, selectInput("selcod_plotmeteo2", "Scegli un'azienda", choices = ""))
                                                  )
                                                  
                                                )),
                                            conditionalPanel(
                                              condition = "input.type_lineplot == 'Statico'",
                                              shinycssloaders::withSpinner(plotly::plotlyOutput("ggline_meteo"), image = "www/running_olive.gif")
                                            ),
                                            conditionalPanel(
                                              condition = "input.type_lineplot == 'Animato' && input.meteoplot_tipoconf == 'Tra aziende'",
                                              shinycssloaders::withSpinner(imageOutput("plotline_animated"), image = "www/running_olive.gif")
                                            )
                                          )
                                        )
                                        
                                        
                                      )
                                    )
                                  )
                                )
                              )
                              
                            )#end of tabitems
                          )#end of dashboardbody
                        )#end of dashboardpage
                        
               )
    )
  ) #end of taglist
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'OliveHealthR'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

