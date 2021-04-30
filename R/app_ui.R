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
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage(title = "OliveHealthR", theme = shinytheme("spacelab"), id = "navb1",
               tabPanel(title = "Welcome", value = "panel1",
                        tags$img(src = "www/Photo_2.jpg", width = "100%"),
                        h2(strong("Cos'è OliveHealthR?"), style = "text-align: center"),
                        br(), br(),
                        fluidRow(
                          column(1, offset=1,tags$img(src = "www/Olivehealt_Logo.png", width = "200", height = "200")),
                          column(width = 6, offset = 1,
                                 p(h4("OliveHealthR è un software che svolge le analisi sui dati
                 provenienti dal progetto Olive Health. L'obiettivo principale del progetto è quello di identificare le
                 componenti salutistiche (es polifenoli) in prodotti della filiera olivicola (quali foglie, drupe e
                 olio) correlandole alla geo-localizzazione di ciascun appezzamento. Sulla base di queste informazioni
                 verranno prodotti dataset delle variabili rappresentative delle principali caratteristiche fenotipiche,
                 biochimiche e genetiche associate all’ulivo dalle quali nascerà un database messo a disposizione ai
                 produttori olivicoli campani.", "Per maggiori informazioni sul progetto clicca",
                                      strong(a(href = "https://olivehealth.it", "qui", .noWS = "after"), .noWS = "after"),
                                      ".", style = "font-family:'Quicksand', Sans-serif; font-weight: 400; line-height: 1.5; text-align: justify;"
                                 )),
                          )
                        ),
                        
                        fluidRow(column(1, offset = 5, 
                                        br(),
                                        actionButton("jumpToP2", label = HTML("&nbsp;VAI!"), icon("fas fa-rocket"), class = "btn btn-primary btn-lg", width = "200px", style='padding:10px; font-size:200%; font-weight: bold;')
                        )
                        ),
                        
                        p(h4("Questo progetto è stato finanziato da:")),
                        fluidRow(column(1, 
                                        tags$img(src = "www/Logo_AprolCampania.jpg", width = "225", height = "150")
                        ),
                        column(2, offset = 4, 
                               tags$img(src = "www/CNR.png", width = "150", height = "155")
                        ),
                        column(3, offset = 2,
                               tags$img(src = "www/stringa-2.png", width = "423", height = "110")
                        )
                        )
               ),
               
               ##### Lista menuItem #### 
               tabPanel(title = "Codice", value = "panel2", 
                        dashboardPage(
                          dashboardHeader(title = "OliveHealthR"),
                          dashboardSidebar(
                            sidebarMenu(
                              menuItem("Azienda", tabName = "azienda", startExpanded = TRUE,
                                       menuSubItem("Home", tabName = "homesub"),
                                       menuSubItem("Mappa", tabName = "mappa1sub")
                              ),
                              menuItem("Campionamento azienda", tabName = "campazienda",
                                       menuSubItem("Drupe e foglie", tabName = "drupleafsub"),
                                       menuSubItem("Olio", tabName = "oliosub"),
                                       menuSubItem("Calendario campionamenti", tabName = "calendar"),
                                       menuSubItem("Analisi sensoriali", tabName = "assaggsub")),
                              menuItem("Analisi laboratorio", tabName = "anlab",
                                       menuSubItem("Polifenoli totali", tabName = "totpolsub"),
                                       menuSubItem("Polifenoli individuali", tabName = "inpolsub"),
                                       menuSubItem("Mappa Polifenoli", tabName = "mappapoli"),
                                       menuSubItem("Analisi cromatografica", tabName = "ancromasub"),
                                       menuSubItem("Analisi morfometrica", tabName = "anmorfosub")),
                              menuItem("Integrazione dati", tabName = "integrdati")
                            )
                          ), 
                          dashboardBody(
                            tabItems(
                              
                              ##### tabItem Home ####
                              tabItem(tabName = "homesub",
                                      sidebarLayout(
                                        sidebarPanel(width = 2,
                                          #seleziona il separatore
                                          radioButtons("delim", "Scegli il separatore", choices = c("Virgola" = ",", "Punto e virgola" = ";", "Tab" = "\t"), selected = ";"),
                                                     
                                          #scegli se header
                                          checkboxInput("header", "Header", TRUE),
                                          hr(), 
                                                     
                                          #carica il file
                                          fileInput("file1", "File aziende (.csv)"),
                                          br(), 
                                                     
                                          #file descrizione aziende
                                          conditionalPanel(condition = "output.file1_ready",
                                            
                                            fileInput("desc1", "File descrizione .csv (opzionale)"),
                                            hr(),
                                            tags$h3("File con dati"), 
                                            radioButtons("tipofile", label = "Tipo di file", choices = list("Schede campionamento" = 1, "Polifenoli" = 2, "Morfometria" = 3),
                                                         selected = 1),
                                            
                                            conditionalPanel(condition = "input.tipofile == 1",
                                              fileInput("drupeinput", "Campionamento drupe e foglie (.csv)"),
                                              fileInput("olioinput", "Campionamento olio (.csv)")),
                                                                      
                                            conditionalPanel(condition = "input.tipofile == 2",
                                              fileInput("polifinput", "File dati polifenoli (.csv)")),
                                                                      
                                            conditionalPanel(condition = "input.tipofile == 3",
                                              fileInput("morfoleafinput", "File morfometria foglie 2D (.csv)"),
                                              fileInput("morfodrupeinput", "File morfometria drupe 3D (.csv)"),
                                              fileInput("morfoendoinput", "File morfometria endocarpo 3D (.csv)"),
                                              fileInput("morforatioinput", "File rapporti drupe endocarpo (.csv)")
                                            )
                                          )
                                        ),
                                        
                                        mainPanel(width = 10,
                                              tabsetPanel(id = "tab2",
                                                          tabPanel(title = "Tabella",
                                                                   box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                                                       DT::DTOutput("content")
                                                                   )
                                                          ),
                                                          
                                                          tabPanel("Cultivar",
                                                                   box(width=NULL, status = "primary", h4(htmlOutput("numcult"))),
                                                                   br(),
                                                                   fluidRow(column(width = 2,
                                                                                   box(width = NULL, status = "primary",
                                                                                       radioButtons("selplotcult", label = h4("Tipo di grafico"), choices = list("Grafico a torta" = 1, "Grafico a barre" = 2), selected = 2))
                                                                   ),
                                                                   column(width=10, box(width=NULL, status = "primary", uiOutput("cultplot")))
                                                                   )
                                                          ),
                                                          
                                                          tabPanel("Descrizione", 
                                                                   tags$h3(strong("Selezionare un'azienda")),
                                                                   fluidRow(column(width=4, DT::DTOutput("descriz")),
                                                                            column(width=8, 
                                                                                   htmlOutput("taby12"), 
                                                                                   br(), 
                                                                                   htmlOutput("contatti")
                                                                            )
                                                                   )
                                                          )
                                              )#end of tabsetpanel

                                        )#end of mainpanel
                                      )#end of sidebarlayout
                              ),#end of tabitem "Home"
                              
                              
                              tabItem(tabName = "mappa1sub", 
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     div(actionButton("update", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                                     hr(), 
                                                     selectInput("select3", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE)
                                        ),
                                        mainPanel(width = 9,
                                          conditionalPanel(condition = "input.update != 0",
                                                           tmapOutput("map1")
                                          )
                                        )
                                      )
                              ), #end of tabitem mappa1sub
                              
                              
                              
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
                                            
                                            plotly::plotlyOutput("plotxy")
                                          ),
                                          
                                          tabPanel("Barplot",
                                            fluidRow(column(width = 2, br(),
                                                            box(width = NULL, status = "primary", checkboxGroupInput("checkcamp", "Seleziona campionamento", choices = ""))
                                                           ),
                                                     column(width = 10, br(), box(width=NULL, status = "primary", plotly::plotlyOutput("barplot1")))
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
                                          
                                          fluidRow(column(12, box(width=NULL, status = "primary", DT::DTOutput("prov2"))))
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
                                    #qui la mappa
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                        div(actionButton("update2map", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                        conditionalPanel(condition = "input.update2map != 0",
                                          hr(),
                                          selectInput("select2map", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                          selectInput("selyear", "Seleziona l'anno", choices = "", multiple = FALSE),
                                          selectInput("num", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
                                          br(),
                                          hr(),
                                          br(),
                                          div(actionButton("addmap2", label = "Aggiungi seconda mappa"), align = "center"),
                                        ),
                                        conditionalPanel(condition = ("input.addmap2 != 0"),
                                          br(),
                                          selectInput("select3map", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                          selectInput("selyear2", "Seleziona l'anno", choices = "", multiple = FALSE),  
                                          selectInput("num2map", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
                                        )
                                      ),
                                      
                                      mainPanel(
                                        conditionalPanel(condition = ("input.update2map != 0"),
                                          tmapOutput("map2")),
                                        conditionalPanel(condition = ("input.addmap2 != 0"),
                                          hr(),
                                          tmapOutput("map3"))
                                      )
                                    ) #end of sidebarlayout
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
                                  
                                  tabPanel(
                                    tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")),
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
                                          tmapOutput("mapolio")),
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
                                    plotOutput("yearcalendar", height = "700px")
                                  )
                                )
                              ),
                              
                              tabItem(tabName = "assaggsub"),
                              
                              
                              
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
                                    )
                                  ),
                                  
                                  mainPanel(width = 10,
                                    tabBox(id = "tabboxpoltot", width = NULL,
                                           
                                      tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), value = "tabdtpoltot",
                                               fluidPage(
                                                 box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                                     DT::DTOutput("tablepoltot")),
                                                 mod_render_NAbox_ui("naboxpoltot")
                                               )
                                      ), #end of tabpanel tabella
                                      
                                      
                                      tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")), value = "tabpoltotgraph",
                                               tabsetPanel(id = "boxpoltotgraph",
                                                 
                                                           
                                                 tabPanel("Scatter plot", value = "boxscattpoltot",
                                                   box(width=NULL, status = "primary",
                                                     fluidRow(
                                                       column(3, selectInput("selyearscattertot", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                       column(3, selectInput("numtot", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)),
                                                       column(3, selectInput("selectfilltot", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                                     )),
                                                   plotly::plotlyOutput("totscatplot")
                                                 ), #end of tabpanel scatter plot
                                                 
                                                 
                                                 
                                                 tabPanel("Barplot", value = "boxbarpoltot",
                                                   plotly::plotlyOutput("barplottot")
                                                 ) #end of tabpanel barplot
                                                 
                                               ) #end of tabsetpanel grafici
                                      ) #end of tabpanel grafici
                                      
                                      
                                      
                                      ) #end of tabbox poltot
                                    ) #end of mainpanel
                                )#end of sidebarlayout
                                      
      
                              ), #end of tabitem "totpolsub"
                              
                              # Tab polifenoli individuali ----------------------------------------------------------------------------
                              tabItem(tabName = "inpolsub",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    radioGroupButtons("selfilepolind", "Seleziona i polifenoli da analizzare", 
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
                                    
                                    conditionalPanel(
                                      condition = "input.tabboxpolind  == 'tabpolindgraph'",
                                      h4(strong("Impostazioni grafici")),
                                      
                                      # Scatterplot ______________________________________________________
                                      conditionalPanel(condition = "input.boxpolindgraph == 'boxscattpolind'",
                                                       selectInput("selectxind", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                                       selectInput("selectyind", "Seleziona la colonna Y", choices = "", multiple = FALSE)
                                      ),
                                      
                                      # Barplot ____________________________________________________________
                                      conditionalPanel(condition = "input.boxpolindgraph == 'boxbarpolind'",
                                        selectInput("selectxindbar", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                        selectInput("selectyindbar", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                        selectInput("selectfillindbar", "Colonna da usare come riempimento", choices = c("N_campionamento", "Cultivar_principale"), selected = "N_campionamento"),
                                        hr(),
                                        checkboxGroupInput("checkcampind", "Seleziona campionamento", choices = "")
                                      ),
                                    
                                    # Heatmap _______________________________________________________
                                    conditionalPanel(
                                      condition = "input.boxpolindgraph == 'boxheatpolind'",
                                      div(actionButton("updateheat", label = "Carica!", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:150%; font-weight: bold;'), align= "center"),
                                      br(),
                                      h4(strong("Dati")),
                                      selectInput("numheat", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
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
                                      
                                      
                                    # Correlation _________________________________________________________________
                                    conditionalPanel(
                                      condition = "input.boxpolindgraph == 'boxcorrpolind'",
                                      selectInput("selyearcorrind", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numcorr", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
                                    ),
                                    ), #end of conditional panel dei grafici
                                    
                                    
                                      
                                    # PCA ____________________________________________________________________
                                    conditionalPanel(
                                      condition = "input.tabboxpolind == 'tabpolindpca'",
                                      selectInput("selyearpca", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numpca", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
                                      radioGroupButtons(inputId = "selcorpca", label = "Matrice:", choices = c("Correlazione" = TRUE, "Covarianza" = FALSE), 
                                                        individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                    ),
                                      
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
                                                column(3, selectInput("numind", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)),
                                                column(3, selectInput("selectfillind", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                              )),
                                            plotly::plotlyOutput("scatterindpol")
                                          ), #end of tabpanel scatterplot

                                          
                                          
                                          tabPanel("Barplot", value = "boxbarpolind",
                                            plotly::plotlyOutput("barplotind")
                                          ), #end of tabpanel barplot
                                                 
                                                 
                                                 
                                          tabPanel("Heatmap", value = "boxheatpolind",
                                            htmlOutput("heatmap_output")
                                          ),
                                                 
                                                 
                                                 
                                          tabPanel("Correlation Plot", value = "boxcorrpolind",
                                            plotly::plotlyOutput("corrplotind")
                                          ) #end of tabpanel correlation
                                                 
                                        ) #end of tabsetpanel
                                      ), #end of tabPanel Grafici
                                      
                                      
                                      
                                      tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")), value = "tabpolindpca",
                                        tabsetPanel(
                                          
                                          tabPanel("Screeplot",
                                            br(),
                                            plotly::plotlyOutput("screeplot", width = "75%")         
                                          ),
                                                             

                                          tabPanel("Loadings",
                                            br(),
                                            fluidRow(column(4, box(width = NULL, status = "primary", uiOutput("sliderpc")))),
                                            fluidRow(plotly::plotlyOutput("loadings"))
                                            ),
                                                             

                                          tabPanel("Biplot",
                                            br(),
                                            fluidRow(
                                              column(4, box(width = NULL, status = "primary", 
                                                            selectInput("colbiplot", "Seleziona colonna riempimento", choices = c("Cultivar_principale", "Areale", "Provincia")))),
                                              column(4, box(width = NULL, status = "primary",
                                                            awesomeCheckboxGroup("shpbiplot", "Aggiungi geometria", choices = "Provincia", inline = TRUE)
                                              ))
                                            ),
                                            fluidRow(plotly::plotlyOutput("biplot", height = "500px"))
                                          ),
                                                             
                                          
                                          tabPanel("Plot 3D",
                                            br(),
                                            fluidRow(
                                              column(4, box(width = NULL, status = "primary", 
                                                            selectInput("col3dind", "Seleziona colonna riempimento", choices = c("Provincia", "Cultivar_principale", "Areale"))))
                                            ),
                                            fluidRow(plotly::plotlyOutput("pca3dpolind", height = "500px"))
                                          )

                                        ) #end of tabsetpanel
                                      ) #end of tabpanel PCA
                                      
                                      
                                      
                                    )
                                  )
                                )

                              ), #end of tabitem "inpolsub"
                              
                              
                              
                              tabItem(tabName = "mappapoli",
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                    div(actionButton("upmappol", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:160%'), align = "center"),
                                    conditionalPanel(condition = "input.upmappol != 0",
                                      hr(),
                                      selectInput("mapxpol", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                      selectInput("selyearpol", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      selectInput("numpol", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
                                      br(),
                                      hr(),
                                      br(), 
                                      div(actionButton("addmappol2", label = "Aggiungi seconda mappa"), align = "center"),
                                    ),
                                    conditionalPanel(condition = "input.addmappol2 != 0",
                                      br(), 
                                      selectInput("mapxpol2", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                      selectInput("selyearpol2", "Seleziona l'anno", choices = "", multiple = FALSE),  
                                      selectInput("numpol2", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
                                    )
                                  ), 
                                  
                                  mainPanel(width = 9,
                                    conditionalPanel(condition = ("input.upmappol != 0"),
                                      tmapOutput("mappol")
                                    ),

                                    conditionalPanel(condition = ("input.addmappol2 != 0"),
                                      hr(),
                                      tmapOutput("mappol2")
                                    )
                                  )
                                ) #end of sidebarlayout
                                
                              ),
                              
                              ###### Tab Cromatografia ###########
                              
                              tabItem(
                                tabName = "ancromasub",
                                sidebarLayout(
                                  sidebarPanel(width = 2,
                                    
                                    radioGroupButtons("selfilecromatph", "Seleziona i polifenoli da analizzare", 
                                      choiceValues = list("foglie", "drupe", "olio"), #, "posa", "sansa"
                                      choiceNames = list(
                                        paste(shiny::icon("leaf",  style='font-size:16px;'), HTML("<b style=font-size:16px>&nbsp;Foglie</b>")),
                                        paste(tags$img(src = "www/olive_icon2.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Drupe</b>")),
                                        paste(tags$img(src = "www/olive_oil.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Olio</b>"))
                                        #paste(tags$img(src = "www/posa.png", height = "22px", width = "22px"), HTML("<b style=font-size:16px>&nbsp;Posa</b>")),
                                        #paste(tags$img(src = "www/sansa3.png", height = "23px", width = "23px"), HTML("<b style=font-size:16px>&nbsp;Sansa</b>"))
                                        ),
                                      direction = "vertical", justified = TRUE, status = "primary"
                                    ),
                                    
                                  ), #end of sidebarpanel
                                  
                                  mainPanel(width = 10,
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
                                        conditionalPanel(condition = "input.prov3_rows_selected == 0",
                                          p(strong(h4("Per favore seleziona un'azienda dalla tabella", align = "center")))
                                        ),
                                        
                                        conditionalPanel(condition = "input.prov3_rows_selected != 0",
                                          column(8, 
                                            fluidRow(
                                              box(width=NULL, status = "primary", title = "Cromatogramma", align= "center", uiOutput("phcromat"))
                                            ),
                                            fluidRow(
                                              box(width=NULL, status = "primary", title = "Polifenoli",align = "center", 
                                              
                                              #inserire qui i dati numerici dei polifenoli
                                                      
                                              )
                                            )
                                          )
                                        )
                                      ) #end of fluidRow
                                    )
                                  ) #end of mainpanel
                                ) #end of sidebarlayout
                              ), 
                              
                              
                              
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
                                      materialSwitch(inputId = "summarizetab", label = "Sintetizza i dati", status = "primary"),
                                      conditionalPanel(condition = "input.summarizetab == true",
                                        awesomeCheckboxGroup("selectdtmorfo", "Seleziona la colonna da usare per la sintesi", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale", "N_campionamento"), selected = "Codice_azienda")
                                      )
                                    ),
                                    

                                    # Grafici
                                    conditionalPanel(
                                      condition = "input.tabboxmorfo  == 'tabpanmorfograph'",
                                    h4(strong("Impostazioni grafici")),
                                    #boxplot e barplot
                                    conditionalPanel(
                                      condition = "input.boxmorfograph == 'tabpanboxmorfo' || input.boxmorfograph == 'tabpanbarmorfo'",
                                      selectInput("selectxmorfobb", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                      selectInput("selectymorfobb", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                      selectInput("selectfillmorfobb", "Colonna da usare come riempimento", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE)
                                    ),
                                    
                                    #scatterplot
                                    conditionalPanel(
                                      condition = "input.boxmorfograph == 'tabpanscattmorfo'",
                                      materialSwitch(inputId = "summarizescatt", label = "Sintetizza i dati", status = "primary"),
                                      conditionalPanel(condition = "input.summarizescatt == true",
                                                       selectInput("selectsummscatt", "Seleziona la colonna da usare per la sintesi", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = TRUE),
                                                       hr(),
                                      ),
                                      selectInput("selectxmorfoscatt", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                      selectInput("selectymorfoscatt", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                      selectInput("selectfillmorfoscatt", "Colonna da usare come riempimento", choices = "", multiple = FALSE)
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
                                    materialSwitch(inputId = "summarizepcamorfo", label = "Sintetizza i dati", status = "primary"),
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
                                        selectInput("corrtestfill", "Colonna riempimento", choices = "", multiple = FALSE)
                                        
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
                                              
                                              fluidRow(column(12, box(width=NULL, status = "primary", DT::DTOutput("dtfotomorfo"))))
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
                                               plotly::plotlyOutput("boxmorfo")
                                               ),
                                      
                                          tabPanel("Barplot", value = "tabpanbarmorfo",
                                               plotly::plotlyOutput("barmorfo")
                                               ),
                                          
                                          tabPanel("Scatter plot", value = "tabpanscattmorfo",
                                                   plotly::plotlyOutput("scattmorfo")
                                                   ),
                                          
                                          tabPanel("Heatmap", value = "tabpanheatmorfo",
                                                   br(),
                                                   htmlOutput("heatmap_outputmorfo")
                                                   ),
                                          tabPanel("Correlation plot", value = "tabpancorrmorfo",
                                            br(),
                                            plotly::plotlyOutput("corrplotmorfo")
                                          )
                                        )
                                      ),
                                      
                                      #pca
                                      tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")), value = "tabpcamor",
                                            tabsetPanel(
                                              
                                              tabPanel("Biplot",
                                                       br(),
                                                       fluidPage(
                                                         fluidRow(
                                                           column(4, box(width = NULL, status = "primary",
                                                                         selectInput("colbiplotmorfo", "Seleziona colonna riempimento", choices = c("Codice_azienda", "Provincia", "Cultivar_principale", "Areale")))),
                                                           column(3, box(width = NULL, status = "primary",
                                                                         awesomeCheckboxGroup(inputId = "shpbiplotmorfo", label = "Aggiungi geometria", choices = "Provincia", inline = TRUE)
                                                           ))
                                                           
                                                         ),
                                                         fluidRow(plotly::plotlyOutput("biplotmorfo", height = "500px")))
                                              ),

                                              tabPanel("Screeplot",
                                                br(),
                                                plotly::plotlyOutput("screeplotmorfo", width = "75%")
                                              ),

                                              tabPanel("Loadings",
                                                br(),
                                                fluidPage(
                                                  fluidRow(column(4, box(width = NULL, status = "primary",
                                                                       uiOutput("sliderpcmorfo")))),
                                                  fluidRow(plotly::plotlyOutput("loadingsmorfo")))
                                                ),


                                              tabPanel("Plot 3D",
                                                       br(),
                                                       fluidPage(
                                                         fluidRow(
                                                           column(4, box(width = NULL, status = "primary",
                                                                       selectInput("col3dmorfo", "Seleziona colonna riempimento", choices = c("Provincia", "Cultivar_principale", "Areale"))))
                                                         ), 
                                                         fluidRow(plotly::plotlyOutput("pca3dmorfo", height = "500px")))
                                              )
                                            )
                                            
                                      ), #end of tabpanel PCA

                                      
                                      # Clustering
                                      tabPanel(tagList(tags$img(src = "www/clustering_icon.png", height = "16px", width = "16px"), HTML("&nbsp;Clustering")), value = "tabclustmor",
                                        tabsetPanel(
                                          tabPanel("Numero cluster",
                                            br(),
                                            plotOutput("numclustergraph", height = "800px")
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
                                              fluidRow(plotOutput("plotclustermorfo", height = "600px"))
                                            )
                                          )
                                          
                                        )
                                      ), #end of tabpanel clustering

                                      
                                      
                                      #tabpanel test d'ipotesi
                                      tabPanel(tagList(shiny::icon("clipboard-check"), HTML("&nbsp;Test d'ipotesi")), value = "tabpanmorfotest",
                                          tabsetPanel(id = "boxmorfotest",
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
                                                  column(8, offset = 2, box(width = NULL, title = strong("Grafico"), status = "primary", plotlyOutput("boxttest")))
                                                )
                                              ) #end of fluidpage
                                                
                                                 
                                          ), #end of tabpanel conf 2 gruppi
                                          
                                          
                                          tabPanel("Test correlazione", value = "tabpancorrtestmorfo",
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
                                                  column(6, box(width = NULL, title = strong("Grafico"), status = "primary", plotly::plotlyOutput("scattcorrtest")))
                                              )
                                            ),
                                          ), #end of tabpanel
                                          
                                          
                                          tabPanel("Confronto tra più gruppi", value = "tabpananovamorfo",
                                            fluidPage(
                                              fluidRow(
                                                column(12, box(width = NULL, title = strong("Boxplot"), status = "primary", plotly::plotlyOutput("boxanova")))
                                              ),
                                              
                                              fluidRow(
                                                column(6, box(width = NULL, title = strong("Test sulla normalità"), status = "primary", verbatimTextOutput("shapiroanova1"))),
                                                
                                                conditionalPanel(
                                                  condition = "(output.shapanovamorfoui == 'distribuzione normale' && (input.selectanovatest2 == 'ANOVA' || input.selectanovatest2 == 'Kruskal-Wallis')) || (output.shapanovamorfoui == 'distribuzione non normale' && input.selectanovatest2 == 'Kruskal-Wallis')",
                                                
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
                                              
                                              
                                              # selectanovatest2 = c("ANOVA", "Kruskal-Wallis")
                                              conditionalPanel(
                                                condition = "(output.shapanovamorfoui == 'distribuzione normale' && (input.selectanovatest2 == 'ANOVA' || input.selectanovatest2 == 'Kruskal-Wallis')) || (output.shapanovamorfoui == 'distribuzione non normale' && input.selectanovatest2 == 'Kruskal-Wallis')",
                                                
                                                fluidRow(
                                                  column(10,offset = 1,
                                                    box(width = NULL, title = strong("Post-hoc"), status = "primary", style = "text-align: justify;  text-align: center;",
                                                      conditionalPanel(condition = "output.signiftestmorfoui == 'significativo'",
                                                                     
                                                        conditionalPanel(condition = "input.selectanovatest2 == 'ANOVA'",
                                                          h4(strong("Tukey HSD"))), 
                                                        conditionalPanel(condition = "input.selectanovatest2 == 'Kruskal-Wallis'",
                                                          h4(strong("Dunn Test")),
                                                        ), 
                                                        plotly::plotlyOutput("posthocmorfograph")
                                                      ),
                                                      conditionalPanel(condition = "output.signiftestmorfoui == 'non significativo'",
                                                        tags$i(class = "fas fa-exclamation-triangle", style="font-size: 25px; color: rgb(243,156,18)"), 
                                                        h4(strong("Il p-value è maggiore del livello di significatività impostato. Non ci sono differenze significative tra le variabili.", style = "color: rgb(243,156,18)"))
                                                      )
                                                    )
                                                  )
                                                ) #end of fluidrow post-hoc
                                              )
                                            )
                                          ) #end of tabpanel più gruppi
                                      
                                      ) #end of tabset panel
                                    ),
                                      
                                      
                                      #tabpanel mappa
                                      tabPanel(tagList(shiny::icon("map-marked-alt"), HTML("&nbsp;Mappa")), value = "tabmapmor",
                                              conditionalPanel(condition = ("input.upmapmorfo != 0"),
                                              tmapOutput("mapmorfo1"),
                                              conditionalPanel(condition = "input.addmapmorfo2 != 0",
                                                hr(),
                                                tmapOutput("mapmorfo2")
                                              )
                                              ))
                                    ) #end of tabBox
                                    
                                  ) #end of mainpanel
                                ) #end of sidebarlayout
                                
                              )#end of tabitem morfo
                              
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

