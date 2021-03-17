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
                                       menuSubItem("Foto campioni", tabName = "fotosub"),
                                       menuSubItem("Grafici", tabName = "graficisub"),
                                       menuSubItem("Mappa", tabName = "mappa2sub"),
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
                                              fileInput("drupeinput", "File schede campionamento (.csv)")),
                                                                      
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
                                                     actionButton("update", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:120%'),
                                                     hr(), 
                                                     selectInput("select3", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE)
                                        ),
                                        mainPanel(width = 9,
                                          conditionalPanel(condition = ("input.update != 0"),
                                                           tmapOutput("map1")
                                          )
                                        )
                                      )
                              ), #end of tabitem mappa1sub
                              
                              
                              
                              ##### TabItem Campionamento Azienda ####
                              
                              tabItem(tabName = "graficisub",
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
                                                         fluidRow(column(width = 2,
                                                                         br(),
                                                                         box(width = NULL, status = "primary", checkboxGroupInput("checkcamp", "Seleziona campionamento", choices = ""))
                                                         ),
                                                         column(width = 10, br(), box(width=NULL, status = "primary", plotly::plotlyOutput("barplot1")))
                                                         )
                                                )#end of tabpanel
                                              )#end of tabset
                                        )#end of mainpanel
                                        
                                      )#end of sidebarlayout
                              ),#end of tabitem "grafici"
                              
                              
                              
                              tabItem(tabName = "fotosub",
                                      fluidPage(
                                        fluidRow(
                                          column(width=4, 
                                                 fluidRow(
                                                   column(6,
                                                          box(width = NULL, status = "primary",
                                                              radioGroupButtons(inputId = "campfoto", label = "Numero campionamento", 
                                                                                choices = c("1" = "1_campionamento", "2" = "2_campionamento"),
                                                                                individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                                                                                                                    no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                                          )
                                                   ),
                                                   column(6, 
                                                          box(width=NULL, status = "primary", selectInput("selyearfoto", "Seleziona l'anno", choices = "", multiple = FALSE))
                                                   )
                                                 ),
                                                 fluidRow(column(12, box(width=NULL, status = "primary", DT::DTOutput("prov2"))))
                                          ),
                                          conditionalPanel(condition = ("input.prov2_rows_selected == 0"),
                                                           p(strong(h4("Per favore seleziona un'azienda dalla tabella", align = "center")))
                                          ),
                                          
                                          conditionalPanel(condition = ("input.prov2_rows_selected != 0"),
                                                           column(width = 4, box(width=NULL, status = "primary", title = "Foglie", align= "center", uiOutput("phfoglia"))),
                                                           column(width = 4, box(width=NULL, status = "primary", title = "Drupe",align = "center", uiOutput("phdrupa")))
                                          )
                                        ) #end of fluidRow
                                      )
                              ),#end of tabItem
                              
                              tabItem(tabName = "mappa2sub",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     actionButton("update2map", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:120%'),
                                                     hr(),
                                                     selectInput("select2map", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                                     selectInput("selyear", "Seleziona l'anno", choices = "", multiple = FALSE),
                                                     selectInput("num", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
                                                     br(),
                                                     hr(),
                                                     br(),
                                                     actionButton("addmap2", label = "Aggiungi seconda mappa"),
                                                     conditionalPanel(
                                                       condition = ("input.addmap2 != 0"),
                                                       br(),
                                                       selectInput("select3map", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                                       selectInput("selyear2", "Seleziona l'anno", choices = "", multiple = FALSE),  
                                                       selectInput("num2map", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
                                                     )
                                        ),
                                        
                                        mainPanel(
                                          conditionalPanel(condition = ("input.update2map != 0"),
                                                           tmapOutput("map2"),
                                          ),
                                          conditionalPanel(condition = ("input.addmap2 != 0"),
                                                           hr(),
                                                           tmapOutput("map3")
                                          )
                                        )
                                      )
                              ),
                              
                              tabItem(tabName = "assaggsub"),
                              
                              
                              
                              ##### TabItem Analisi Laboratorio #####
                              
                              # Tab polifenoli totali ----------------------------------------------------------------
                              tabItem(tabName = "totpolsub",
                                      tabBox(width=NULL,
                                             
                                             tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")),
                                                      DT::DTOutput("tablepoltot")
                                             ),
                                             
                                             tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")),
                                                      
                                                      sidebarLayout(
                                                        sidebarPanel(width=2,
                                                                     selectInput("selectxtot", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                                                     selectInput("selectytot", "Seleziona la colonna Y", choices = "", multiple = FALSE)
                                                        ),
                                                        
                                                        mainPanel(width = 10,
                                                              tabsetPanel(
                                                                tabPanel("Scatter plot",
                                                                         br(),
                                                                         box(width=NULL, status = "primary",
                                                                             fluidRow(
                                                                               column(3, selectInput("selyearscattertot", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                                               column(3, selectInput("numtot", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)),
                                                                               column(3, selectInput("selectfilltot", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                                                             )),
                                                                         plotly::plotlyOutput("totscatplot")
                                                                ),
                                                                
                                                                tabPanel("Barplot",
                                                                         fluidRow(
                                                                           column(width = 2, 
                                                                                  br(),
                                                                                  box(width = NULL, status = "primary", checkboxGroupInput("checkcamptot", "Seleziona campionamento", choices = ""))
                                                                           ),
                                                                           column(width = 10, br(), box(width=NULL, status = "primary", plotly::plotlyOutput("barplottot")))
                                                                         )#end of fluidRow
                                                                )
                                                              )#end of tabsetpanel
                                                        )
                                                      )
                                                      
                                             )
                                      ) #end of tabsetpanel
                                      
                              ), #end of tabitem "totpolsub"
                              
                              # Tab polifenoli individuali ----------------------------------------------------------------------------
                              tabItem(tabName = "inpolsub",
                                tabBox(width=NULL,
                                  tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), 
                                    box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                        DT::DTOutput("tablepolind")
                                    )),
                                             
                                             
                                  
                                  tabPanel(tagList(shiny::icon("chart-bar"), HTML("&nbsp;Grafici")),
                                    tabsetPanel(
                                      tabPanel("Scatter plot",
                                        sidebarLayout(
                                          
                                          sidebarPanel(width = 2,
                                            selectInput("selectxind", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                            selectInput("selectyind", "Seleziona la colonna Y", choices = "", multiple = FALSE)
                                          ),
                                                                   
                                          
                                          mainPanel(width = 10,
                                            br(),
                                            box(width=NULL, status = "primary",
                                              fluidRow(
                                                column(3, selectInput("selyearscatterind", "Seleziona l'anno", choices = "", multiple = FALSE)),
                                                column(3, selectInput("numind", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)),
                                                column(3, selectInput("selectfillind", "Colonna da usare come riempimento", choices = "", multiple = FALSE))
                                              )),
                                            plotly::plotlyOutput("scatterindpol")
                                          )
                                        ) #end of sidebarlayout
                                      ), #end of tabpanel scatterplot
                                                        
                                                        
                                      
                                      tabPanel("Barplot",
                                        sidebarLayout(
                                          
                                          sidebarPanel(width = 2,
                                            selectInput("selectxindbar", "Seleziona la colonna X", choices = "", multiple = FALSE),
                                            selectInput("selectyindbar", "Seleziona la colonna Y", choices = "", multiple = FALSE),
                                            hr(),
                                            checkboxGroupInput("checkcampind", "Seleziona campionamento", choices = "")
                                          ),
                                                                   
                                          mainPanel(width = 10,
                                            br(),
                                            box(width=12, status = "primary", plotly::plotlyOutput("barplotind"))
                                          )
                                        ) #end of sidebarlayout
                                      ), #end of tabpanel barplot
                                                        
                                                        
                                      
                                      tabPanel("Heatmap",
                                        sidebarLayout(
                                          sidebarPanel(width = 3,
                                            div(actionButton("updateheat", label = "Carica!", class = "btn btn-primary btn-lg", width = "160px", style='padding:5px; font-size:200%; font-weight: bold;'), align= "center"),
                                            br(),
                                            h4(strong("Dati")),
                                            fluidRow(
                                              column(6, selectInput("numheat", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)),
                                              column(6, br(), selectInput("selyearheatind", "Seleziona l'anno", choices = "", multiple = FALSE))
                                            ),
                                            hr(),
                                            h4(strong("Preprocessing")),
                                            selectInput("selscaleheat", "Scala i dati:", choices = c("No" = "none", "Per riga" = "row", "Per colonna" = "column"), selected = "none"),
                                            hr(),
                                            h4(strong("Opzioni dendrogramma")),

                                            
                                            ###voglio il dendrograma su riga o colonna o entrambi?
                                            h5(strong("Scegli dove mostrare il dendrogramma")),
                                            fluidRow(
                                              column(6, materialSwitch(inputId = "rowdend", label = "Riga",  value = TRUE, status = "primary")),
                                              column(6, materialSwitch(inputId = "columndend", label = "Colonna",  value = TRUE, status = "primary"))
                                            ),
                                            hr(),
                                            selectInput("selectannot", "Aggiungi annotazione:", choices = c("Provincia", "Cultivar_principale", "Areale")),
                                            conditionalPanel(condition = "input.rowdend == 1 || input.columndend == 1",
                                              fluidRow(
                                                column(6, selectInput("seldistheatpol", "Funzione di distanza:", choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski"), selected = "euclidean")),
                                                column(6,selectInput("selhclustheatpol", "Metodo clustering:", choices = c("ward.D", "ward.D2", "single", "complete", "average" , "mcquitty", "median", "centroid"), selected = "complete"))
                                              )
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
                                          ), #end of sidebarpanel
                                                                   
                                                            
                                          
                                          mainPanel(width = 9,
                                            br(),
                                            htmlOutput("heatmap_output")
                                          )
                                        ) #end of sidebarlayout
                                      ),
                                                        
                                                        
                                      
                                      tabPanel("Correlation Plot",
                                        sidebarLayout(
                                          sidebarPanel(width = 2,
                                            selectInput("selyearcorrind", "Seleziona l'anno", choices = "", multiple = FALSE),
                                            selectInput("numcorr", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE)
                                          ),

                                          mainPanel(width = 10,
                                                    br(),
                                                    box(width=7, status = "primary", plotly::plotlyOutput("corrplotind"))
                                          ) #end of mainpanel
                                        ) #end of sidebarlayout
                                                                 
                                      ) #end of tabpanel correlation
                                                        
                                    )#end of tabsetpanel
                                  ), #end of tabPanel Grafici
                                             
                                             
                                             
                                             
                                  
                                  tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")), 
                                    sidebarLayout(
                                      sidebarPanel(width = 2,
                                        selectInput("selyearpca", "Seleziona l'anno", choices = "", multiple = FALSE),
                                        selectInput("numpca", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
                                        radioGroupButtons(inputId = "selcorpca", label = "Matrice:", choices = c("Correlazione" = TRUE, "Covarianza" = FALSE), 
                                          individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
                                      ),
                                                        
                                                        
                                      
                                      mainPanel(width = 10,
                                        tabsetPanel(
                                          tabPanel("Screeplot",
                                            br(),
                                            plotly::plotlyOutput("screeplot", width = "75%")         
                                          ),
                                                            
                                          
                                          tabPanel("Loadings",
                                            br(),
                                            fluidRow(column(4, box(width = NULL, status = "primary", uiOutput("sliderpc")))),
                                            fluidRow(plotly::plotlyOutput("loadings"))),
                                                            
                                          
                                          tabPanel("Biplot",
                                            br(),
                                            fluidRow(
                                              column(4, box(width = NULL, status = "primary", 
                                                selectInput("colbiplot", "Seleziona colonna riempimento", choices = c("Provincia", "Cultivar_principale", "Areale"))))
                                            ),
                                            fluidRow(plotly::plotlyOutput("biplot", height = "500px"))
                                          )
                                                            
                                        )
                                      ) #end of mainpanel
                                      
                                      
                                    ) #end of sidebarLayout
                                    #) #end of tabsetpanel
                                  ) #end of tabpanel PCA
                                             
                                             
                                ) #end of tabBox
                              ), #end of tabitem "inpolsub"
                              
                              
                              
                              tabItem(tabName = "mappapoli",
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                    actionButton("upmappol", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:120%'),
                                    hr(), 
                                    selectInput("mapxpol", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                    selectInput("selyearpol", "Seleziona l'anno", choices = "", multiple = FALSE),
                                    selectInput("numpol", "Scegli il numero di campionamento", choices = c("1" = "R1", "2" = "R2"), selected = "R1", multiple = FALSE),
                                    br(),
                                    hr(),
                                    br(),
                                    actionButton("addmappol2", label = "Aggiungi seconda mappa"),
                                    conditionalPanel(condition = ("input.addmappol2 != 0"),
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
                              
                              
                              tabItem(tabName = "ancromasub"),
                              
                              
                              
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
                                      h4(strong("Impostazioni tabella")),
                                      selectInput("selyeardtmorfo", "Seleziona l'anno", choices = "", multiple = FALSE),
                                      numericInput("selroundmorfo", "Numero di digits", value = 3),
                                      hr(),
                                      materialSwitch(inputId = "summarizetab", label = "Sintetizza i dati", status = "primary"),
                                      conditionalPanel(condition = "input.summarizetab == true",
                                        selectInput("selectdtmorfo", "Seleziona la colonna da usare per la sintesi", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE)
                                      )
                                    ),
                                    
                                    # Grafici
                                    conditionalPanel(
                                      condition = "input.tabboxmorfo  == 'tabpanmorfograph'",
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
                                      materialSwitch(inputId = "summarizescatt", label = "Sintetizza i dati", status = "primary"),
                                      conditionalPanel(condition = "input.summarizescatt == true",
                                                       selectInput("selectsummscatt", "Seleziona la colonna da usare per la sintesi", choices = c("Codice_azienda", "Provincia", "Azienda", "Cultivar_principale"), multiple = FALSE),
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
                                      h4(strong("Preprocessing")),
                                      selectInput("selscaleheatmorfo", "Scala i dati:", choices = c("No" = "none", "Per riga" = "row", "Per colonna" = "column"), selected = "none"),
                                      hr(),
                                      h4(strong("Opzioni dendrogramma")),
                                      
                                      ###voglio il dendrograma su riga o colonna o entrambi?
                                      h5(strong("Scegli dove mostrare il dendrogramma")),
                                      fluidRow(
                                        column(6, materialSwitch(inputId = "rowdendmorfo", label = "Riga",  value = TRUE, status = "primary", width = "90%")),
                                        column(6, materialSwitch(inputId = "columndendmorfo", label = "Colonna",  value = TRUE, status = "primary", width = "90%"))
                                      ),
                                      hr(),
                                      selectInput("selectannotmorfo", "Aggiungi annotazione:", choices = c("Provincia", "Cultivar_principale")),
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
                                      
                                      
                                     )
                                    
                                    
                                    ), #end of conditionapanel dei grafici
                                    
                                    # Mappa
                                    conditionalPanel(
                                      condition = "input.tabboxmorfo == 'tabmapmor'",
                                      actionButton("upmapmorfo", "Carica mappa", class = "btn-primary", style = 'padding:4px; font-size:120%'),
                                      conditionalPanel(
                                        condition = ("input.upmapmorfo != 0"),
                                        br(),
                                        selectInput("mapxmorfomap1", "Seleziona la colonna da visualizzare", choices = "", multiple = FALSE),
                                        selectInput("selyearmorfomap1", "Seleziona l'anno", choices = "", multiple = FALSE),  
                                        selectInput("nummorfomap1", "Scegli il numero di campionamento", choices = "", multiple = FALSE), 
                                        
                                        actionButton("addmapmorfo2", label = "Aggiungi seconda mappa"),
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
                                  
                                  
                                  mainPanel(width = 10,
                                    tabBox(id = "tabboxmorfo", width=NULL,
                                      tabPanel(tagList(shiny::icon("table"), HTML("&nbsp;Tabella")), value = "tabdtmor",
                                               fluidRow(box(width = NULL, status = "primary", style = "overflow-x: scroll;",
                                                   DT::DTOutput("dtmorfo"))),
                                               fluidRow(
                                                 column(width = 3, shinydashboard::valueBoxOutput("namorfobox", width = 12)),
                                                 column(width = 3, br(), uiOutput("namorfobuttui"))),
                                               conditionalPanel(condition = "input.namorfobutt != 0", 
                                                 fluidRow(plotOutput("namorfoplot")))
                                               
                                               ),

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
                                                   )
                                        )
                                      ),
                                      
                                      tabPanel(tagList(shiny::icon("chart-line"), HTML("&nbsp;PCA")),
                                        #qui la pca

                                      ),

                                      tabPanel(tagList(tags$img(src = "www/clustering_icon.png", height = "16px", width = "16px"), HTML("&nbsp;Clustering")),
                                               #qui la pca

                                      ),

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

