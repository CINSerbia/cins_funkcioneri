library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)

library(tidyverse)
library(plotly)
library(RColorBrewer)
library(DT)
library(c3)
library(networkD3)

library(future)
library(promises)

tagList(
  dashboardPage(
    # Header ----
    dashboardHeader(title = "Postupci protiv funkcionera"),
    
    # Sidebar ----
    dashboardSidebar(collapsed = FALSE, 
                     sidebarMenu(
                       menuItem("Naslovna", 
                                tabName = "tab_info", 
                                icon = icon("home")),
                       menuItem("Pretraga", 
                                tabName = "tab_pretraga", 
                                icon = icon("search")),
                       menuItem("Top 10", 
                                tabName = "tab_top_10", 
                                icon = icon("sort-amount-desc")),
                       menuItem("Prijave",
                                tabName = "tab_prijave",
                                icon = icon("exclamation-triangle")),
                       menuItem("Mere", 
                                tabName = "tab_mere",
                                icon = icon("exclamation")),
                       menuItem("Dijagram postupaka", 
                                tabName = "tab_dijagram", 
                                icon = icon("project-diagram")),
                       menuItem("O projektu",
                                tabName = "o_projektu",
                                icon = icon("info"))
                     ),
                     tags$hr(),
                     
                     column(width = 12,
                            wellPanel(style = "background: #c4dbed; border: transparent",
                                      tags$p("Ukoliko imate informacije ili sumnjate da neki funkcioner nije prijavio imovinu ili se našao u sukobu interesa, pišite nam na", tags$a("office@cins.rs", href="mailto:office@cins.rs", 
                                                                                                                                                                                     style = "color: blue"),
                                             style = "color: black"), 
                                      tags$p("Mi ćemo informacije proveriti i objaviti ih ukoliko su tačne.",
                                             style = "color: black")
                            )
                     )
    ),
    
    # Body ----
    dashboardBody(
      tags$style(".glyphicon-ok {color:#009999}"),
      tags$style(".glyphicon-remove {color:#990033}"),
      tags$style(".glyphicon-ban-circle {color:#990033}"),
      
      tags$head(
        tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Work+Sans');")),
        tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Quicksand');"))
      ),
      
      tags$head(
        # twitter
        tags$meta(name="twitter:card",         content = "summary_large_image"),
        tags$meta(name="twitter:site",         content = "@CINSerbia"),
        tags$meta(name="twitter:title",        content = "Postupci protiv funkcionera"),
        tags$meta(name="twitter:description",  content = 'Baza "Postupci protiv funkcionera" sadrži informacije o skoro 2.800 postupaka koje je od 2010. do novembra 2018. godine Agencija za borbu protiv korupcije pokrenula protiv funkcionera koji nisu prijavljivali imovinu na vreme, nisu dostavljali tačne podatke o istoj ili su se našli u sukobu interesa.'),
        tags$meta(name="twitter:image",        content = "https://funkcioneri.cins.rs/twitter_image.png"),
        # open graph
        tags$meta(property="og:title",       content="Postupci protiv funkcionera"),
        tags$meta(property="og:description", content='Baza "Postupci protiv funkcionera" sadrži informacije o skoro 2.800 postupaka koje je od 2010. do novembra 2018. godine Agencija za borbu protiv korupcije pokrenula protiv funkcionera koji nisu prijavljivali imovinu na vreme, nisu dostavljali tačne podatke o istoj ili su se našli u sukobu interesa.'),
        tags$meta(property="og:image",       content="twitter_image.png")
      ),
      
      shinyWidgets::setShadow("box"),
      
      ### changing theme
      shinyDashboardThemes(
        theme = "grey_light"
      ),
      tags$head(tags$style(HTML(".small-box {height: 110px}"))),
      includeCSS("style.css"),
      useShinyjs(),
      
      # > Tab Items ----
      tabItems(
        # > > Info ----
        tabItem(tabName = "tab_info",
                
                # > > > R1 ----
                fluidRow(
                  column(width = 2,
                         valueBoxOutput("value_box_postupci", width = 12)
                  ),
                  column(width = 2,
                         valueBoxOutput("value_box_prekrsaji", width = 12)
                  ),
                  column(width = 2,
                         valueBoxOutput("value_box_krivica", width = 12)
                  ),
                  column(width = 2,
                         valueBoxOutput("value_box_mera_upozorenja", width = 12)
                  ),
                  column(width = 2,
                         valueBoxOutput("value_box_mera_razresenje", width = 12)
                  ),
                  column(width = 2,
                         valueBoxOutput("value_box_mera_povreda_zakona", width = 12)
                  )
                ),
                # > > > R2 ----
                fluidRow(
                  column(width = 12,
                         box(width = 12, status = "primary",
                             column(width = 8, uiOutput("get_prvi_deo_txt")),
                             column(width = 4, c3Output("c3_mera"))
                             
                         )
                  )
                ),
                
                # > > > R3 ----
                fluidRow(
                  column(width = 12,
                         box(width = 12, status = "primary",
                             tags$br(),
                             column(width = 4, c3Output("c3_prekrsaj_krivica")),
                             column(width = 8, uiOutput("get_drugi_deo_txt"))
                         )
                  )
                )
        ),
        # > > Pretraga ----
        tabItem(tabName = "tab_pretraga",
                tags$div(id = "div_reset_filters",
                         # > > > R1 ----
                         fluidRow(
                           column(width = 6, offset = 0, 
                                  selectizeInput(inputId = 'inp_search_funkcioner',
                                                 label = "Ime i prezime",
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 width = "100%")
                           ),
                           column(width = 6,
                                  selectizeInput(inputId = 'inp_search_funkcija',
                                                 label = "Funkcija",
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 width = "100%")
                           )
                         ),
                         
                         # > > > R2 ----
                         fluidRow(
                           column(width = 6, 
                                  selectizeInput(inputId = 'inp_search_mesto',
                                                 label = "Mesto",
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 width = "100%")
                           ),
                           column(width = 6,
                                  selectizeInput(inputId = 'inp_search_organ',
                                                 label = "Organ",
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 width = "100%")
                           )
                         ),
                         
                         # > > > R3 ----
                         fluidRow(
                           column(width = 6,
                                  checkboxGroupButtons(inputId = "inp_switch_prijava",
                                                       label = "Prijava",
                                                       choices = c("Prekršajna" = "Prekršajna prijava", 
                                                                   "Krivična" = "Krivična prijava", 
                                                                   "Bez prijave" = "Bez prijave"),
                                                       selected = c("Prekršajna prijava", 
                                                                    "Krivična prijava", 
                                                                    "Bez prijave"),
                                                       status = NULL,
                                                       individual = FALSE,
                                                       justified = TRUE,
                                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                        no = icon("remove", lib = "glyphicon"))
                                  )
                           ),
                           column(width = 6,
                                  selectizeInput(inputId = 'inp_ishod_postupka',
                                                 label = "Ishod prijave",
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 width = "100%")
                           )
                         ),
                         
                         # > > > R4 ----
                         fluidRow(
                           
                           column(width = 3,
                                  checkboxGroupButtons(inputId = "inp_switch_mera_upozorenja", 
                                                       label = "Mera upozorenja",
                                                       choices = c("Da" = "da", "Ne" = "ne"), 
                                                       selected = c("da", "ne"),
                                                       status = NULL, 
                                                       individual = FALSE,
                                                       justified = TRUE,
                                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                                                        no = icon("remove", lib = "glyphicon"))
                                  )
                           ),
                           column(width = 3,
                                  checkboxGroupButtons(inputId = "inp_switch_mera_razresenje", 
                                                       label = "Preporuka za razrešenje", 
                                                       choices = c("Da" = "da", "Ne" = "ne"), 
                                                       selected = c("da", "ne"),
                                                       justified = TRUE,
                                                       status = NULL, 
                                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                                                        no = icon("remove", lib = "glyphicon"))
                                  )
                           ),
                           column(width = 3,
                                  checkboxGroupButtons(inputId = "inp_switch_mera_povreda_zakona", 
                                                       label = "Odluka o povredi zakona", 
                                                       choices = c("Da" = "da", "Ne" = "ne"), 
                                                       selected = c("da", "ne"),
                                                       justified = TRUE,
                                                       status = NULL, 
                                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                                                        no = icon("remove", lib = "glyphicon"))
                                  )
                           ),
                           column(width = 1, offset = 2,
                                  tags$br(),
                                  tags$div(align = "center", style = "vertical-align: middle;",
                                           actionBttn(inputId = "inp_reset_filters", 
                                                      label = "",
                                                      icon = icon("redo-alt"),
                                                      style = "material-circle", 
                                                      color = "success",
                                                      block = TRUE,
                                                      size = "sm",
                                                      no_outline = TRUE)
                                  )
                           )
                           
                         )
                ),
                
                # > > > R5 ----
                fluidRow(
                  
                  # + Tabela ----
                  box(width = 6,
                      title = NULL, 
                      status = "primary",
                      DT::dataTableOutput("pretraga_tabela") %>% 
                        withSpinner(type = 1, size = 3)
                  ),
                  
                  # + Kartica postupka ----
                  box(width = 6,
                      status = "primary",
                      uiOutput("kartica_postupka")
                  )
                )
        ),
        # > > Top 10 ----
        tabItem(tabName = "tab_top_10",
                # > > > R1 ----
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("top_10_funkcionera", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("top_10_funkcija", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                
                # > > > R3 ----
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("top_10_organa", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("top_10_mesta", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                
                # > > > R2 ----
                fluidRow(
                  box(width = 12,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("top_10_zakonski_osnov", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                )
        ),
        
        # > > Mere ----
        tabItem(tabName = "tab_mere",
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("mere_upozorenja_po_godinama", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("mera_upozorenja_funkcija", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("preporuka_za_rezresenje_po_godinama", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("preporuka_za_rezresenje_funkcija", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("odluka_o_povredi_zakona_po_godinama", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("odluka_o_povredi_zakona_funkcija", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                )
        ),
        
        # > > Prijave ----
        tabItem(tabName = "tab_prijave",
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("krivicne_po_godinama", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("prekrsajne_po_godinama", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("krivicne_funkcija_organ", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("prekrsajne_funkcija_organ", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                fluidRow(
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("krivicne_ishod_postupka", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  ),
                  box(width = 6,
                      status = "primary",
                      height = "380px",
                      plotlyOutput("prekrsajne_ishod_postupka", height = "340px") %>% 
                        withSpinner(type = 1, size = 2)
                  )
                ),
                
                fluidRow(
                  column(width = 12,
                         tags$br(),
                         tags$div(align = "right", 
                                  tags$p(tags$em("Prekršajni postupci do 2018. godine"),
                                         style = "font-size: x-small")
                         )
                  )
                )
        ),
        
        # > > Dijagram postupaka ----
        tabItem(tabName = "tab_dijagram",
                fluidRow(
                  column(width = 12,
                         tabBox(
                           title = tagList(shiny::icon("project-diagram"), "Dijagram postupaka"),
                           height = "750px",
                           width = 12,
                           # tab 1 - Funkcija > Mera > Prijava
                           tabPanel(
                             title = "Funkcija > Mera > Prijava",
                             sankeyNetworkOutput("dijagram_postupaka_fu_m_p", height = "750px")
                           ),
                           # tab 2 - Zakonski osnov -> Prijava
                           tabPanel(
                             title = "Zakonski osnov > Prijava",
                             sankeyNetworkOutput("dijagram_postupaka_zo_p", height = "750px")
                           ),
                           # tab 3 - Funkcija -> Prijava
                           tabPanel(
                             title = "Funkcija > Prijava",
                             sankeyNetworkOutput("dijagram_postupaka_fu_p", height = "750px")
                           ),
                           # tab 4 - Zakonski osnov > Mera > Prijava
                           tabPanel(
                             title = "Zakonski osnov > Mera > Prijava",
                             sankeyNetworkOutput("dijagram_postupaka_zo_m_p", height = "750px")
                           ),
                           # tab 5 - Prijava > Postupak > Resenje
                           tabPanel(
                             title = "Prijava > Postupak > Rešenje",
                             sankeyNetworkOutput("dijagram_postupaka_pr_po_re", height = "750px")
                           )
                         )
                  )
                ),
                fluidRow(
                  column(width = 12,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$div(align = "right", 
                                  tags$p(tags$span(icon("chrome"), style = "font-size: small"), 
                                         " Optimizovano za ", tags$em("Google Chrome"), "pretraživač",
                                         style = "font-size: x-small")
                         )
                  )
                )
        ),
        
        # > > O projektu ----
        tabItem(tabName = "o_projektu",
                fluidRow(
                  column(width = 12,
                         box(width = 9,
                             status = "primary",
                             uiOutput("get_o_projektu_txt"))
                  )
                )
        )
        
        
      )
      
    ) # body
  ), # page
  
  tags$footer(
    fluidRow(
      column(width = 10, offset = 1,
             tags$div(align = "center",
                      tags$p("CINS - Centar za istraživačko novinarstvo Srbije © 2018"),
                      tags$p("Hvala vam što ćete koristiti podatke iz baze, uz to je potrebno da navedete CINS kao autora baze i link: https://funkcioneri.cins.rs")
             )
      ),
      column(width = 1)
    ),
    
    align = "left",
    style = "
              bottom:0;
              width:100%;
              height:130px;   /* Height of the footer */
              color: white;
              padding: 20px;
              background-color: #1A1A1A;
              z-index: 1000;")
) # tagList