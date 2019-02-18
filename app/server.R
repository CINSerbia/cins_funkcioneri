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

baza <- readRDS("data/baza_funkcionera_jan_2019.RDS")

function(input, output, session) {
  
  options(shiny.sanitize.errors = TRUE, warn = -1)
  
  # | ----
  # | Info Tab ----
  # + get_prvi_deo_txt (Text) ----
  output$get_prvi_deo_txt <- renderUI({
    prva_strana_text <- get_prvi_deo_text()
    
    return(prva_strana_text)
  })
  # + get_drugi_deo_txt (Text) ----
  output$get_drugi_deo_txt <- renderUI({
    prva_strana_text <- get_drugi_deo_text()
    
    return(prva_strana_text)
  })

  # + c3_prekrsaj_krivica (c3) ----
  output$c3_prekrsaj_krivica <- renderC3({
    baza %>%
      dplyr::select(prijava) %>%
      dplyr::mutate(prijava = replace_na(prijava, "Bez prijave")) %>%
      dplyr::mutate(prijava = factor(prijava, levels = c("Bez prijave", 
                                                         "Prekršajna prijava", 
                                                         "Krivična prijava"))) %>%
      dplyr::group_by(prijava) %>%
      dplyr::summarise(Ukupno = n()) %>%
      tidyr::spread(prijava, Ukupno) %>%
      dplyr::mutate_all(as.numeric) %>%
      c3() %>%
      c3_bar(stacked = FALSE, rotated = FALSE, bar_width = 0.8,
             zerobased = FALSE) %>%
      xAxis(show = FALSE) %>%
      c3::legend(inset = list(anchor = 'top-left'))
  })
  # + c3_mera (c3) ----
  output$c3_mera <- renderC3({
    baza %>% 
      dplyr::select(`Mera upozorenja` = mera_upozorenja,
                    `Preporuka za razrešenje` = preporuka_za_rezresenje,
                    `Odluka o povredi zakona` = odluka_o_povredi_zakona) %>%
      tidyr::gather() %>%
      dplyr::filter(value == "da") %>%
      dplyr::group_by(key) %>%
      dplyr::summarise(Ukupno = n()) %>%
      tidyr::spread(key, Ukupno) %>%
      dplyr::mutate_all(as.numeric) %>%
      c3() %>%
      c3_donut(title = "Mere")
  })
  
  # + value_box_postupci (vB) ----
  output$value_box_postupci <- renderValueBox({
    br_postupci <- nrow(baza)
    
    valueBox(
      value = br_postupci,
      subtitle = "Postupaka",
      icon = icon("file"),
      color = "light-blue"
    )
  })
  # + value_box_prekrsaji (vB) ----
  output$value_box_prekrsaji <- renderValueBox({
    br_prekrsaja <- baza %>% 
      dplyr::count(prijava) %>% 
      dplyr::filter(prijava == "Prekršajna prijava") %>% 
      dplyr::pull(n)
    
    valueBox(
      value = br_prekrsaja, 
      subtitle = "Prekršajnih prijava", 
      icon = icon("exclamation"),
      color = "light-blue"
    )
  })
  # + value_box_krivica (vB) ----
  output$value_box_krivica <- renderValueBox({
    br_krivica <- baza %>% 
      dplyr::count(prijava) %>% 
      dplyr::filter(prijava == "Krivična prijava") %>% 
      dplyr::pull(n)
    
    valueBox(
      value = br_krivica, 
      subtitle = "Krivičnih prijava", 
      icon = icon("exclamation-triangle"),
      color = "light-blue"
    )
  })
  # + value_box_mera_upozorenja (vB) ----
  output$value_box_mera_upozorenja <- renderValueBox({
    br_mera_upozorenja <- baza %>% 
      dplyr::count(mera_upozorenja) %>% 
      dplyr::filter(mera_upozorenja == "da") %>% 
      dplyr::pull(n)
    
    valueBox(
      value = br_mera_upozorenja, 
      subtitle = "Mera upozorenja", 
      icon = icon("file-text-o"),
      color = "light-blue"
    )
  })
  # + value_box_mera_razresenje (vB) ----
  output$value_box_mera_razresenje <- renderValueBox({
    br_mera_razresenje <- baza %>% 
      dplyr::count(preporuka_za_rezresenje) %>% 
      dplyr::filter(preporuka_za_rezresenje == "da") %>% 
      dplyr::pull(n)
    
    valueBox(
      value = br_mera_razresenje, 
      subtitle = "Preporuka za razrešenje", 
      icon = icon("file-text-o"),
      color = "light-blue"
    )
  })
  # + value_box_mera_povreda_zakona (vB) ----
  output$value_box_mera_povreda_zakona <- renderValueBox({
    br_mera_povreda_zakona <- baza %>% 
      dplyr::count(odluka_o_povredi_zakona) %>% 
      dplyr::filter(odluka_o_povredi_zakona == "da") %>% 
      dplyr::pull(n)
    
    valueBox(
      value = br_mera_povreda_zakona, 
      subtitle = "Odluka o povredi zakona", 
      icon = icon("file-text-o"),
      color = "light-blue"
    )
  })
  
  # | ----
  # | Pretraga ----
  # + updateSelectizeInput (UI) ----
  updateSelectizeInput(session, 'inp_search_funkcioner', 
                       choices = c(sort(unique(baza[["ime_prezime"]]))),
                       selected = NULL,
                       server = TRUE,
                       options = list(maxOptions = 20))
  updateSelectizeInput(session, 'inp_ishod_postupka', 
                       choices = c(sort(unique(baza[["ishod_postupka"]]))),
                       selected = NULL,
                       server = TRUE,
                       options = list(maxOptions = 20))
  updateSelectizeInput(session, 'inp_search_funkcija', 
                       choices = c(sort(unique(baza[["funkcija"]]))), 
                       selected = NULL,
                       server = TRUE,
                       options = list(maxOptions = 20))
  updateSelectizeInput(session, 'inp_search_organ', 
                       choices = c(sort(unique(baza[["organ"]]))), 
                       selected = NULL,
                       server = TRUE,
                       options = list(maxOptions = 20))
  updateSelectizeInput(session, 'inp_search_mesto', 
                       choices = c(sort(unique(baza[["mesto"]]))), 
                       selected = NULL,
                       server = TRUE,
                       options = list(maxOptions = 20))
  
  
  observeEvent(input$inp_reset_filters, {
    shinyjs::reset(id = "div_reset_filters")
    updateCheckboxGroupButtons(session, inputId = "inp_switch_prijava",
                               selected = c("Prekršajna prijava", 
                                            "Krivična prijava", 
                                            "Bez prijave"))
    
    updateCheckboxGroupButtons(session, inputId = "inp_switch_mera_upozorenja",
                               selected = c("da", "ne"))
    
    updateCheckboxGroupButtons(session, inputId = "inp_switch_mera_razresenje",
                               selected = c("da", "ne"))
    
    updateCheckboxGroupButtons(session, inputId = "inp_switch_mera_povreda_zakona",
                               selected = c("da", "ne"))
  })
  
  # + get_filtered_data (R) ----
  get_filtered_data <- reactive({
    # inputs
    inp_prijava         <- input$inp_switch_prijava
    inp_upozorenje      <- input$inp_switch_mera_upozorenja
    inp_razresenje      <- input$inp_switch_mera_razresenje
    inp_porvreda_zakona <- input$inp_switch_mera_povreda_zakona
    
    inp_funkcioner <- input$inp_search_funkcioner
    inp_ishod      <- input$inp_ishod_postupka
    inp_funkcija   <- input$inp_search_funkcija
    inp_organ      <- input$inp_search_organ
    inp_mesto      <- input$inp_search_mesto
    
    data <- baza %>%
      dplyr::filter(prijava %in% inp_prijava) %>%
      dplyr::filter(mera_upozorenja %in% inp_upozorenje) %>%
      dplyr::filter(preporuka_za_rezresenje %in% inp_razresenje) %>%
      dplyr::filter(odluka_o_povredi_zakona %in% inp_porvreda_zakona)
    
    if (!is.null(inp_funkcioner)) {
      data <- data %>%
        dplyr::filter(ime_prezime %in% inp_funkcioner)
    }
    if (!is.null(inp_ishod)) {
      data <- data %>%
        dplyr::filter(ishod_postupka %in% inp_ishod)
    }
    if (!is.null(inp_funkcija)) {
      data <- data %>%
        dplyr::filter(funkcija %in% inp_funkcija)
    }
    if (!is.null(inp_organ)) {
      data <- data %>%
        dplyr::filter(organ %in% inp_organ)
    }
    if (!is.null(inp_mesto)) {
      data <- data %>%
        dplyr::filter(mesto %in% inp_mesto)
    }
    
    return(data)
  })
  
  # + pretraga_tabela (DT) ----
  output$pretraga_tabela <- DT::renderDataTable(server = FALSE, {
    da <- get_filtered_data()
    
    da_tab <- da %>% dplyr::select(`Ime i prezime` = ime_prezime, 
                                   Funkcija = funkcija,
                                   Organ = organ,
                                   Mesto = mesto)
    DT_table_funkcioneri(da_tab)
  })
  # + kartica_postupka (UI) ----
  output$kartica_postupka <- renderUI({
    da <- get_filtered_data()
    
    shiny::validate(
      need(nrow(da) > 0, message = "Nema postupaka za izabrane parametre.")
    )
    
    id_sel <- input$pretraga_tabela_rows_selected
    
    shiny::validate(
      need(!is.null(id_sel), message = "Izaberi postupak.")
    )
    
    da_id <- da %>% dplyr::slice(id_sel)
    
    funkcioner                                 <- da_id[["ime_prezime"]]
    funkcija                                   <- da_id[["funkcija"]]
    organ                                      <- da_id[["organ"]]
    mesto                                      <- da_id[["mesto"]]
    zakonski_osnov                             <- da_id[["osnov"]]
    godina_pokretanja_postupka                 <- da_id[["godina_pok_post"]]
    mera_upozorenja                            <- da_id[["mera_upozorenja"]]
    mera_objavljivanja_razresenje              <- da_id[["preporuka_za_rezresenje"]]
    mera_objavljivanja_razresenje_komentar     <- da_id[["preporuka_za_rezresenje_komentar"]]
    mera_objavljivanja_povreda_zakona          <- da_id[["odluka_o_povredi_zakona"]]
    mera_objavljivanja_povreda_zakona_komentar <- da_id[["odluka_o_povredi_zakona_komentar"]]
    prijava                                    <- da_id[["prijava"]]
    godina_podnosenja                          <- da_id[["godina_podnosenja_prijave"]]
    godina_resen                               <- da_id[["godina_resenja_postupka"]]
    ishod_postupka                             <- da_id[["ishod_postupka"]]
    detalji                                    <- da_id[["detalji"]]
    izvestaji                                  <- da_id[["izvestaji"]]
    oportunitet_iznos                          <- da_id[["iznos_oportunitet"]]
    link_obustava                              <- da_id[["link_obustava"]]
    link_mera_upozorenja                       <- da_id[["link_mera_upozorenja"]]
    link_razresenje                            <- da_id[["link_razresenje"]]
    link_javna_opomena                         <- da_id[["link_javna_opomena"]]
    link_prekrsajna                            <- da_id[["link_prekrsajna"]]
    link_krivicna                              <- da_id[["link_krivicna"]]
    link_prek_kriv <- link_prekrsajna
    link_prek_kriv[is.na(link_prek_kriv)] <- link_krivicna[is.na(link_prek_kriv)]
    
    if (is.na(godina_pokretanja_postupka)) {
      ui_godina_pokretanja_postupka <- ""
    } else {
      ui_godina_pokretanja_postupka <- godina_pokretanja_postupka
    }
    
    if (is.na(godina_podnosenja)) {
      ui_godina_podnosenja <- ""
    } else {
      ui_godina_podnosenja <- godina_podnosenja
    }
    
    if (is.na(godina_resen)) {
      if (is.na(ishod_postupka) | ishod_postupka != "u toku") {
        ui_godina_resen <- ""
      } else {
        ui_godina_resen <- "U TOKU"
      }
    } else {
      ui_godina_resen <- godina_resen
    }
    
    # mera_upozorenja
    if (mera_upozorenja == "da") {
      ui_mera_upozorenja <- tags$h4(tags$span(icon("file-text-o"),
                                              style = "font-size: x-large"),
                                    tags$b("Mera upozorenja ", 
                                           icon("ok", lib = "glyphicon")),
                                    style = "display:inline-block;")
      
      ui_mera_upozorenja_link <- html_link_dokumenta(link_mera_upozorenja)
      
      if (is.na(link_mera_upozorenja)) {
        ui_mera_upozorenja_div <- tags$div(ui_mera_upozorenja)
      } else {
        ui_mera_upozorenja_div <- tags$div(ui_mera_upozorenja,
                                           ui_mera_upozorenja_link)
      }
      
    } else {
      ui_mera_upozorenja_div <- tags$h4("Mera upozorenja ",
                                        icon("remove", lib = "glyphicon"), 
                                        style = "color: #595959")
    }
    
    # mera_objavljivanja_razresenje
    if (mera_objavljivanja_razresenje == "da") {
      ui_mera_objavljivanja_razresenje <- tags$h4(tags$span(icon("file-text-o"),
                                                            style = "font-size: x-large"),
                                                  tags$b("Preporuka za razrešenje ", 
                                                         icon("ok", lib = "glyphicon")),
                                                  style = "display:inline-block;")
      
      ui_mera_objavljivanja_razresenje_link <- html_link_dokumenta(link_razresenje)
      
      if (is.na(link_razresenje)) {
        ui_mera_objavljivanja_razresenje_div <- tags$div(ui_mera_objavljivanja_razresenje)
      } else {
        ui_mera_objavljivanja_razresenje_div <- tags$div(ui_mera_objavljivanja_razresenje,
                                                         ui_mera_objavljivanja_razresenje_link)
      }
      
    } else {
      ui_mera_objavljivanja_razresenje_div <- tags$h4("Preporuka za razrešenje ", 
                                                      icon("remove", lib = "glyphicon"),
                                                      style = "color: #595959")
    }
    if (is.na(mera_objavljivanja_razresenje_komentar)) {
      ui_mera_objavljivanja_razresenje_komentar <- NULL
    } else {
      ui_mera_objavljivanja_razresenje_komentar <- tags$p(tags$li(mera_objavljivanja_razresenje_komentar, style = "font-size:large"))
    }
    
    # mera_objavljivanja_povreda_zakona
    if (mera_objavljivanja_povreda_zakona == "da") {
      ui_mera_objavljivanja_povreda_zakona <- tags$h4(tags$span(icon("file-text-o"),
                                                                style = "font-size: x-large"),
                                                      tags$b("Odluka o povredi zakona ", 
                                                             icon("ok", lib = "glyphicon")),
                                                      style = "display:inline-block;")
      
      ui_mera_objavljivanja_povreda_zakona_link <- html_link_dokumenta(link_javna_opomena)
      
      if (is.na(link_javna_opomena)) {
        ui_mera_objavljivanja_povreda_zakona_div <- tags$div(ui_mera_objavljivanja_povreda_zakona)
      } else {
        ui_mera_objavljivanja_povreda_zakona_div <- tags$div(ui_mera_objavljivanja_povreda_zakona,
                                                             ui_mera_objavljivanja_povreda_zakona_link)
      }
      
    } else {
      ui_mera_objavljivanja_povreda_zakona_div <- tags$h4("Odluka o povredi zakona ", 
                                                          icon("remove", lib = "glyphicon"),
                                                          style = "color: #595959")
    }
    if (is.na(mera_objavljivanja_povreda_zakona_komentar)) {
      ui_mera_objavljivanja_povreda_zakona_komentar <- NULL
    } else {
      ui_mera_objavljivanja_povreda_zakona_komentar <- tags$p(tags$li(mera_objavljivanja_povreda_zakona_komentar, style = "font-size:large"))
    }
    
    # detalji
    if (is.na(detalji)) {
      ui_detalji <- NULL
    } else {
      ui_detalji <- 
        tagList(
          tags$h4("Detalji", style = "color: #595959"),
          tags$p(detalji, style = "font-size:medium")
        )
    }
    
    # izvestaji
    if (is.na(izvestaji)) {
      ui_izvestaji <- NULL
    } else {
      ui_izvestaji <- 
        tagList(
          tags$h4("Izveštaj", style = "color: #595959"),
          tags$p(izvestaji, style = "font-size:medium")
        )
    }
    
    # prijava
    if (prijava == "Bez prijave") {
      ui_prijava <- 
        tagList(
          tags$h4(tags$span(icon("ban-circle", lib = "glyphicon"),
                            style = "font-size: x-large"),
                  tags$b(str_to_upper(prijava))),
          tags$hr()
        )
    } else {
      if (is.na(ishod_postupka)) {
        ishod <- ""
      } else {
        if (ishod_postupka == "oportunitet") {
          ishod <- paste0(ishod_postupka, " (", oportunitet_iznos, ")")
        } else {
          ishod <- ishod_postupka
        }
      }
        
      ui_prijava_wo_link <- 
        tagList(
          tags$h4(tags$span(icon("exclamation-triangle"),
                            style = "font-size: x-large"),
                  tags$b(str_to_upper(prijava))),
          
          tags$h4("Godina podnošenja prijave: ", tags$b(ui_godina_podnosenja), style = "color: #595959"),
          tags$h4("Godina kada je rešen postupak: ", tags$b(ui_godina_resen), style = "color: #595959"),
          tags$h4("Ishod postupka: ", tags$b(str_to_upper(ishod)), style = "color: #595959"),
          tags$hr()
        )
      
      ui_prijava_w_link <- 
        tagList(
          tags$h4(tags$span(icon("exclamation-triangle"),
                            style = "font-size: x-large"),
                  tags$b(str_to_upper(prijava)),
                  style = "display:inline-block;"),
          html_link_dokumenta(link_prek_kriv),
          
          tags$h4("Godina podnošenja prijave: ", tags$b(ui_godina_podnosenja), style = "color: #595959"),
          tags$h4("Godina kada je rešen postupak: ", tags$b(ui_godina_resen), style = "color: #595959"),
          tags$h4("Ishod postupka: ", tags$b(str_to_upper(ishod)), style = "color: #595959"),
          tags$hr()
        )
      
      if (is.na(link_prek_kriv)) {
        ui_prijava <- ui_prijava_wo_link
      } else {
        ui_prijava <- ui_prijava_w_link
      }
      
    }
    
    tagList(
      tags$div(style = "margin-right: 10px; margin-left: 10px;",
        tags$h1(tags$b(funkcioner), style = "color: #337ab7"),
        tags$h3(tags$b(str_to_upper(funkcija)), style = "color: #595959"),
        tags$h4(organ, paste0("(", mesto, ")"), style = "color: #595959"),
        tags$hr(),
        
        tags$h4("Zakonski osnov: ", tags$b(zakonski_osnov), style = "color: #595959"),
        tags$h4("Godina pokretanja postupka: ", tags$b(ui_godina_pokretanja_postupka), style = "color: #595959"),
        tags$hr(),
        
        ui_mera_upozorenja_div,
        ui_mera_objavljivanja_razresenje_div,
        ui_mera_objavljivanja_razresenje_komentar,
        ui_mera_objavljivanja_povreda_zakona_div,
        ui_mera_objavljivanja_povreda_zakona_komentar,
        tags$hr(),
        
        ui_prijava,
        
        ui_detalji,
        ui_izvestaji
      )
    )
    
  })
  
  # | ----
  # | Top 10 ----
  # + top_10_funkcionera (Plotly) ----
  output$top_10_funkcionera <- renderPlotly({
    p <- baza %>%
      dplyr::select(ime_prezime, mera_upozorenja, 
                    preporuka_za_rezresenje, 
                    odluka_o_povredi_zakona, prijava) %>%
      tidyr::gather(key, value, -ime_prezime) %>%
      dplyr::filter(value %in% c("da", "Prekršajna prijava", "Krivična prijava")) %>%
      dplyr::count(ime_prezime, sort = T) %>%
      dplyr::filter(n > 4) %>%
      dplyr::mutate(ime_prezime = fct_reorder(ime_prezime, n)) %>%
      dplyr::rename(Funkcioner = ime_prezime,
                    Postupaka = n) %>%
      ggplot(aes(x = Funkcioner, y = Postupaka)) +
      geom_col(fill = "#2166ac") +
      labs(x = NULL, y = "Broj postupaka", title = "Funkcioner") +
      coord_flip() + 
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + top_10_funkcija (Plotly) ----
  output$top_10_funkcija <- renderPlotly({
    p <- baza %>%
      dplyr::count(funkcija, sort = T) %>%
      dplyr::top_n(10, n) %>%
      dplyr::mutate(funkcija = fct_reorder(funkcija, n)) %>%
      dplyr::rename(Funkcija = funkcija,
                    Postupaka = n) %>%
      ggplot(aes(x = Funkcija, y = Postupaka)) +
      geom_col(fill = "#bf812d") +
      labs(x = NULL, y = "Broj postupaka", title = "Funkcija") +
      coord_flip() + 
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + top_10_organa (Plotly) ----
  output$top_10_organa <- renderPlotly({
    p <- baza %>%
      dplyr::filter(!is.na(organ)) %>%
      dplyr::count(organ, sort = T) %>%
      dplyr::top_n(10, n) %>%
      dplyr::mutate(organ = fct_reorder(organ, n)) %>%
      dplyr::rename(Organ = organ,
                    Postupaka = n) %>%
      ggplot(aes(x = Organ, y = Postupaka)) +
      geom_col(fill = "#fdae61") +
      labs(x = NULL, y = "Broj postupaka", title = "Organ") +
      coord_flip() + 
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + top_10_zakonski_osnov (Plotly) ----
  output$top_10_zakonski_osnov <- renderPlotly({
    p <- baza %>%
      dplyr::select(osnov) %>%
      tidyr::separate(osnov, into = paste0("O", 1:5), sep = ",") %>%
      tidyr::gather() %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(value = str_to_lower(str_trim(value))) %>%
      dplyr::count(value, sort = T) %>%
      dplyr::rename(`Zakonski osnov` = value,
                    Postupaka = n) %>%
      dplyr::mutate(`Zakonski osnov` = fct_reorder(`Zakonski osnov`, Postupaka)) %>%
      head(10) %>%
      ggplot(aes(x = `Zakonski osnov`, y = Postupaka)) +
      geom_col(fill = "#9970ab") +
      labs(x = NULL, y = "Broj postupaka", title = "Zakonski osnov") +
      coord_flip() + 
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + top_10_mesta (Plotly) ----
  output$top_10_mesta <- renderPlotly({
    p <- baza %>%
      dplyr::filter(!is.na(mesto)) %>%
      dplyr::count(mesto, sort = T) %>%
      dplyr::top_n(10, n) %>%
      dplyr::mutate(mesto = fct_reorder(mesto, n)) %>%
      dplyr::rename(Mesto = mesto,
                    Postupaka = n) %>%
      ggplot(aes(x = Mesto, y = Postupaka)) +
      geom_col(fill = "#66bd63") +
      labs(x = NULL, y = "Broj postupaka", title = "Mesto") +
      coord_flip() + 
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })

  # | ----
  # | Mere ----
  # + mere_upozorenja_po_godinama (Plotly) ----
  output$mere_upozorenja_po_godinama <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(godina_pok_post), 
                    godina_pok_post != "Informacija trenutno nije dostupna",
                    mera_upozorenja == "da") %>%
      dplyr::mutate(godina_pok_post = as.numeric(godina_pok_post)) %>%
      dplyr::count(godina_pok_post) %>%
      dplyr::rename(Godina = godina_pok_post,
                    Ukupno = n)
    p <- 
      ggplot(da, aes(x = Godina, y = Ukupno)) +
      geom_col(fill = "#08519c") + 
      labs(x = NULL, y = NULL, title = "Mera upozorenja") +
      scale_x_continuous(labels = as.character(da$Godina), breaks = da$Godina) + 
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + preporuka_za_rezresenje_po_godinama (Plotly) ----
  output$preporuka_za_rezresenje_po_godinama <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(godina_pok_post), 
                    godina_pok_post != "Informacija trenutno nije dostupna",
                    preporuka_za_rezresenje == "da") %>%
      dplyr::mutate(godina_pok_post = as.numeric(godina_pok_post)) %>%
      dplyr::count(godina_pok_post) %>%
      dplyr::rename(Godina = godina_pok_post,
                    Ukupno = n)
    p <- 
      ggplot(da, aes(x = Godina, y = Ukupno)) +
      geom_col(fill = "#1b7837") +
      labs(x = NULL, y = NULL, title = "Preporuka za rezrešenje") +
      scale_x_continuous(labels = as.character(da$Godina), 
                         breaks = da$Godina) + 
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + odluka_o_povredi_zakona_po_godinama (Plotly) ----
  output$odluka_o_povredi_zakona_po_godinama <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(godina_pok_post), 
                    godina_pok_post != "Informacija trenutno nije dostupna",
                    odluka_o_povredi_zakona == "da") %>%
      dplyr::mutate(godina_pok_post = as.numeric(godina_pok_post)) %>%
      dplyr::count(godina_pok_post) %>%
      dplyr::rename(Godina = godina_pok_post,
                    Ukupno = n)
    p <- 
      ggplot(da, aes(x = Godina, y = Ukupno)) +
      geom_col(fill = "#f46d43") +
      labs(x = NULL, y = NULL, title = "Odluka o povredi zakona") +
      scale_x_continuous(labels = as.character(da$Godina), 
                         breaks = da$Godina) + 
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + mera_upozorenja_funkcija (Plotly) ----
  output$mera_upozorenja_funkcija <- renderPlotly({
    da <- baza %>%
      dplyr::filter(mera_upozorenja == "da") %>%
      dplyr::count(funkcija, sort = T) %>%
      dplyr::rename(Funkcija = funkcija,
                    Ukupno = n) %>%
      dplyr::mutate(Funkcija = fct_reorder(Funkcija, Ukupno)) %>%
      head(10)
    
    p <- 
      ggplot(da, aes(x = Funkcija, y = Ukupno)) +
      geom_col(fill = "#08519c") +
      labs(x = NULL, y = NULL, title = "Mera upozorenja po funkciji") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + preporuka_za_rezresenje_funkcija (Plot) ----
  output$preporuka_za_rezresenje_funkcija <- renderPlotly({
    da <- baza %>%
      dplyr::filter(preporuka_za_rezresenje == "da") %>%
      dplyr::count(funkcija, sort = T) %>%
      dplyr::rename(Funkcija = funkcija,
                    Ukupno = n) %>%
      dplyr::mutate(Funkcija = fct_reorder(Funkcija, Ukupno)) %>%
      head(10)
    
    p <- 
      ggplot(da, aes(x = Funkcija, y = Ukupno)) +
      geom_col(fill = "#1b7837") +
      labs(x = NULL, y = NULL, title = "Preporuka za razrešenje po funkciji") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # + odluka_o_povredi_zakona_funkcija (Plotly) ----
  output$odluka_o_povredi_zakona_funkcija <- renderPlotly({
    da <- baza %>%
      dplyr::filter(odluka_o_povredi_zakona == "da") %>%
      dplyr::count(funkcija, sort = T) %>%
      dplyr::rename(Funkcija = funkcija,
                    Ukupno = n) %>%
      dplyr::mutate(Funkcija = fct_reorder(Funkcija, Ukupno)) %>%
      head(10)
    
    p <- 
      ggplot(da, aes(x = Funkcija, y = Ukupno)) +
      geom_col(fill = "#f46d43") +
      labs(x = NULL, y = NULL, title = "Odluka o povredi zakona po funkciji") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  # | ----
  # | Prijave ----
  # + krivicne_po_godinama (Plotly) ----
  output$krivicne_po_godinama <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(godina_podnosenja_prijave), 
                    godina_podnosenja_prijave != "Informacija trenutno nije dostupna",
                    prijava == "Krivična prijava") %>%
      dplyr::mutate(godina_podnosenja_prijave = as.numeric(godina_podnosenja_prijave)) %>%
      dplyr::count(godina_podnosenja_prijave) %>%
      dplyr::rename(Godina = godina_podnosenja_prijave,
                    Ukupno = n)
    p <- 
      ggplot(da, aes(x = Godina, y = Ukupno)) +
      geom_col(fill = "#de2d26") + 
      labs(x = NULL, y = NULL, title = "Krivične prijave") +
      scale_x_continuous(labels = as.character(da$Godina), breaks = da$Godina) + 
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  
  # + krivicne_funkcija_organ (Plot) ----
  output$krivicne_funkcija_organ <- renderPlotly({
    da <- baza %>%
      dplyr::filter(prijava == "Krivična prijava") %>%
      dplyr::count(funkcija, sort = T) %>%
      dplyr::rename(Funkcija = funkcija,
                    Ukupno = n) %>%
      dplyr::mutate(Funkcija = fct_reorder(Funkcija, Ukupno)) %>%
      head(10)
    
    p <- 
    ggplot(da, aes(x = Funkcija, y = Ukupno)) +
      geom_col(fill = "#de2d26") + 
      labs(x = NULL, y = NULL, title = "Krivične prijave po funkciji") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  
  # + krivicne_ishod_postupka (Plotly) ----
  output$krivicne_ishod_postupka <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(ishod_postupka),
                    prijava == "Krivična prijava") %>%
      dplyr::count(ishod_postupka, sort = T) %>%
      dplyr::rename(Ishod = ishod_postupka,
                    Ukupno = n) %>%
      dplyr::mutate(Ishod = fct_reorder(Ishod, Ukupno))
    
    p <- 
      ggplot(da, aes(x = Ishod, y = Ukupno)) +
      geom_col(fill = "#de2d26") + 
      labs(x = NULL, y = NULL, title = "Ishodi krivičnih prijava") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  
  # + prekrsajne_po_godinama (Plotly) ----
  output$prekrsajne_po_godinama <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(godina_podnosenja_prijave), 
                    godina_podnosenja_prijave != "Informacija trenutno nije dostupna",
                    prijava == "Prekršajna prijava") %>%
      dplyr::mutate(godina_podnosenja_prijave = as.numeric(godina_podnosenja_prijave)) %>%
      dplyr::count(godina_podnosenja_prijave) %>%
      dplyr::rename(Godina = godina_podnosenja_prijave,
                    Ukupno = n)
    p <- 
      ggplot(da, aes(x = Godina, y = Ukupno)) +
      geom_col(fill = "#fc9272") + 
      labs(x = NULL, y = NULL, title = "Prekršajne prijave") +
      scale_x_continuous(labels = as.character(da$Godina), breaks = da$Godina) + 
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  
  # + prekrsajne_funkcija_organ (Plot) ----
  output$prekrsajne_funkcija_organ <- renderPlotly({
    da <- baza %>%
      dplyr::filter(prijava == "Prekršajna prijava") %>%
      dplyr::count(funkcija, sort = T) %>%
      dplyr::rename(Funkcija = funkcija,
                    Ukupno = n) %>%
      dplyr::mutate(Funkcija = fct_reorder(Funkcija, Ukupno)) %>%
      head(10)
    
    p <- 
    ggplot(da, aes(x = Funkcija, y = Ukupno)) +
      geom_col(fill = "#fc9272") + 
      labs(x = NULL, y = NULL, title = "Prekršajne prijave po funkciji") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  
  # + prekrsajne_ishod_postupka (Plotly) ----
  output$prekrsajne_ishod_postupka <- renderPlotly({
    da <- baza %>%
      dplyr::filter(!is.na(ishod_postupka),
                    prijava == "Prekršajna prijava") %>%
      dplyr::count(ishod_postupka, sort = T) %>%
      dplyr::rename(Ishod = ishod_postupka,
                    Ukupno = n) %>%
      dplyr::mutate(Ishod = fct_reorder(Ishod, Ukupno))
    
    p <- 
      ggplot(da, aes(x = Ishod, y = Ukupno)) +
      geom_col(fill = "#fc9272") + 
      labs(x = NULL, y = NULL, title = "Ishodi prekršajnih prijava") +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank())
    
    pl <- ggplotly(p) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(yaxis=list(fixedrange = TRUE),
             xaxis=list(fixedrange = TRUE))
    
    pl$elementId <- NULL
    pl <- plotly::config(pl, 
                         staticPlot = F, 
                         collaborate = F, 
                         doubleClick = F, 
                         displayModeBar = F)
    return(pl)
  })
  
  # | ----
  # | Dijagram postupaka ----
  # + dijagram_postupaka_zo_p (sankeyNetwork) ----
  output$dijagram_postupaka_zo_p <- renderSankeyNetwork({
    sankey_df <- baza %>%
      dplyr::select(osnov,
                    prijava,
                    ishod_postupka) %>%
      dplyr::filter(!is.na(osnov))
    
    trans1_2 <- sankey_df %>% 
      group_by(osnov, prijava) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(20, ukupno)
    
    trans2_3 <- sankey_df %>% 
      group_by(prijava, ishod_postupka) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(15, ukupno)
    
    colnames(trans1_2)[1:2] <- colnames(trans2_3)[1:2] <- c("source","target")
    
    links <- rbind(as.data.frame(trans1_2), 
                   as.data.frame(trans2_3))
    
    links$source <- str_to_upper(links$source)
    links$target <- str_to_upper(links$target)
    
    nodes <- data.frame(name=unique(c(links$source, links$target)))
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1
    
   sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "ukupno", NodeID = "name",
                  fontSize = 11, 
                  nodeWidth = 15,
                  colourScale = JS("d3.scaleOrdinal(d3.schemeDark2);"))
  })
  
  # + dijagram_postupaka_fu_p (sankeyNetwork) ----
  output$dijagram_postupaka_fu_p <- renderSankeyNetwork({
    sankey_df <- baza %>%
      dplyr::select(funkcija,
                    prijava,
                    ishod_postupka) %>%
      dplyr::filter(!is.na(funkcija),
                    !is.na(prijava))
    
    trans1_2 <- sankey_df %>% 
      group_by(funkcija, prijava) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(50, ukupno)
    
    trans2_3 <- sankey_df %>% 
      group_by(prijava, ishod_postupka) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(50, ukupno)
    
    colnames(trans1_2)[1:2] <- colnames(trans2_3)[1:2] <- c("source","target")
    
    links <- rbind(as.data.frame(trans1_2), 
                   as.data.frame(trans2_3))
    
    links$source <- str_to_upper(links$source)
    links$target <- str_to_upper(links$target)
    
    nodes <- data.frame(name=unique(c(links$source, links$target)))
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1

    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "ukupno", NodeID = "name",
                  fontSize = 11, 
                  nodeWidth = 15, iterations = 32,
                  colourScale = JS("d3.scaleOrdinal(d3.schemeDark2);"))
  })
  
  # + dijagram_postupaka_zo_m_p (sankeyNetwork) ----
  output$dijagram_postupaka_zo_m_p <- renderSankeyNetwork({
    sankey_df <- baza %>%
      dplyr::select(osnov,
                    `Mera upozorenja` = mera_upozorenja,
                    `Preporuka za razrešenje` = preporuka_za_rezresenje,
                    `Odluka o povredi zakona` = odluka_o_povredi_zakona,
                    prijava,
                    ishod_postupka) %>%
      tidyr::gather(mera, value, -c(osnov, prijava, ishod_postupka)) %>%
      dplyr::filter(value == "da") %>%
      dplyr::select(-value)
    
    trans1_2 <- sankey_df %>% 
      group_by(osnov, mera) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(20, ukupno)
    
    trans2_3 <- sankey_df %>% 
      group_by(mera, prijava) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(15, ukupno)
    
    trans3_4 <- sankey_df %>% 
      group_by(prijava, ishod_postupka) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(50, ukupno)
    
    colnames(trans1_2)[1:2] <- colnames(trans2_3)[1:2] <- colnames(trans3_4)[1:2] <- c("source","target")
    
    links <- rbind(as.data.frame(trans1_2), 
                   as.data.frame(trans2_3), 
                   as.data.frame(trans3_4))
    
    links$source <- str_to_upper(links$source)
    links$target <- str_to_upper(links$target)
    
    nodes <- data.frame(name=unique(c(links$source, links$target)))
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "ukupno", NodeID = "name",
                  fontSize = 11, 
                  nodeWidth = 15,
                  colourScale = JS("d3.scaleOrdinal(d3.schemeDark2);"))
  })
  
  # + dijagram_postupaka_fu_m_p (sankeyNetwork) ----
  output$dijagram_postupaka_fu_m_p <- renderSankeyNetwork({
    sankey_df <- baza %>%
      dplyr::select(funkcija,
                    `Mera upozorenja` = mera_upozorenja,
                    `Preporuka za razrešenje` = preporuka_za_rezresenje,
                    `Odluka o povredi zakona` = odluka_o_povredi_zakona,
                    prijava,
                    ishod_postupka) %>%
      tidyr::gather(mera, value, -c(funkcija, prijava, ishod_postupka)) %>%
      dplyr::filter(value == "da") %>%
      dplyr::select(-value)
    
    trans1_2 <- sankey_df %>% 
      group_by(funkcija, mera) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(50, ukupno)
    
    trans2_3 <- sankey_df %>% 
      group_by(mera, prijava) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(15, ukupno)
    
    trans3_4 <- sankey_df %>% 
      group_by(prijava, ishod_postupka) %>% 
      summarise(ukupno = n()) %>% 
      ungroup() %>%
      top_n(50, ukupno)
    
    colnames(trans1_2)[1:2] <- colnames(trans2_3)[1:2] <- colnames(trans3_4)[1:2] <- c("source","target")
    
    links <- rbind(as.data.frame(trans1_2), 
                   as.data.frame(trans2_3), 
                   as.data.frame(trans3_4))
    
    links$source <- str_to_upper(links$source)
    links$target <- str_to_upper(links$target)
    
    nodes <- data.frame(name=unique(c(links$source, links$target)))
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "ukupno", NodeID = "name",
                  fontSize = 11, 
                  nodeWidth = 15,
                  colourScale = JS("d3.scaleOrdinal(d3.schemeDark2);"))
  })
  
  # + dijagram_postupaka_pr_po_re (sankeyNetwork) ----
  output$dijagram_postupaka_pr_po_re <- renderSankeyNetwork({
    sankey_df <- baza %>%
      dplyr::select(godina_pok_post,
                    godina_podnosenja_prijave,
                    godina_resenja_postupka) %>%
      dplyr::mutate(godina_pok_post = paste0("Godina pokretanja postupka: ", godina_pok_post),
                    godina_podnosenja_prijave = paste0("Godina podnosenja prijave: ", godina_podnosenja_prijave),
                    godina_resenja_postupka = paste0("Godina kada je resen postupak: ", godina_resenja_postupka))
    
    trans1_2 <- sankey_df %>% 
      group_by(godina_pok_post, godina_podnosenja_prijave) %>% 
      summarise(ukupno = n()) %>% 
      ungroup()
    
    trans2_3 <- sankey_df %>% 
      group_by(godina_podnosenja_prijave, godina_resenja_postupka) %>% 
      summarise(ukupno = n()) %>% 
      ungroup()

    colnames(trans1_2)[1:2] <- colnames(trans2_3)[1:2] <- c("source","target")
    
    links <- rbind(as.data.frame(trans1_2), 
                   as.data.frame(trans2_3))
    
    links$source <- str_to_upper(links$source)
    links$target <- str_to_upper(links$target)
    
    nodes <- data.frame(name=unique(c(links$source, links$target)))
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "ukupno", NodeID = "name",
                  fontSize = 11, 
                  nodeWidth = 15, iterations = 0,
                  colourScale = JS("d3.scaleOrdinal(d3.schemeDark2);"))
  })
  
  # | ----
  # | O projektu ----
  output$get_o_projektu_txt <- renderUI({
    o_projektu_text <- get_o_projektu_text()
    
    return(o_projektu_text)
  })
  
  
}