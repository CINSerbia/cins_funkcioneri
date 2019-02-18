get_prvi_deo_text <- function() {
  p1 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Baza „Postupci protiv funkcionera” sadrži informacije o skoro 2.800 postupaka koje
je od 2010. do novembra 2018. godine Agencija za borbu protiv korupcije pokrenula
protiv funkcionera koji nisu prijavljivali imovinu na vreme, nisu dostavljali tačne
podatke o istoj ili su se našli u sukobu interesa.")
  
  p2 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Među više od 1.700 funkcionera u bazi su se našli narodni poslanici, ministri,
državni sekretari, ambasadori, gradonačelnici, lokalni odbornici, direktori javnih
preduzeća, dekani i drugi.")
  
  p3 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Prikupljanje i obrada informacija trajala je šest meseci, za šta su korišćeni sledeći
izvori: Agencija za borbu protiv korupcije, tužilaštva i sudovi na teritoriji Srbije,
lokalne samouprave, javna preduzeća i druge institucije.")
  
  p4 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Baza se koristi jednostavnom pretragom prema imenu i prezimenu funkcionera,
funkciji koju je obavljao/la u trenutku navodnog kršenja zakona, organu odnosno
radnom mestu i njegovoj lokaciji, kao i prema vrsti postupka koji je pokrenut.")
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h1("Postupci protiv funkcionera"),
             p1, p2, p3, p4)
    )
  )
}

get_drugi_deo_text <- function() {
  
  p5 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Zbog povreda Zakona o Agenciji za borbu protiv korupcije, funkcioneri su
kažnjavani merama upozorenja, preporukama za razrešenje, merama javnog
objavljivanja odluke o povredi zakona. Zbog težih prekršaja, Agencija je pokretala
krivične i prekršajne postupke.")
  
  p51 <- tags$p(style = "font-size:large; style = color: #595959", 
                "Do objavljivanja baze CINS nije dobio sve podatke o ishodima pokrenutih postupaka
pa će informacije koje nedostaju, uključujući kopije dokumenata, naknadno biti
unete.")
  
  p6 <- tags$p(style = "font-size:large; style = color: #595959", 
               tags$li(style = "font-size:large; style = color: #595959", 
                       tags$b("Mera upozorenja"), " – najblaža mera koju Agencija može izreći funkcioneru.
Funkcioner ima pravo da se na nju izjasni, a ukoliko to ne uradi u propisanom
roku, Agencija protiv njega može podneti:"),
               tags$li(style = "font-size:large; style = color: #595959", 
                       tags$b("Preporuku za razrešenje"), " – inicijativa za razrešenje funkcionera organu koji ga je postavio na funkciju."),
               tags$li(style = "font-size:large; style = color: #595959", 
                       tags$b("Odluku o povredi Zakona (Meru javnog objavljivanja odluke o povredi Zakona)"), " – javno se objavljuje na sajtu Agencije i u Službenom glasniku Republike Srbije. U ovoj odluci je opisan način na koji je zakon prekršen.")
  )
  
  p7 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Za težu povredu Zakona Agencija podnosi:",
               tags$li(style = "font-size:large; style = color: #595959", 
                       tags$b("Prekršajnu prijavu"), " – traži da se funkcioner kazni novčanom kaznom u iznosu od 10.000 – 2.000.000 dinara"),
               tags$li(style = "font-size:large; style = color: #595959", 
                       tags$b("Krivičnu prijavu"), " – podnosi za najteže slučajeve neprijavljivanja imovine ili davanja lažnih podataka o imovini. Ukoliko sud osudi funkcionera kaznom zatvora, funkcioneru će biti zabranjeno da se bavi javnom funkcijom narednih 10 godina.")
  )
  
  tagList(
    fluidRow(
      column(width = 12, p5, p51, p6, p7)
    )
  )
}

get_o_projektu_text <- function() {
  p1 <- tags$p(style = "font-size:large; style = color: #595959",
               '„Postupci protiv funkcionera” je jedinstvena baza podataka', tags$a('Centra za istraživačko novinarstvo Srbije', 
                                                                                    href = "https://www.cins.rs/", 
                                                                                    target = "_blank"),' (CINS)  koja daje građanima mogućnost da se na jednom mestu informišu o postupcima koje je državna Agencija za borbu protiv korupcije pokrenula protiv funkcionera u periodu od 2010. do novembra 2018. godine.')
  
  p2 <- tags$p(style = "font-size:large; style = color: #595959", 
               "CINS je prvi put objavio bazu postupaka protiv funkcionera 2016. godine, a ovaj projekat je nastavak rada na prikupljanju i ažuriranju podataka, uz nove opcije pretrage i informativne grafike.")
  
  p3 <- tags$p(style = "font-size:large; style = color: #595959", 
               "Ova baza podataka finansirana je grantom američkog Stejt departmenta. Mišljenja, nalazi i zaključci navedeni ovde pripadaju autorima i ne oslikavaju nužno stavove Stejt departmenta.")
  
  p31 <- tags$p(style = "font-size:large; style = color: #595959",
                "Kod koji smo koristili prilikom kreiranja baze je open source i možete ga pronaći na sledećoj adresi: ", 
                tags$a("GitHub/CINS", 
                       href = "https://github.com/CINSerbia/cins_funkcioneri",
                       target = "_blank"))
  
  p41 <- tags$p(style = "font-size:large; style = color: #595959", 
                tags$br(tags$b("Autori baze")), 
                "Anđela Milivojević, Bojana Bosanac, Vladimir Kostić")
  
  p42 <- tags$p(style = "font-size:large; style = color: #595959", 
                tags$br(tags$b("Stažistkinje")), 
                "Teodora Ćurčić, Jovana Tomić")
  
  
  p5 <- tags$div(align = "left",
                 tags$img(src = "dep_of_state_logo_200.png", 
                          alt = "U.S. Department of State", width = "150", height = "150"))
  
  p6 <- tags$div(align = "left",
                 tags$img(src = "cins_logo_200.png", 
                          alt = "CINS", width = "200"))
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$div(style = "margin-right: 10px; margin-left: 10px;",
               tags$h1("O projektu"),
               tags$br(),
               p1, tags$br(),
               p2, tags$br(), 
               p3, tags$br(),
               p31, tags$br(),
               p41, 
               p42, 
               tags$br(), 
               tags$br(),
               fluidRow(
                 column(width = 4, p5),
                 column(width = 5, tags$br(), tags$br(), p6)
               ),
               tags$br(),
               tags$hr()
             )
      )
    )
  )
}

DT_table_funkcioneri <- function(data) {
  DT::datatable(data,
                selection = list(mode = 'single', 
                                 selected = c(1), 
                                 target = 'row'),
                rownames = FALSE,
                style = "bootstrap",
                options = list(
                  autoWidth = TRUE,
                  searching = FALSE,
                  pageLength = 8,
                  dom = 'tip',
                  scrollX = TRUE,
                  language = list(
                    info = "Prikazuje _START_ do _END_ od _TOTAL_ postupaka",
                    infoEmpty = "",
                    paginate = list(previous = 'Prethodna', `next` = 'Sledeća'),
                    emptyTable = "Nema podataka"
                  ))
  )
}

html_link_dokumenta <- function(link) {
  tags$a(tags$span(icon("file-pdf"), "Dokument",
                   style = "font-size: large"),
         href = link,
         target = "_blank",
         style = "clear:both;float:right;margin-top: 10px;")
}