library(shiny)

artykuly <- read.csv("artykuly.txt")
artykuly$data <- strptime(artykuly$data, "%Y-%m-%d")
podsumowanie <- read.csv("podsumowanie.txt")
podsumowanie$data <- strptime(podsumowanie$data, "%Y-%m-%d")

shinyUI(fluidPage(
  titlePanel("Wybory"),
  sidebarLayout(
    sidebarPanel(
      helpText("Przefiltruj dane, które chcesz przeanalizować."),
      
      dateRangeInput("data", label = "Podaj zakres dat:",
                     format="yyyy-mm-dd", separator = "-",
                     start = min(strptime(podsumowanie$data, "%Y-%m-%d")),
                     end = max(strptime(podsumowanie$data, "%Y-%m-%d"))),
      
      checkboxGroupInput("Portal",
                  "Wybierz portal",
                  c("Interia" = "interia",
                    "NaTemat (główna)" = "NaTematGlowna",
                    "NaTemat (wybory)" = "NaTematWybory",
                    "Onet (główna)" = "OnetGlowna",
                    "Onet (wybory)" = "OnetWybory",
                    "TVN 24" = "tvn24",
                    "Wirtualna Polska" = "wp",
                    "W Polityce" = "wPolityceGlowna",
                    "W Sieci" = "wsieci"),
                  c("Interia" = "interia",
                    "NaTemat (główna)" = "NaTematGlowna",
                    "NaTemat (wybory)" = "NaTematWybory",
                    "Onet (główna)" = "OnetGlowna",
                    "Onet (wybory)" = "OnetWybory",
                    "TVN 24" = "tvn24",
                    "Wirtualna Polska" = "wp",
                    "W Polityce" = "wPolityceGlowna",
                    "W Sieci" = "wsieci")),
      
      checkboxGroupInput("Kandydat",
                  "Wybierz kandydata",
                  c("Grzegorz Braun" = "braun",
                    "Andrzej Duda" = "duda",
                    "Adam Jarubas" = "jarubas",
                    "Bronisław Komorowski" = "komorowski",
                    "Janusz Korwin-Mikke" = "korwin",
                    "Marian Kowalski" = "kowalski",
                    "Paweł Kukiz" = "kukiz",
                    "Magdalena Ogórek" = "ogorek",
                    "Janusz Palikot" = "palikot",
                    "Jacek Wilk" = "wilk"),
                  selected="korwin"),
      
      radioButtons("waga_co", "Waga - co pokazać?",
                   c("sumaryczna waga", "średnia waga"),
                   "sumaryczna waga"),
      
      radioButtons("naglowki_co", "Nagłówki - co pokazać?",
                   c("ile razy w tytule", "ile razy w tytule/ilość tytułów"),
                   "ile razy w tytule"),
      
      radioButtons("tresc_co", "Treść - co pokazać?",
                   c("ile razy w sumie nazwisko w artykule", 
                     "ile razy średnio nazwisko w artykule",
                     "sumaryczna dlugość artykułów",
                     "średnia długość artykułów"),
                   "ile razy w sumie nazwisko w artykule"),
      
      radioButtons("typ", "Wybierz typ wykresu",
                   c("słupkowy", "liniowy (sensowny tylko dla jednego kandydata!)"),
                   "słupkowy"),
      
     actionButton("goButton", "Aktualizuj chmurę")

    ),
    
    mainPanel(
      h1("Podsumowanie"),
      tabsetPanel(
        tabPanel("Art. Wagi",
                 plotOutput("suma_wag_plot", width = 800),
                 tableOutput("suma_wag_table"),
                 plotOutput("heatMap_sum", width=800)),
        tabPanel("Art. Nagłówki",
                 plotOutput("naglowki_plot", width = 800),
                 tableOutput("naglowki_table")),
        tabPanel("Art. Treść",
                 plotOutput("tresc_plot", width = 800),
                 tableOutput("tresc_table")),
        #tabPanel("Podsumowanie", verbatimTextOutput("podsumowanie")),
        tabPanel("Art. Chmura", 
                 plotOutput("chmura", width = 800),
                 tableOutput("czestosc")),
        tabPanel("Facebook", 
                 plotOutput("fb_lajki", width=800),
                 plotOutput("fb_kto_kogo1", width=800),
                 tableOutput("fb_kto_kogo2"),
                 plotOutput("fb_chmura", width=800, height=1000),
                 tableOutput("fb_czestosc")),
        tabPanel("Twitter",
                 plotOutput("twitter_mapa", width=800),
                 plotOutput("twitter_mapa_world", width=800)),
        tabPanel("Art. Surowe dane", tableOutput("tabelaPods"), tableOutput("tabelaArt"))
      )

    )
  )
))
