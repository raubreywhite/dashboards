library(shiny)
library(ggvis)
library(shinydashboard)


dashboardPage(
  dashboardHeader(title = "MSIS"),
  dashboardSidebar(sidebarMenu(id = "tabs",
   menuItem("Signal ved årsak", tabName = "barometer", icon = icon("th")),
   menuItem("Signal ved geografi", tabName = "compareBar", icon = icon("th")),
   menuItem("Signal antall", tabName = "lines", icon = icon("line-chart")),
   menuItem("Ved registrering", tabName = "regtable", icon = icon("table")),
   menuItem("Fargeblind", tabName = "colourBlindTab", icon = icon("eye-slash"),
            menuSubItem(icon=NULL,
                        checkboxInput("colourBlind",label="Bytt farger",width="95%")))
   )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
tabItems(
  tabItem(tabName = "barometer",
          fluidRow(column(9,
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "primary",
                            ggvisOutput("barometerPlot")),
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "info",
                            p("Denne tabellen er en oversikt over fylkene eller kommunene i et fylke. Grønn farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er som forventet. En gul farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet.En rød farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet.")
                          )
          ),
          column(3,
                 box(
                   title = "Innstillinger", width = NULL, solidHeader = FALSE, status = "warning",
                   uiOutput("barometerLocationSelector"),
                   uiOutput("barometerTypeSelector"),
                   uiOutput("barometerGroupSelector"),
                   uiOutput("barometerWarningSelector"),
                   uiOutput("barometerDiseaseSelector"),
                   uiOutput("barometerWeekSelector")
                 )))),
  tabItem(tabName = "compareBar",
          fluidRow(column(9,
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "primary",
                            ggvisOutput("compareBarPlot")),
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "info",
                            p("Denne tabellen er en oversikt over fylkene eller kommunene i et fylke. Grønn farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er som forventet. En gul farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet.En rød farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet.")
                          )
          ),
          column(3,
                 box(
                   title = "Innstillinger", width = NULL, solidHeader = FALSE, status = "warning",
                   uiOutput("compareBarTypeSelector"),
                   uiOutput("compareBarGroupSelector"),
                   uiOutput("compareBarDiseaseSelector"),
                   uiOutput("compareBarWeekSelector")
                 )))),
  tabItem(tabName = "lines",
          fluidRow(column(9,
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "primary",
                            ggvisOutput("linesPlot")),
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "info",
                            p("Denne tabellen er en oversikt over fylkene eller kommunene i et fylke. Grønn farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er som forventet. En gul farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet.En rød farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet.")
                          )
          ),
          column(3,
                 box(
                   title = "Innstillinger", width = NULL, solidHeader = FALSE, status = "warning",
                   uiOutput("linesLocationSelector"),
                   uiOutput("linesTypeSelector"),
                   uiOutput("linesDiseaseSelector"),
                   uiOutput("linesSeasonSelector")
                 )))),
  tabItem(tabName = "regtable",
          fluidRow(column(9,
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "primary",
                            DT::dataTableOutput("regtableTable")),
                          box(
                            title = NULL, width = NULL, solidHeader = FALSE, status = "info",
                            p("Denne tabellen er en oversikt over fylkene eller kommunene i et fylke. Grønn farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er som forventet. En gul farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet.En rød farge betyr at 
                          antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet.")
                          )
          ),
          column(3,
                 box(
                   title = "Innstillinger", width = NULL, solidHeader = FALSE, status = "warning",
                   uiOutput("regtableLocationSelector"),
                   uiOutput("regtableTypeSelector"),
                   uiOutput("regtableGroupSelector"),
                   uiOutput("regtableDiseaseSelector"),
                   uiOutput("regtableWeekSelector")
                 ))))
)))

