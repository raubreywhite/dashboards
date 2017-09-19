library(shiny)
library(shiny.router)

options(shiny.maxRequestSize=30*1024^2) 

# This creates UI for each page.
pageHome <- function(input, output, session) {
  div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://rawgit.com/raubreywhite/css/master/shiny.css"),
      tags$link(href='http://fonts.googleapis.com/css?family=Montserrat:400,700',rel='stylesheet',type='text/css'),
      tags$link(href='http://fonts.googleapis.com/css?family=Cardo:400,700',rel='stylesheet',type='text/css')
    ),
    div(class="container",
        div(class="txt-center",
            div(class="column12",
                h1("Data sluse")
            ),
            div(class="column4 prefix4",
                hr()
            )
        ),
        div(class="container column12",
            div(class="sidepadding",
                div(
                  div(class="txt-center",
                    textOutput("eventTimeRemaining")
                    ),
                  br(),
                  br(),
                  selectInput("project","Prosjekt",choices=list("TB Rapport"="reporttb")),
                  h2("ONLY UPLOAD .XLSX FILES. EXCEL SIKKERSONE ONLY GIVES .XLS FILES."),
                  h2("FILE MUST BE TAKEN OUT OF SIKKERSONE, OPENED IN EXCEL, AND SAVED AS .XLSX"),
                  fileInput("myFile", "Velg fil")
                  
                )
              )
          )
      )
  )
}

# Plug router into Shiny server.
server <- shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) 
  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path("/data_in/reporttb/", paste0("TB_Richard_",format.Date(Sys.time(),format="%Y_%M_%d"),".xlsx")) )
  })
  #10-17

  
  output$eventTimeRemaining <- renderText({
    invalidateLater(60000, session)
    
    hours <- 10:17
    times <- c()
    for(i in hours){
      newTime <- as.POSIXct(sprintf("%s %d:05:00 UTC",Sys.Date(),i-1))
      times <- c(times,newTime)
      newTime <- as.POSIXct(sprintf("%s %d:05:00 UTC",Sys.Date()+1,i-1))
      times <- c(times,newTime)
    }
    diffs <- as.numeric(difftime(Sys.time(),as.POSIXct(times,origin="1970-01-01"),"mins"))
    identify <- which(diffs==max(diffs[diffs<0]))
    timeLeft <- abs(round(diffs[identify]))
    time <- sprintf("%d:05",hours[floor(identify/2)])
    sprintf("NOTE: The next processing batch will run in %d minutes at %s",timeLeft,time)
  })
  
  router(input, output)
})

# Both sample pages.
root_page <- pageHome()


# Creates router. We provide routing path and UI for this page.
router <- make_router(
  route("/s/statistics_course_1/index", root_page)
)

# Creat output for our router in main UI of Shiny app.
ui <- shinyUI(fluidPage(
  router_ui()
))


# Run server in a standard way.
shinyApp(ui, server)