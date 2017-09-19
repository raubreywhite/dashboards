library(shiny)
library(shiny.router)
library(shinyBS)
library(shinyjs)
library(data.table)

# This creates UI for each page.
pageHome <- function() {
  div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://rawgit.com/raubreywhite/css/master/shiny.css"),
      tags$link(href='http://fonts.googleapis.com/css?family=Montserrat:400,700',rel='stylesheet',type='text/css'),
      tags$link(href='http://fonts.googleapis.com/css?family=Cardo:400,700',rel='stylesheet',type='text/css')
    ),
    div(class="container",
      div(class="txt-center",
        div(class="column12",
          h1("Infeksjonsepidemiologi og modellering")
        ),
        div(class="column12 nav nav-small",
          a("Hjem",href="/s/smhb/index",class="cl-teal"),
          a("Advisory service",href="/s/smhb/rad"),
          a("NorMOMO",href="../normomo/",target="_blank"),
          a("Sykdomspuls",href="../sykdomspuls/",target="_blank"),
          a("TB Rapport",href="../reporttb/",target="_blank")
        )
      ),
      div(class="container",
        div(class="column10 prefix1 txt-center",
          br(),
          h2("Velkommen til SMHB"),
          plotOutput("plot")   
        )
      )
    )
  )
}

# This creates UI for each page.
pageApply <- function() {
  div(class="container",
    div(class="txt-center",
      div(class="column12",
        h1("Infeksjonsepidemiologi og modellering")
      ),
      div(class="column12 nav nav-small",
        a("Hjem",href="/s/smhb/index"),
        a("Advisory service",href="/s/smhb/rad",class="cl-teal"),
        a("NorMOMO",href="../normomo/",target="_blank"),
        a("Sykdomspuls",href="../sykdomspuls/",target="_blank"),
        a("TB Rapport",href="../reporttb/",target="_blank")
      )
    ),
    div(class="container",
      div(class="txt-center",
        tags$iframe(src="https://docs.google.com/forms/d/e/1FAIpQLSdiTdreXTCXCWelyBBZ2Cb01Qk5KP203SPi3OFjJPnorMXOcg/viewform?embedded=true",
                    width=900,
                    height=500,
                    frameborder="0",
                    marginheight="0",
                    marginwidth="0")
      )
    )
  )
}



# Both sample pages.
root_page <- pageHome()
apply_page <- pageApply()

# Creates router. We provide routing path and UI for this page.
router <- make_router(
  route("/s/smhb/index", root_page),
  route("/s/smhb/rad", apply_page)

)

# Creat output for our router in main UI of Shiny app.
ui <- shinyUI(fluidPage(
  useShinyjs(),
  router_ui()
))



# Plug router into Shiny server.
server <- shinyServer(function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  router(input, output)
})

# Run server in a standard way.
shinyApp(ui, server)