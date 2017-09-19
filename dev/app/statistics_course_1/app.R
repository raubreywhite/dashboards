library(shiny)
library(shinyBS)
library(shiny.router)
library(data.table)
library(ggplot2)

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
            div(class="column10 prefix1",
                h1(textOutput("headerTitle"))
            ),
            div(class="column12 nav nav-small",
                a("Spørreskjema",href="/s/statistics_course_1/index",class="cl-teal"),
                a("Resultater",href="/s/statistics_course_1/resultater")
            )
        ),
        div(class="container column12",
            div(class="sidepadding",
                br(),
                div(class="txt-center",bsButton("language", "English", class = "btn-primary")),
                br(),
                h2(textOutput("headerYourLevel")),
                uiOutput("levelStatistics"),
                br(),
                uiOutput("levelR"),
                br(),
                uiOutput("levelSTATA"),
                br(),
                uiOutput("levelSPSS"),
                br(),
                uiOutput("preferredLanguage"),
                br(),
                uiOutput("hoursWorked"),
                br(),
                h2(textOutput("headerYourWants")),
                uiOutput("wantsStatisticsProblems"),
                br(),
                uiOutput("wantsCoursesMain"),
                uiOutput("wantsCoursesOther"),
                br(),

                h2(textOutput("headerYourData")),
                br(),
                uiOutput("dataSize"),
                br(),
                uiOutput("dataSource"),
                br(),
                uiOutput("dataType"),
                br(),
                h2(textOutput("headerOtherComments")),
                br(),
                uiOutput("furtherComments"),
                br(),
                div(class="txt-center",
                bsButton("submit", "Submit", class = "btn-primary")
                ),
                br(),
                br(),
                br()
          )
        )
     )
   )
}

# This creates UI for each page.
pageResults <- function(input, output, session) {
  div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://rawgit.com/raubreywhite/css/master/shiny.css"),
      tags$link(href='http://fonts.googleapis.com/css?family=Montserrat:400,700',rel='stylesheet',type='text/css'),
      tags$link(href='http://fonts.googleapis.com/css?family=Cardo:400,700',rel='stylesheet',type='text/css')
    ),
    div(class="container",
        div(class="txt-center",
            div(class="column10 prefix1",
                h1("Spørreskjema til kartlegging av statistikkunnskap")
            ),
            div(class="column12 nav nav-small",
                a("Spørreskjema",href="/s/statistics_course_1/index"),
                a("Resultater",href="/s/statistics_course_1/resultater",class="cl-teal")
            )
        ),
        div(class="container column12 txt-center",
            textOutput("numberResponses"),
            plotOutput("plotResponses"),
            passwordInput("password","Password",width=400),
            plotOutput("plotLevel"),
            plotOutput("plotFavouriteLanguage"),
            plotOutput("plotHours"),
            plotOutput("plotCoursesLevel"),
            plotOutput("plotCoursesLanguage"),
            plotOutput("plotDataSize"),
            plotOutput("plotDataSource"),
            plotOutput("plotDataType")
        )
    )
  )
}

# Plug router into Shiny server.
server <- shinyServer(function(input, output, session) {
  if(!dir.exists("data/responses")) dir.create("data/responses",recursive=TRUE)
  HumanTime <- function() format(Sys.time(), "%Y-%m-%d")
  
  values <- reactiveValues(language = "norwegian")
  source("norwegian.R", local=TRUE)
  
  observeEvent(input$language, ({
    if(values$language=="norwegian"){
      # MAKE EVERYTHING ENGLISH
      source("english.R", local=TRUE)
      
      values$language <- "english"
      updateButton(session, "language", label="Norsk")
    } else {
      
      source("norwegian.R", local=TRUE)
      
      values$language <- "norwegian"
      updateButton(session, "language", label="English")
    }
    
  }))
  
  GetLatestResponse <- function(){
    lastVal <- list.files("data/responses")
    if(length(lastVal)==0){
      lastVal <- 0
    } else {
      lastVal <- as.numeric(gsub(".RDS$","",lastVal))
    }
    lastVal <- max(lastVal)+1
    lastVal <- formatC(lastVal,width=8,flag="0")
    return(lastVal)
  }
  
  fieldsAll <- c(
    "levelStatistics",
    "levelR",
    "levelSTATA",
    "levelSPSS",
    "preferredLanguage",
    "hoursWorked",
    "wantsStatisticsProblems",
    "wantsCoursesMain",
    "wantsCoursesOther",
    "dataSize",
    "dataSource",
    "dataType",
    "furtherComments")
  
  formData <- reactive({
    retval <- list()
    for(i in fieldsAll){
      if(!is.null(input[[i]])) retval[[i]] <- input[[i]]
    }
    retval[["time"]] <- HumanTime()
    return(retval)
  })
  
  observeEvent(input$submit, ({
    #data <- formData()
    retval <- list()
    for(i in fieldsAll){
      if(!is.null(input[[i]])){
        retval[[i]] <- input[[i]]
      } else {
        retval[[i]] <- ""
      }
    }
    retval[["daySubmitted"]] <- HumanTime()
    retval[["id"]] <- GetLatestResponse()
    saveRDS(retval,file.path("data","responses",paste0(retval[["id"]],".RDS")))
    
    updateButton(session, "submit", label="Takk for interessen!", disabled = TRUE)
  }))
  
  GetAllResponses <- function(){
    lastVal <- list.files("data/responses")
    if(length(lastVal)==0){
      return(NULL)
    } else {
      retval <- vector("list",length(lastVal))
      for(i in 1:length(retval)){
        retval[[i]] <- readRDS(file.path("data","responses",lastVal[i]))
        x <- list()
        m <- 1
        for(j in 1:length(retval[[i]])){
          if(names(retval[[i]])[j]=="id") next
          for(l in 1:length(retval[[i]][[j]])){
            x[[m]] <- data.frame(var=names(retval[[i]])[j],val=retval[[i]][[j]][l],id=retval[[i]]$id)
            m <- m + 1
          }
        }
        retval[[i]] <- rbindlist(x)
      }
      retval <- rbindlist(retval)
      return(retval)
    }
  }
  
  GetNumResponses <- function(){
    d <- GetAllResponses()
    if(is.null(d)){
      return(0)
    } else return(length(unique(d$id)))
  }
  
  output$numberResponses <- renderText({
    sprintf("Number of responses: %d",GetNumResponses())
    })
  
  output$plotResponses <- renderPlot({
     d <- GetAllResponses()
     if(is.null(d)){
       return(NULL)
     }
     pd <- d[var=="daySubmitted",.(N=.N),by=.(val)]
     pd[,date:=as.Date(val)]
     q <- ggplot(pd,aes(x=date,y=N))
     #q <- q + geom_hline(yintercept=0)
     q <- q + geom_line()
     q <- q + geom_point()
     q <- q + scale_x_date("Date submitted")
     q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
     q <- q + expand_limits(y=0)
     q <- q + labs(title="Number of questionnaires submitted")
     q
  })
  
  output$plotLevel <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    d[,val:=factor(val,levels=rev(c("None","Beginner","Competent","Advanced")))]
    pd <- d[var %in% c("levelStatistics","levelR","levelSTATA","levelSPSS")]
    pdx <- d[var %in% c("levelR","levelSTATA","levelSPSS"),
             .(val=min(as.numeric(val))),
             by=id]
    pdx[,val:=factor(val,levels=1:4)]
    levels(pdx$val) <- rev(c("None","Beginner","Competent","Advanced"))
    pdx[,var:="levelR+STATA+SPSS"]
    pd <- rbind(pd,pdx)[,.(N=.N),by=.(val, var)]
    pd[,val:=factor(val,levels=rev(c("None","Beginner","Competent","Advanced")))]
    setorder(pd,-val)
    q <- ggplot(pd,aes(x=var,y=N,fill=val))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Level",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Language level")
    q
  })
  
  
  output$plotFavouriteLanguage <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("preferredLanguage","levelStatistics")]
    pd[,levelStatistics:=""]
    pd[var=="levelStatistics",levelStatistics:=val]
    setorder(pd,id,-levelStatistics)
    pd[levelStatistics=="",levelStatistics:=NA]
    pd[,levelStatistics:=zoo::na.locf(levelStatistics),by=id]
    
    pd <- pd[var %in% c("preferredLanguage"),
             .(N=.N),by=.(val, levelStatistics)]
    pd[,levelStatistics:=factor(levelStatistics,levels=rev(c("None","Beginner","Competent","Advanced")))]
    setorder(pd,-levelStatistics)
    
    pdx <- pd[,.(N=sum(N)),by=val]
    setorder(pdx,N)
    pd[,val:=factor(val,levels=pdx$val)]
    
    q <- ggplot(pd,aes(x=val,y=N,fill=levelStatistics))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Statistics level",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Preferred language")
    q
  })
  
  output$plotHours <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("hoursWorked","levelStatistics")]
    pd[,levelStatistics:=""]
    pd[var=="levelStatistics",levelStatistics:=val]
    setorder(pd,id,-levelStatistics)
    pd[levelStatistics=="",levelStatistics:=NA]
    pd[,levelStatistics:=zoo::na.locf(levelStatistics),by=id]
    
    pd <- pd[var %in% c("hoursWorked")]
    pd[,levelStatistics:=factor(levelStatistics,levels=(c("None","Beginner","Competent","Advanced")))]
    pd[,val:=as.numeric(as.character(val))]
    
    q <- ggplot(pd,aes(x=levelStatistics,y=val))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_boxplot()
    q <- q + scale_x_discrete("Statistics level")
    q <- q + scale_y_continuous("Hours",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Statistics level",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Hours worked weekly on data and/or analyses")
    q
  })
  
  
  output$plotCoursesLevel <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("wantsCoursesMain","levelStatistics")]
    pd[,levelStatistics:=""]
    pd[var=="levelStatistics",levelStatistics:=val]
    setorder(pd,id,-levelStatistics)
    pd[levelStatistics=="",levelStatistics:=NA]
    pd[,levelStatistics:=zoo::na.locf(levelStatistics),by=id]

    pd <- pd[var %in% c("wantsCoursesMain"),
            .(N=.N),by=.(val, levelStatistics)]
    pd[,levelStatistics:=factor(levelStatistics,levels=rev(c("None","Beginner","Competent","Advanced")))]
    setorder(pd,-levelStatistics)
    
    pdx <- pd[,.(N=sum(N)),by=val]
    setorder(pdx,N)
    pd[,val:=factor(val,levels=pdx$val)]
    
    q <- ggplot(pd,aes(x=val,y=N,fill=levelStatistics))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Statistics level",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Desired courses")
    q
  })
  
  output$plotCoursesLanguage <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("wantsCoursesMain","preferredLanguage")]
    pd[,preferredLanguage:=""]
    pd[var=="preferredLanguage",preferredLanguage:=val]
    setorder(pd,id,-preferredLanguage)
    pd[preferredLanguage=="",preferredLanguage:=NA]
    pd[,preferredLanguage:=zoo::na.locf(preferredLanguage),by=id]
    
    pd <- pd[var %in% c("wantsCoursesMain"),
             .(N=.N),by=.(val, preferredLanguage)]
    pd[,preferredLanguage:=factor(preferredLanguage,levels=rev(c("JMP","R","STATA","SPSS","Excel")))]
    setorder(pd,-preferredLanguage)
    
    pdx <- pd[,.(N=sum(N)),by=val]
    setorder(pdx,N)
    pd[,val:=factor(val,levels=pdx$val)]
    
    q <- ggplot(pd,aes(x=val,y=N,fill=preferredLanguage))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Preferred language",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Desired courses")
    q
  })
  
  output$plotDataSize <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("dataSize","preferredLanguage")]
    pd[,preferredLanguage:=""]
    pd[var=="preferredLanguage",preferredLanguage:=val]
    setorder(pd,id,-preferredLanguage)
    pd[preferredLanguage=="",preferredLanguage:=NA]
    pd[,preferredLanguage:=zoo::na.locf(preferredLanguage),by=id]
    
    pd <- pd[var %in% c("dataSize"),
             .(N=.N),by=.(val, preferredLanguage)]
    pd[,preferredLanguage:=factor(preferredLanguage,levels=rev(c("JMP","R","STATA","SPSS","Excel")))]
    setorder(pd,-preferredLanguage)
    
    pdx <- pd[,.(N=sum(N)),by=val]
    setorder(pdx,N)
    pd[,val:=factor(val,levels=pdx$val)]
    
    q <- ggplot(pd,aes(x=val,y=N,fill=preferredLanguage))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Preferred language",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Data size")
    q
  })
  
  
  output$plotDataSource <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("dataSource","preferredLanguage")]
    pd[,preferredLanguage:=""]
    pd[var=="preferredLanguage",preferredLanguage:=val]
    setorder(pd,id,-preferredLanguage)
    pd[preferredLanguage=="",preferredLanguage:=NA]
    pd[,preferredLanguage:=zoo::na.locf(preferredLanguage),by=id]
    
    pd <- pd[var %in% c("dataSource"),
             .(N=.N),by=.(val, preferredLanguage)]
    pd[,preferredLanguage:=factor(preferredLanguage,levels=rev(c("JMP","R","STATA","SPSS","Excel")))]
    setorder(pd,-preferredLanguage)
    
    pdx <- pd[,.(N=sum(N)),by=val]
    setorder(pdx,N)
    pd[,val:=factor(val,levels=pdx$val)]
    
    q <- ggplot(pd,aes(x=val,y=N,fill=preferredLanguage))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Preferred language",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Data source")
    q
  })
  
  output$plotDataType <- renderPlot({
    if(input$password!="richard") return(NULL)
    d <- GetAllResponses()
    if(is.null(d)){
      return(NULL)
    }
    pd <- d[var %in% c("dataType","preferredLanguage")]
    pd[,preferredLanguage:=""]
    pd[var=="preferredLanguage",preferredLanguage:=val]
    setorder(pd,id,-preferredLanguage)
    pd[preferredLanguage=="",preferredLanguage:=NA]
    pd[,preferredLanguage:=zoo::na.locf(preferredLanguage),by=id]
    
    pd <- pd[var %in% c("dataType"),
             .(N=.N),by=.(val, preferredLanguage)]
    pd[,preferredLanguage:=factor(preferredLanguage,levels=rev(c("JMP","R","STATA","SPSS","Excel")))]
    setorder(pd,-preferredLanguage)
    
    pdx <- pd[,.(N=sum(N)),by=val]
    setorder(pdx,N)
    pd[,val:=factor(val,levels=pdx$val)]
    
    q <- ggplot(pd,aes(x=val,y=N,fill=preferredLanguage))
    #q <- q + geom_hline(yintercept=0)
    q <- q + geom_bar(stat="identity")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("N",breaks=scales::pretty_breaks())
    q <- q + scale_fill_brewer("Preferred language",palette="Accent",drop=F)
    q <- q + expand_limits(y=0)
    q <- q + labs(title="Data type")
    q
  })
  
  router(input, output)
})

# Both sample pages.
root_page <- pageHome()
results_page <- pageResults()


# Creates router. We provide routing path and UI for this page.
router <- make_router(
  route("/s/statistics_course_1/index", root_page),
  route("/s/statistics_course_1/resultater", results_page)
)

# Creat output for our router in main UI of Shiny app.
ui <- shinyUI(fluidPage(
  router_ui()
))


# Run server in a standard way.
shinyApp(ui, server)