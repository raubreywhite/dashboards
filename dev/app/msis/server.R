#HAI resistens		matt osv		sex og blod		barnvak		Influensa	

library(shiny)
library(ggvis)
library(data.table)

GetLocationName <- function(location) {
  locationName <- "Norge"
  locationHTML <- "Norge"
  
  if (location != "Norge") {
    norwayLocations <- readxl::read_excel("data/norwayLocations.xlsx")
    #data("countyToMunicip", package = "RAWmisc")
    if (sum(norwayLocations$municip == location) > 0) {
      locationName <- as.character(norwayLocations$municipName[norwayLocations$municip == location])
      locationHTML <- as.character(norwayLocations$municipHTML[norwayLocations$municip == location])
    } else if (sum(norwayLocations$county == location) > 0) {
      locationName <- as.character(norwayLocations$countyName[norwayLocations$county == location])
      locationHTML <- as.character(norwayLocations$countyHTML[norwayLocations$county == location])
    }
  }
  
  return(c(locationName,locationHTML))
}

diagSignalResults <- readRDS(file = "data/diagSignalResults.RDS")
agentSignalResults <- readRDS(file = "data/agentSignalResults.RDS")
delaySignal <- readRDS(file = "data/delaySignal.RDS")

diagSignalResults <- merge(diagSignalResults,delaySignal,by="diag")
diagSignalResults[,sufficientRegistered:=TRUE]
diagSignalResults[time>=max(time)-delay,sufficientRegistered:=FALSE]

agentSignalResults <- merge(agentSignalResults,delaySignal,by="diag")
agentSignalResults[,sufficientRegistered:=TRUE]
agentSignalResults[time>=max(time)-delay,sufficientRegistered:=FALSE]

diagRegistered <- readRDS(file = "data/diagRegistered.RDS")
agentRegistered <- readRDS(file = "data/agentRegistered.RDS")

control <- readRDS(file = "data/control.RDS")
groups <- c("Alt"=0,"HAI resistens"=1,"Matt"=2,"Sex og blod"=3,"Barnvak"=4,"Influensa"=5)

signalLocation <- registeredLocation <- c(
                        "Alt",
                        "Norge",
                        "Utlandet",
                        "Østfold",
                        "Akershus",
                        "Oslo",
                        "Hedmark",
                        "Oppland",
                        "Buskerud",
                        "Vestfold",
                        "Telemark",
                        "Aust-Agder",
                        "Vest-Agder",
                        "Rogaland",
                        "Hordaland",
                        "Sogn og Fjordane",
                        "Møre og Romsdal",
                        "Sør-Trøndelag",
                        "Nord-Trøndelag",
                        "Nordland",
                        "Troms",
                        "Finnmark")

yrwk <- rev(unique(diagSignalResults$yrwk))
season <- rev(unique(diagSignalResults$season))

diag <- unique(diagSignalResults$diag)
agent <- unique(agentSignalResults$agent)

type <- c("Diagnose"="Diagnose","Smittestøff"="Agent")

shinyServer(function(input, output) {

  GetCols <- reactive({
    retval <- c('#fc8d59','#91cf60')
    if(!is.null(input$colourBlind)){
      if(input$colourBlind){
        retval <- c('#fc8d59','#91bfdb')
      } 
    }
    return(retval)
  })
  
  GetCols5 <- reactive({
    retval <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
    if(!is.null(input$colourBlind)){
      if(input$colourBlind){
        retval <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
      } 
    }
    return(retval)
  })
  
  ## diagnosis barometer
  output$barometerLocationSelector <- renderUI({
    selectInput("barometerLocation", "Smittested", as.list(signalLocation),selected="Norge")
  })
  
  output$barometerTypeSelector <- renderUI({
    selectInput("barometerType", "Type", as.list(type), selected = type[1], selectize=FALSE, size=2)
  })
  
  output$barometerGroupSelector <- renderUI({
    selectInput("barometerGroup", "Gruppe", as.list(groups), selected = groups[1], selectize=FALSE, multiple=TRUE, size=length(groups))
  })
  
  output$barometerWarningSelector <- renderUI({
    selectInput("barometerWarning", "Varsler", as.list(c("Alt","Varsler innen 52 uker","Varsler innen 26 uker","Varsler innen 12 uker")), selected = "Varsler innen 52 uker", selectize=FALSE, multiple=FALSE, size=4)
  })
  
  barometerDiseaseChoices <- reactive({
    if (is.null(input$barometerType) | is.null(input$barometerGroup) | is.null(input$barometerWarning)) return("X")
    print("change")
    if(input$barometerGroup[1]==0){
      agentx <- control$agentControl
      diagx <- control$diagControl
    } else {
      agentx <- c()
      diagx <- c()
      for(i in as.numeric(input$barometerGroup)){
        agentx <- unique(c(agentx,control$agentGroupControl[[i]]))
        diagx <- unique(c(diagx,control$diagGroupControl[[i]]))
      }
    }
    print(diagx)
    print(input$barometerGroup)
    
    if (input$barometerType == "Diagnose") {
      con <- diagx
      useData <- diagSignalResults[location==input$barometerLocation]
    } else {
      con <- agentx
      useData <- agentSignalResults[location==input$barometerLocation]
      useData[,diag:=agent]
    }
    useData <- useData[diag %in% con]
    
    if(input$barometerWarning=="Alt"){
      disease <- unique(useData$diag)
    } else if(input$barometerWarning=="Varsler innen 26 uker"){
      useData[,alarmX:=0]
      useData[time > max(time)-26,alarmX:=alarm]
      useData[,alarmX:=max(alarmX),by=diag]
      
      disease <- unique(useData[alarmX==1]$diag)
    } else if(input$barometerWarning=="Varsler innen 52 uker"){
      useData[,alarmX:=0]
      useData[time > max(time)-52,alarmX:=alarm]
      useData[,alarmX:=max(alarmX),by=diag]
      
      disease <- unique(useData[alarmX==1]$diag)
    } else {
      useData[,alarmX:=0]
      useData[time > max(time)-12,alarmX:=alarm]
      useData[,alarmX:=max(alarmX),by=diag]
      
      disease <- unique(useData[alarmX==1]$diag)
    }
    if(length(disease)==0) disease <- "INGEN SYKDOM MED DISSE KRAVENE"
    return(disease)
    
  })
  
  output$barometerDiseaseSelector <- renderUI({
    x <- barometerDiseaseChoices()
    sel <- x
    if(length(sel)>100) sel <- NULL
    selectInput("barometerDisease", "Sykdom", as.list(x), selected = sel, selectize=FALSE, multiple=TRUE, size=min(c(10,length(x))))
  })
  
  output$barometerWeekSelector <- renderUI({
    selectInput('barometerWeek',
                       label = 'Uker',
                       choices = as.list(yrwk),
                multiple=TRUE, selectize=FALSE,
                       selected = yrwk[1:52],
                size=25)
  })
  
  barometerPlotData <- reactive({
    useData <- diagSignalResults
    
    retData <- useData[location=="Norge" & yrwk %in% yrwk[1:10]]
    if(!is.null(input$barometerLocation) & !is.null(input$barometerType) & !is.null(input$barometerDisease)){
      if(input$barometerType=="Agent"){
        useData <- agentSignalResults[location == input$barometerLocation]
        useData[,diag:=agent]
      } else {
        useData <- diagSignalResults[location == input$barometerLocation]
      }
 
      retData <- useData[diag %in% input$barometerDisease & yrwk %in% input$barometerWeek]
    }

    return(retData)

  })
  reactive({
    if(is.null(input$barometerDisease)){
      return(
        data.frame(x=1:3,y=1:3) %>%
          ggvis(~x, ~y) %>%
          layer_points(opacity:=0) %>%
          layer_text(text:="SELECT DISEASE(S) FROM THE RIGHT COLUMN",fontSize := 20, align:="center", data=data.frame(x=2,y=2))
      )
    }
    if(input$barometerDisease[1]=="INGEN SYKDOM MED DISSE KRAVENE"){
      return(
        data.frame(x=1:3,y=1:3) %>%
          ggvis(~x, ~y) %>%
          layer_points(opacity:=0) %>%
          layer_text(text:="INGEN SYKDOM MED DISSE KRAVENE",fontSize := 20, align:="center", data=data.frame(x=2,y=2))
      )
    }
    
    pd <- barometerPlotData()
    xLen <- length(unique(pd$yrwk))
    yLen <- 100+30*length(unique(pd$diag))
    if(yLen>800) yLen <- 800
    pd %>%
    ggvis(x = ~yrwk, y = ~diag) %>%
      layer_rects(width = band(), height = band(), data=pd, fill="Høyere enn forventet", opacity := 0)%>%
      layer_rects(width = band(), height = band(), data=pd, fill="Forventet", opacity := 0) %>%
      layer_rects(width = band(), height = band(), data=pd[sufficientRegistered==FALSE & displayAlarm=="Vanlig"], fill="Forventet", opacity := 0.3)%>%
      layer_rects(width = band(), height = band(), data=pd[sufficientRegistered==TRUE & displayAlarm=="Vanlig"], fill="Forventet", opacity := 0.7)%>%
      layer_rects(width = band(), height = band(), data=pd[displayAlarm=="Varsel"], fill="Høyere enn forventet", opacity := 0.7)%>%
    
      scale_ordinal("fill", range = GetCols()) %>%
      add_legend("fill") %>%
      scale_nominal("x", padding = 0, points = FALSE) %>%
      scale_nominal("y", padding = 0, points = FALSE) %>% 
      add_axis("x", title = "", properties=axis_props(
        labels=list(angle=-90, align="right", baseline="middle"),
        axis = list(strokeWidth = 0),
        grid = list(strokeWidth = 0),
        ticks = list(strokeWidth = 0))) %>%
      add_axis("y", title = "", properties=axis_props(
        axis = list(strokeWidth = 0),
        grid = list(strokeWidth = 0),
        ticks = list(strokeWidth = 0)))%>%
      set_options(width = "auto", height = yLen)
  }) %>% bind_shiny("barometerPlot")
  
  ## compare barometer
  
  output$compareBarTypeSelector <- renderUI({
    selectInput("compareBarType", "Type", as.list(type), selected = type[1], selectize=FALSE, size=2)
  })
  
  output$compareBarGroupSelector <- renderUI({
    selectInput("compareBarGroup", "Gruppe", as.list(groups), selected = groups[1], selectize=FALSE, multiple=TRUE, size=length(groups))
  })
  
  compareBarDiseaseChoices <- reactive({
    if (is.null(input$compareBarType) | is.null(input$compareBarGroup)) return("X")
    print("change")
    if(input$compareBarGroup[1]==0){
      agentx <- control$agentControl
      diagx <- control$diagControl
    } else {
      agentx <- c()
      diagx <- c()
      for(i in as.numeric(input$compareBarGroup)){
        agentx <- unique(c(agentx,control$agentGroupControl[[i]]))
        diagx <- unique(c(diagx,control$diagGroupControl[[i]]))
      }
    }
    agentx <- agentx[order(agentx)]
    diagx <- diagx[order(diagx)]
    if (input$compareBarType == "Diagnose") {
      return(diagx)
    } else {
      return(agentx)
    }
  })
  
  output$compareBarDiseaseSelector <- renderUI({
    selectInput("compareBarDisease", "Sykdom", as.list(compareBarDiseaseChoices()), selected = compareBarDiseaseChoices()[1], selectize=FALSE, size=15)
  })
  
  output$compareBarWeekSelector <- renderUI({
    selectInput('compareBarWeek',
                label = 'Uker',
                choices = as.list(yrwk),
                multiple=TRUE, selectize=FALSE,
                selected = yrwk[1:52],
                size=25)
  })
  
  compareBarPlotData <- reactive({
    useData <- diagSignalResults
    
    retData <- useData[yrwk %in% yrwk[1:10]]
    if(!is.null(input$compareBarType) & !is.null(input$compareBarDisease)){
      if(input$compareBarType=="Agent"){
        useData <- agentSignalResults[agent == input$compareBarDisease]
      } else {
        useData <- diagSignalResults[diag == input$compareBarDisease]
      }
      
      retData <- useData[ yrwk %in% input$compareBarWeek]
    }
    retData[,location:=factor(location,levels=signalLocation)]
    return(retData)
    
  })
  reactive({
    
    if(is.null(input$compareBarDisease)){
      return(
        data.frame(x=1:3,y=1:3) %>%
          ggvis(~x, ~y) %>%
          layer_points(opacity:=0) %>%
          layer_text(text:="SELECT DISEASE(S) FROM THE RIGHT COLUMN",fontSize := 20, align:="center", data=data.frame(x=2,y=2))
      )
    }

    pd <- compareBarPlotData()
    xLen <- length(unique(pd$yrwk))
    yLen <- 100+30*length(unique(pd$location))
    if(yLen>800) yLen <- 800
    pd %>%
      ggvis(x = ~yrwk, y = ~location) %>%
      layer_rects(width = band(), height = band(), data=pd, fill="Høyere enn forventet", opacity := 0)%>%
      layer_rects(width = band(), height = band(), data=pd, fill="Forventet", opacity := 0) %>%
      layer_rects(width = band(), height = band(), data=pd[sufficientRegistered==FALSE & displayAlarm=="Vanlig"], fill="Forventet", opacity := 0.3)%>%
      layer_rects(width = band(), height = band(), data=pd[sufficientRegistered==TRUE & displayAlarm=="Vanlig"], fill="Forventet", opacity := 0.7)%>%
      layer_rects(width = band(), height = band(), data=pd[displayAlarm=="Varsel"], fill="Høyere enn forventet", opacity := 0.7)%>%
      
      scale_ordinal("fill", range = GetCols()) %>%
      add_legend("fill") %>%
      scale_nominal("x", padding = 0, points = FALSE) %>%
      scale_nominal("y", padding = 0, points = FALSE) %>% 
      add_axis("x", title = "", properties=axis_props(
        labels=list(angle=-90, align="right", baseline="middle"),
        axis = list(strokeWidth = 0),
        grid = list(strokeWidth = 0),
        ticks = list(strokeWidth = 0))) %>%
      add_axis("y", title = "", properties=axis_props(
        axis = list(strokeWidth = 0),
        grid = list(strokeWidth = 0),
        ticks = list(strokeWidth = 0)))%>%
      set_options(width = "auto", height = yLen)
  }) %>% bind_shiny("compareBarPlot")
  
  
  ## lines
  output$linesLocationSelector <- renderUI({
    selectInput("linesLocation", "Smittested", as.list(signalLocation),selected="Norge")
  })
  
  output$linesTypeSelector <- renderUI({
    selectInput("linesType", "Type", as.list(type), selected = type[1], selectize=FALSE, size=2)
  })
  
  linesDiseaseChoices <- reactive({
    if (is.null(input$linesType)) return(diag)
      
    if (input$linesType == "Diagnose") {
      return(diag)
    } else {
      return(agent)
    }
  })
  
  output$linesDiseaseSelector <- renderUI({
    selectInput("linesDisease", "Sykdom", as.list(linesDiseaseChoices()), selected = linesDiseaseChoices()[1], selectize=FALSE, size=15)
  })
  
  output$linesSeasonSelector <- renderUI({
    selectInput('linesSeason',
                label = 'Sesonger',
                choices = as.list(season),
                multiple=TRUE, selectize=FALSE,
                selected = season[1:2],
                size=length(season))
  })
  
  linesPlotData <- reactive({
    useData <- diagSignalResults
    if(!is.null(input$linesType)){
      if(input$linesType=="Agent"){
        useData <- copy(agentSignalResults)
        useData[,diag:=agent]
      }
    }
    
    retDataNorway <- useData[
      diag==linesDiseaseChoices()[1] & 
      season %in% season[1:2]]
    if (is.null(input$linesLocation) | is.null(input$linesDisease)) {
      retData <- retDataNorway
    } else {
      retData <- useData[
        location == input$linesLocation &
          diag==input$linesDisease & 
          season %in% input$linesSeason]
      setorder(retData,location)
    }
    cat(dim(retData))
    if (nrow(retData) == 0) retData <- retDataNorway
    suppressWarnings(retData[, top := max(c(num, upperbound), na.rm = T) + 2])
    return(retData)
  })
  
  reactive({
    pd <- linesPlotData() 
    pd %>%
      ggvis(x = ~displayDay) %>%
      layer_ribbons(y = ~upperbound, y2 = ~top, fill = "Høyere enn forventet", opacity := 0.4) %>%
      layer_ribbons(y = 0, y2 = ~upperbound, fill = "Forventet", opacity := 0.4) %>%
      scale_ordinal("fill", range = GetCols()) %>%
      scale_datetime("x", nice = "week") %>%
      add_legend("fill") %>%
      layer_paths(y = ~num, stroke := "black", strokeWidth := 3, data=pd) %>%
      layer_points(y = ~num, size := 80, fill := "black", data=pd) %>%
      layer_points(y = ~num, size := 40, fill="Høyere enn forventet", data=pd[displayAlarm=="Varsel"]) %>%
      add_axis("x", title = "", properties=axis_props(labels=list(angle=-90, align="right", baseline="middle"))) %>%
      add_axis("y", title="Antall tilfeller", title_offset = 50) %>%
      set_options(width = "auto", height = 700)
  }) %>% bind_shiny("linesPlot")
  
  
  ## regdate tables
  output$regtableLocationSelector <- renderUI({
    selectInput("regtableLocation", "Smittested", as.list(registeredLocation), selected = registeredLocation[1], selectize=FALSE, multiple=TRUE, size=length(registeredLocation))
  })
  
  output$regtableTypeSelector <- renderUI({
    selectInput("regtableType", "Type", as.list(type), selected = type[1], selectize=FALSE, size=2)
  })
  
  output$regtableGroupSelector <- renderUI({
    selectInput("regtableGroup", "Gruppe", as.list(groups), selected = groups[1], selectize=FALSE, multiple=TRUE, size=length(groups))
  })
  
  regtableDiseaseChoices <- reactive({
    if (is.null(input$regtableType) | is.null(input$regtableGroup)) return("X")
    print("change")
    if(input$regtableGroup[1]==0){
      agentx <- control$agentControl
      diagx <- control$diagControl
    } else {
      agentx <- c()
      diagx <- c()
      for(i in as.numeric(input$regtableGroup)){
        agentx <- unique(c(agentx,control$agentGroupControl[[i]]))
        diagx <- unique(c(diagx,control$diagGroupControl[[i]]))
      }
    }
    agentx <- agentx[order(agentx)]
    diagx <- diagx[order(diagx)]
    if (input$regtableType == "Diagnose") {
      return(diagx)
    } else {
      return(agentx)
    }
  })
  
  output$regtableDiseaseSelector <- renderUI({
    selectInput("regtableDisease", "Sykdom", as.list(regtableDiseaseChoices()), selected = regtableDiseaseChoices(), selectize=FALSE, size=15, multiple=TRUE)
  })
  
  output$regtableWeekSelector <- renderUI({
    selectInput('regtableWeek',
                label = 'Uker',
                choices = as.list(yrwk),
                multiple=TRUE, selectize=FALSE,
                selected = yrwk[1:4],
                size=25)
  })
  
  regtableData <- reactive({
    useData <- copy(diagRegistered)
    if(!is.null(input$regtableType)){
      if(input$regtableType=="Agent"){
        useData <- agentRegistered[agent %in% input$regtableDisease]
      } else {
        useData <- diagRegistered[diag %in% input$regtableDisease]
        useData[,agent:=""]
      }
    }
    
    retDataNorway <- useData[yrwk %in% yrwk[1:10]]
    
    if (is.null(input$regtableLocation)) {
      retData <- retDataNorway
    } else {
      retData <- useData[location %in% input$regtableLocation]
    }
    
    RegisteredWeeks <- yrwk[1:4]
    if(!is.null(input$regtableWeek)) RegisteredWeeks <- input$regtableWeek
    
    priorWeeks <- vector("list",5)
    for(i in 1:5){
      priorWeeks[[i]] <- RegisteredWeeks
      stringr::str_sub(priorWeeks[[i]],1,4) <- as.numeric(stringr::str_extract(priorWeeks[[i]],"^[0-9][0-9][0-9][0-9]"))-i
    }
    priorWeeks <- do.call(c,priorWeeks)
    
    retData <- retData[yrwk %in% c(RegisteredWeeks,priorWeeks)]
    retData[,status:="Now"]
    retData[yrwk %in% priorWeeks,status:="Historic"]
    
    retData <- retData[,.(num=sum(num)),by=.(yrwk,diag,agent,status)]
    retData <- retData[,.(num=round(mean(num)*length(RegisteredWeeks))),by=.(diag,agent,status)]
    retData <- dcast.data.table(retData,"diag+agent~status", value.var="num")
    setcolorder(retData,c("diag","agent","Now","Historic"))
    retData <- retData[Historic>0 | Now>0]
    retData[,NumIncrease:=Now-Historic]
    retData[Historic!=0,PercIncrease:=round((Now-Historic)/Historic*100,0)]
    retData[Historic==0,PercIncrease:=9999]
    retData <- retData[Now>0]
    setnames(retData,c("Diagnose","Smittestøff","Antall nå","Antall historisk","Antall økning","Prosjent økning"))
    
    return(as.data.frame(retData))
  })
  
  output$regtableTable <- DT::renderDataTable(DT::datatable({
    regtableData()
  }, filter="none",rownames=FALSE,style="bootstrap",options = list(pageLength = 25)))
})