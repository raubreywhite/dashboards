#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @filter logger
function(req, res){
  print(ls(req))
  print(req$HTTP_ORIGIN)
  print(req$REMOTE_ADDR)
  
  forwarded <- req$HTTP_X_FORWARDED_FOR
  if(is.null(forwarded)) forwarded <- "NOTFORWARDED"
  
  val <- sprintf("%s - %s - %s - %s - %s", Sys.time(), forwarded, req$REMOTE_ADDR, req$PATH_INFO, req$QUERY_STRING)
  file <- file.path(DATA,"log",paste0(format.Date(Sys.time(),"%Y-%m-%d"),".txt"))
  try({
    cat(val,"\n",file=file, append=file.exists(file))
  },TRUE)
  forward()
}

#* @get /mean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  samples
}

#* @get /test
test <- function(x){
  x
  #unique(d$locationName)
}

#* @preempt logger
#* @get /v
test <- function(){
  "1_0"
  #unique(d$locationName)
}

#* @preempt logger
#* @get /v1_0_NamesTypes
v1_0_NamesTypes <- function(){
  retval <- as.data.frame(types)
  retval$names <- row.names(retval)
  row.names(retval) <- NULL
  names(retval) <- c("value","name")
  retval$short <- names(typesShort)
  jsonlite::toJSON(retval)
  #unique(d$locationName)
}

#* @preempt logger
#* @get /namesFylke
namesFylke <- function(){
  n <- unique(d[,c("location","locationName"),with=F])
  jsonlite::toJSON(n)
  #unique(d$locationName)
}

#* @preempt logger
#* @get /namesKommune
namesKommune <- function(xname="Norge"){
  n <- unique(d[,c("location","locationName"),with=F])
  retVal <- n[location==xname,]
  if(xname!="Norge" & xname!="All"){
    nk <- unique(dk[county==xname,c("location","locationName"),with=F])
    retVal <- rbind(retVal,nk)
  } else if(xname=="All"){
    nk <- unique(dk[,c("location","locationName"),with=F])
    retVal <- nk
  }
  jsonlite::toJSON(retVal)
  #unique(d$locationName)
}

#* @preempt logger
#* @get /v1_0_WeeksWeeklySignal
v1_0_WeeksWeeklySignal <- function(xlevel="fylke"){
  wkyr <- unique(c(outbreaks[["df"]]$wkyr,outbreaks[["dk"]]$wkyr))
  wkyr <- rev(sort(wkyr))
 
  jsonlite::toJSON(
    data.frame(value=wkyr,name=wkyr)
  )
}

#* @get /v1_0_DataWeeklySignal
v1_0_DataWeeklySignal <- function(xwkyr=NULL,xlevel="fylke"){
  if(xlevel=="fylke"){
    data <- outbreaks[["df"]]
  } else {
    data <- outbreaks[["dk"]]
  }
  
  data <- data[wkyr==xwkyr,-c(1:2),with=F]
  #data
  
  jsonlite::toJSON(
    data
  )
}

#* @get /v1_0_DataDailyLine
v1_0_DataDailyLine <- function(xtype="respiratoryexternal",xage="Totalt",xname="Norge"){
  library(data.table)
  data <- dr[location==xname & age==xage & type==xtype]
  prettyType <- names(types)[types==xtype]
  
  if(xage=="Totalt"){
    prettyAge <- "alle aldersgrupper"
  } else {
    prettyAge <- sprintf("aldersgruppe %s \u00E5r",xage)
  }
  titleMain <- sprintf("%s for %s i %s",prettyType,prettyAge,data$locationName[1])
  #data$x <- 1:nrow(data)
  labs <- unique(data[,c("xRaw",
                         "lab1",
                         "lab2",
                         "lab3",
                         "lab4",
                         "lab5",
                         "lab6",
                         "lab7",
                         "vlines",
                         "label"),with=F])
  
  jsonlite::toJSON(
    list(
      data=data[year >= (as.numeric(max(year))-3),c(
        "xRaw",
        "n",
        "threshold0",
        "threshold2",
        "threshold4"
      ),with=F],
      labs=labs,
      brush=data[,c(
        "xRaw",
        "n",
        "threshold0",
        "threshold2",
        "threshold4"
      ),with=F],
      titleMain=titleMain
    )
  )
}


#* @get /v1_0_DataWeeklyLine
v1_0_DataWeeklyLine <- function(xtype="respiratoryexternal",xage="Totalt",xname="Norge"){
  data <- d[location==xname & age==xage & type==xtype,]
  if(nrow(data)==0){
    data <- dk[location==xname & age==xage & type==xtype,]
  }
  prettyType <- names(types)[types==xtype]
  
  if(xage=="Totalt"){
    prettyAge <- "alle aldersgrupper"
  } else {
    prettyAge <- sprintf("aldersgruppe %s \u00E5r",xage)
  }
  titleMain <- sprintf("%s for %s i %s",prettyType,prettyAge,data$locationName[1])
  #data$x <- 1:nrow(data)
  labs <- unique(data[,c("xRaw",
                         "lab1",
                         "lab2",
                         "lab3",
                         "lab4",
                         "lab5",
                         "lab6",
                         "lab7",
                         "vlines",
                         "label"),with=F])

  jsonlite::toJSON(
    list(
    data=data[,c(
      "xRaw",
      "n",
      "threshold0",
      "threshold2",
      "threshold4"
    ),with=F],
    labs=labs,
    brush=data[,c(
      "xRaw",
      "n",
      "threshold0",
      "threshold2",
      "threshold4"
    ),with=F],
    titleMain = titleMain
    )
  )
}

#* @get /v1_0_DataWeeklyOverview
v1_0_DataWeeklyOverview <- function(xtype="respiratoryexternal",xage="Totalt",xname="Norge"){
  data <- dk[location %in% unique(dkStack[county==xname,]$location) & age==xage & type==xtype,]
  brush <- d[location==xname & age==xage & type==xtype,]
  if(nrow(data)==0){
    data <- d[age==xage & type==xtype,]
    brush <- d[location=="Norge" & age==xage & type==xtype,]
  }
  #data$x <- 1:nrow(data)
  labs <- unique(data[,c("xRaw",
                         "lab1",
                         "lab2",
                         "lab3",
                         "lab4",
                         "lab5",
                         "lab6",
                         "lab7",
                         "vlines",
                         "label"),with=F])

  jsonlite::toJSON(
    list(
      data=
        data[year >= (as.numeric(max(year))-3),c(
          "locationName",
          "xRaw",
          "statusNum"
        ),with=F],
      labs=labs,
      brush=
        brush[year >= (as.numeric(max(year))-3),c(
          "n",
          "xRaw",
          "statusNum"
        ),with=F]
    )
  )
}

#* @get /v1_0_DataWeeklyOverviewKommune
v1_0_DataWeeklyOverviewKommune <- function(xname="municip0301"){
  data <- dk[location %in% xname,]

  #data$x <- 1:nrow(data)
  labs <- unique(data[,c("xRaw",
                         "lab1",
                         "lab2",
                         "lab3",
                         "lab4",
                         "lab5",
                         "lab6",
                         "lab7",
                         "vlines",
                         "label"),with=F])
  
  data <- data[xRaw>max(xRaw-8),]
  data$age <- factor(data$age,levels=c("Totalt","0-4","5-14","15-19","20-29","30-64","65+"))
  data$type <- factor(data$type,levels=typesShort)
  levels(data$type) <- names(typesShort)
  setorder(data,type,age)
  titleMain <- data$locationName[1]
  data$locationName <- paste0(data$type," - ",data$age)
  
  jsonlite::toJSON(
    list(
      data=
        data[data$year >= (as.numeric(max(year))-3),c(
          "locationName",
          "xRaw",
          "statusNum"
        ),with=F],
      labs=labs,
      titleMain=titleMain
    )
  )
}

# curl http://localhost:10002/v1_0_DataWeeklyOverviewKommune?name=municip0101


