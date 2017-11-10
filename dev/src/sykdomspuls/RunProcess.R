con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/SYKDOMSPULS STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(foreach))
suppressMessages(library(doSNOW))
suppressMessages(library(iterators))


if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_sykdomspuls/", export_all=FALSE)
} else {
  library(sykdomspuls)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="sykdomspuls"
)

smallMunicips <- c(
  "municip1151",
  "municip1835",
  "municip1252",
  "municip1739")

StackIterator <- function(stack, data, progressFunction) {
  library(data.table)
  it <- icount(nrow(stack))

  nextEl <- function() {
    i <- nextElem(it)
    progressFunction(i)
    list("stack"=stack[i],"data"=data[.(stack$type[i],stack$location[i],stack$age[i])])
    #list("stack"=stack[i],"data"=data[variable==stack$type[i] & location==stack$location[i] & age==stack$age[i]])
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}

#it <- StackIterator(data,stack)
#nextElem(it)


if(!UpdateData()){
  cat(sprintf("%s/%s/R/SYKDOMSPULS Have not run analyses and exiting",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  q(save="no", status=21)
} else {
  dataDoctor <- readRDS(file = DashboardFolder("data_clean",LatestDatasets()$legekontakt_everyone))
  dataDoctor <- melt.data.table(dataDoctor, measure.vars=c(
    sykdomspuls::CONFIG$SYNDROMES
  ), variable.factor=FALSE)
  dataDoctor <- dataDoctor[variable %in% sykdomspuls::CONFIG$SYNDROMES_DOCTOR]

  dataAll <- readRDS(file = DashboardFolder("data_clean",LatestDatasets()$everyone_everyone))
  dataAll <- melt.data.table(dataAll, measure.vars=c(
    sykdomspuls::CONFIG$SYNDROMES
  ), variable.factor=FALSE)
  dataAll <- dataAll[variable %in% sykdomspuls::CONFIG$SYNDROMES_ALL]

  data <- rbind(dataDoctor,dataAll)
  rm("dataDoctor")
  rm("dataAll")

  counties <- unique(data$county)
  municips <- unique(data$municip)
  #if(Sys.getenv("COMPUTER")=="test") municips <- municips[stringr::str_detect(municips,"^municip01")]
  locations <- c("Norge",counties,municips)

  ages <- unique(data$age)
  if(Sys.getenv("COMPUTER") %in% c("smhb","test")){
    DeleteLatestDoneFile()
  }

  dataCounties <- data[,.(
    consultWithInfluensa=sum(consultWithInfluensa),
    consultWithoutInfluensa=sum(consultWithoutInfluensa),
    pop=sum(pop),
    value=sum(value),
    HelligdagIndikator=max(HelligdagIndikator)
  ),by=.(date,age,county,variable)]
  setnames(dataCounties,"county","location")

  dataNorway <- data[,.(
    consultWithInfluensa=sum(consultWithInfluensa),
    consultWithoutInfluensa=sum(consultWithoutInfluensa),
    pop=sum(pop),
    value=sum(value),
    HelligdagIndikator=max(HelligdagIndikator)
  ),by=.(date,age,variable)]
  dataNorway[,location:="Norge"]

  setnames(data,"municip","location")
  data[,county:=NULL]
  setcolorder(data,c("variable","date","HelligdagIndikator","location","age","pop","consultWithInfluensa","consultWithoutInfluensa","value"))
  setcolorder(dataCounties,c("variable","date","HelligdagIndikator","location","age","pop","consultWithInfluensa","consultWithoutInfluensa","value"))
  setcolorder(dataNorway,c("variable","date","HelligdagIndikator","location","age","pop","consultWithInfluensa","consultWithoutInfluensa","value"))

  data <- rbind(dataNorway,dataCounties,data)

  # setting control stack for counties
  analysesCounties <- data.table(expand.grid(
    type=sykdomspuls::CONFIG$SYNDROMES,
    location=c("Norge", counties),
    age=ages,
    granularity=c("Daily","Weekly"),
    stringsAsFactors = FALSE))
  analysesCounties[,v:=sykdomspuls::CONFIG$VERSION]


  # setting control stack for municipalities
  analysesMunicips <- data.table(expand.grid(
    type=sykdomspuls::CONFIG$SYNDROMES,
    location=municips,
    age=ages,
    granularity=c("Weekly"),
    stringsAsFactors = FALSE))
  analysesMunicips[,v:=sykdomspuls::CONFIG$VERSION]

  # control stack for comparison of models
  analysesComparison <- vector("list",length(sykdomspuls::CONFIG$VERSIONS))
  for(vx in sykdomspuls::CONFIG$VERSIONS){
    temp <- analysesCounties[location=="Norge" & granularity=="Weekly"]
    temp[,v:=vx]
    analysesComparison[[vx]] <- copy(temp)
  }
  analysesComparison <- rbindlist(analysesComparison)

  cat(sprintf("%s/%s/R/SYKDOMSPULS Registering cluster",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  cl <- makeCluster(parallel::detectCores())
  registerDoSNOW(cl)

  numProcesses <- nrow(analysesComparison)+nrow(analysesCounties)+nrow(analysesMunicips)
  pb <- RAWmisc::ProgressBarCreate(max=numProcesses)
  assign("pb", pb, envir = .GlobalEnv)
  progressIndex <- 0
  assign("progressIndex", progressIndex, envir = .GlobalEnv)
  ProgressFunction <- function(n) RAWmisc::ProgressBarSet(pb, progressIndex + n)
  assign("ProgressFunction", ProgressFunction, envir = .GlobalEnv)
  opts <- list(progress=ProgressFunction)
  assign("opts", opts, envir = .GlobalEnv)

  cat(sprintf("%s/%s/R/SYKDOMSPULS Setting keys for binary search - VERY IMPORTANT 3x SPEEDUP",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  setkeyv(data,c("variable","location","age"))

  sprintf("%s/%s/R/SYKDOMSPULS Beginning analyses",Sys.time(),Sys.getenv("COMPUTER"))
  for(i in c(1:4)){
    if(i==1){
      stack <- analysesComparison
      name <- "resComparisons"
    } else if(i==2){
      stack <- analysesCounties[granularity=="Daily"]
      name <- "resRecentLine"
    } else if(i==3){
      stack <- analysesCounties[granularity=="Weekly"]
      name <- "resYearLine"
    } else if(i==4){
      stack <- analysesMunicips
      name <- "resYearLineMunicip"
    }

    res <- foreach(analysisIter=StackIterator(stack, data, ProgressFunction), .noexport=c("data")) %dopar% {
      #a <- StackIterator(analysesMunicips, data, ProgressFunction)
      #analysisIter <- nextElem(a)
      #b <- RunOneAnalysis(analysesStack=analysisIter$stack,analysisData=analysisIter$data)
      #
      #analysesStack <- analysesMunicips[type=="respiratory" & location=="municip1929" & age=="Totalt" & granularity=="Weekly" & v==1]
      #analysisData <- data[variable=="respiratory" & location=="municip1929" & age=="Totalt"]
      #retval <- RunOneAnalysis(analysesStack,analysisData)
      library(data.table)
      if(Sys.getenv("RSTUDIO") == "1"){
      	#devtools::load_all("/src/sykdomspuls/package/", export_all=FALSE)
        library(sykdomspuls)
      } else {
      	library(sykdomspuls)
      }
      
      exceptionalFunction <- function(err){
        sink("/results/sykdomspuls/log.txt")
        print(err)
        print(analysisIter$stack)
        sink()
        saveRDS(analysisIter$stack,"/results/sykdomspulen/analysesStack.RDS")
        saveRDS(analysisIter$data,"/results/sykdomspulen/analysisData.RDS")
      }
      retval <- tryCatch(
        RunOneAnalysis(analysesStack=analysisIter$stack,analysisData=analysisIter$data),
        error=exceptionalFunction
        )

      if(i==4) retval[, county := GetCountyFromMunicip(analysisIter$stack$location, norwayLocations=norwayLocations)]
      retval
    }
    assign("progressIndex", progressIndex + nrow(stack), envir = .GlobalEnv)
    res <- rbindlist(res)
    
    # Performing cleaning on small municipalities
    if(i==4){
      res[location %in% smallMunicips & age != "Totalt", n := 0 ]
      res[location %in% smallMunicips & age != "Totalt", threshold2 := 5 ]
      res[location %in% smallMunicips & age != "Totalt", threshold4 := 10 ]
    }
    SaveData(res, DashboardFolder("results",sprintf("%s.RDS",name)))
    SaveData(res, DashboardFolder("data_app",sprintf("%s.RDS",name)))
    
    # Save last 4 weeks of results
    if(i %in% c(3,4)){
      saveWkYrs <- rev(sort(unique(res$wkyr)))[1:4]
      res <- res[wkyr %in% saveWkYrs]
      SaveData(res, DashboardFolder("results",
                                    sprintf("archive_%s_%s.RDS",LatestDatasets()$date,name)))
    }
  }
  stopCluster(cl)
  rm("data")

  ## GENERATE LIST OF OUTBREAKS
  cat(sprintf("%s/%s/R/SYKDOMSPULS Generate list of outbreaks",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  ListOutbreaks()

  # Done with analyses
  cat(sprintf("%s/%s/R/SYKDOMSPULS Done with all analyses",Sys.time(),Sys.getenv("COMPUTER")),"\n")

  CreateLatestDoneFile()
  cat("done",file="/data_app/sykdomspuls/done.txt")

  ## SENDING OUT EMAILS
  EmailNotificationOfNewResults()
  EmailNotificationUtbrudd()

  cat(sprintf("%s/%s/R/SYKDOMSPULS Finished analyses and exiting",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  quit(save="no", status=0)
} 


