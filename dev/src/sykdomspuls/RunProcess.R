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

SaveData(sykdomspuls::CONFIG, DashboardFolder("results","config.RDS"))
SaveData(sykdomspuls::CONFIG, DashboardFolder("data_app","config.RDS"))

dataFiles <- c("resComparisons", "resRecentLine", "resYearLine", "resYearLineMunicip")

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
    list("stack"=stack[i],"data"=data[.(stack$location[i],stack$age[i])])
    #list("stack"=stack[i],"data"=data[variable==stack$type[i] & location==stack$location[i] & age==stack$age[i]])
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}

#it <- StackIterator(data,stack)
#nextElem(it)

flush.console()

if(!UpdateData()){
  cat(sprintf("%s/%s/R/SYKDOMSPULS Have not run analyses and exiting",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  q(save="no", status=21)
} else {
  DeleteOldDatasets()

  for(SYNDROME in sykdomspuls::CONFIG$SYNDROMES){
    cat(sprintf("\n\n%s/%s/R/SYKDOMSPULS ***%s***\n",Sys.time(),Sys.getenv("COMPUTER"),SYNDROME))
    flush.console()

    data <- list()
    if(SYNDROME %in% sykdomspuls::CONFIG$SYNDROMES_DOCTOR){
      data[["municip"]] <- readRDS(file = DashboardFolder("data_clean",LatestDatasets(SYNDROME=SYNDROME)$legekontakt_everyone))
    } else {
      data[["municip"]] <- readRDS(file = DashboardFolder("data_clean",LatestDatasets(SYNDROME=SYNDROME)$everyone_everyone))
    }

    counties <- unique(data[["municip"]]$county)
    municips <- unique(data[["municip"]]$municip)
    #if(Sys.getenv("COMPUTER")=="test") municips <- municips[stringr::str_detect(municips,"^municip01")]
    locations <- c("Norge",counties,municips)

    ages <- unique(data[["municip"]]$age)

    data[["counties"]] <- data[["municip"]][,.(
      consultWithInfluensa=sum(consultWithInfluensa),
      consultWithoutInfluensa=sum(consultWithoutInfluensa),
      pop=sum(pop),
      value=sum(value),
      HelligdagIndikator=max(HelligdagIndikator)
    ),by=.(date,age,county)]
    setnames(data[["counties"]],"county","location")

    data[["norway"]] <- data[["municip"]][,.(
      consultWithInfluensa=sum(consultWithInfluensa),
      consultWithoutInfluensa=sum(consultWithoutInfluensa),
      pop=sum(pop),
      value=sum(value),
      HelligdagIndikator=max(HelligdagIndikator)
    ),by=.(date,age)]
    data[["norway"]][,location:="Norge"]

    setnames(data[["municip"]],"municip","location")
    data[["municip"]][,county:=NULL]
    setcolorder(data[["municip"]],c("date","HelligdagIndikator","location","age","pop","consultWithInfluensa","consultWithoutInfluensa","value"))
    setcolorder(data[["counties"]],c("date","HelligdagIndikator","location","age","pop","consultWithInfluensa","consultWithoutInfluensa","value"))
    setcolorder(data[["norway"]],c("date","HelligdagIndikator","location","age","pop","consultWithInfluensa","consultWithoutInfluensa","value"))

    data <- rbindlist(data)

    # setting control stack for counties
    analysesCounties <- data.table(expand.grid(
      type=SYNDROME,
      location=c("Norge", counties),
      age=ages,
      granularity=c("Daily","Weekly"),
      stringsAsFactors = FALSE))
    analysesCounties[,v:=sykdomspuls::CONFIG$VERSION]

    # setting control stack for municipalities
    analysesMunicips <- data.table(expand.grid(
      type=SYNDROME,
      location=municips,
      age=ages,
      granularity=c("Weekly"),
      stringsAsFactors = FALSE))
    analysesMunicips[,v:=sykdomspuls::CONFIG$VERSION]
    #analysesMunicips <- analysesMunicips[location=="municip0301"]

    # control stack for comparison of models
    analysesComparison <- vector("list",length(sykdomspuls::CONFIG$VERSIONS))
    for(vx in sykdomspuls::CONFIG$VERSIONS){
      temp <- analysesCounties[location=="Norge" & granularity=="Weekly"]
      temp[,v:=vx]
      analysesComparison[[vx]] <- copy(temp)
    }
    analysesComparison <- rbindlist(analysesComparison)

    if(SYNDROME==sykdomspuls::CONFIG$SYNDROMES[1]){
      numProcessesPerSyndrome <- nrow(analysesComparison)+nrow(analysesCounties)+nrow(analysesMunicips)
      numProcesses <- numProcessesPerSyndrome * length(sykdomspuls::CONFIG$SYNDROMES)

      pb <- RAWmisc::ProgressBarCreate(max=numProcesses)
      assign("pb", pb, envir = .GlobalEnv)

      progressIndex <- 0
      assign("progressIndex", progressIndex, envir = .GlobalEnv)

      ProgressFunction <- function(n) RAWmisc::ProgressBarSet(pb, progressIndex + n)
      assign("ProgressFunction", ProgressFunction, envir = .GlobalEnv)

      opts <- list(progress=ProgressFunction)
      assign("opts", opts, envir = .GlobalEnv)
    }

    cat(sprintf("%s/%s/R/SYKDOMSPULS Setting keys for binary search\n",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    setkeyv(data,c("location","age"))

    cat(sprintf("%s/%s/R/SYKDOMSPULS Registering cluster\n",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    cl <- makeCluster(parallel::detectCores())
    registerDoSNOW(cl)

    for(i in c(1:4)){
      if(i==1){
        stack <- analysesComparison
      } else if(i==2){
        stack <- analysesCounties[granularity=="Daily"]
      } else if(i==3){
        stack <- analysesCounties[granularity=="Weekly"]
      } else if(i==4){
        stack <- analysesMunicips
      }
      flush.console()

      res <- foreach(analysisIter=StackIterator(stack, data, ProgressFunction), .noexport=c("data")) %dopar% {
        library(data.table)
        if(Sys.getenv("RSTUDIO") == "1"){
        	#devtools::load_all("/src/sykdomspuls/package/", export_all=FALSE)
          library(sykdomspuls)
        } else {
        	library(sykdomspuls)
        }

        exceptionalFunction <- function(err){
          #sink("/results/sykdomspuls/log.txt")
          print(err)
          print(analysisIter$stack)
          #sink()
          #saveRDS(analysisIter$stack,"/results/sykdomspulen/analysesStack.RDS")
          #saveRDS(analysisIter$data,"/results/sykdomspulen/analysisData.RDS")
        }
        #x <- analysisIter$nextElem()
        #analysesStack=x$stack
        #analysisData=x$data
        retval <- tryCatch(
          RunOneAnalysis(analysesStack=analysisIter$stack,analysisData=analysisIter$data),
          error=exceptionalFunction
          )

        #if(i==4) retval[, county := GetCountyFromMunicip(analysisIter$stack$location, norwayLocations=norwayLocations)]
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
      SaveData(res, DashboardFolder("results",sprintf("%s_%s.RDS",dataFiles[i],SYNDROME)))
      rm("res")
    }
    stopCluster(cl)
    rm("data")
  }

  # Append all the syndromes together
  for(i in 1:length(dataFiles)){
    res <- vector("list",length=length(sykdomspuls::CONFIG$SYNDROMES))
    for(s in 1:length(sykdomspuls::CONFIG$SYNDROMES)){
      res[[s]] <- readRDS(DashboardFolder("results",sprintf("%s_%s.RDS",dataFiles[i],sykdomspuls::CONFIG$SYNDROMES[s])))
    }
    res <- rbindlist(res)
    SaveData(res, DashboardFolder("results",sprintf("%s.RDS",dataFiles[i])))
    SaveData(res, DashboardFolder("data_app",sprintf("%s.RDS",dataFiles[i])))

    # Save last 8 weeks of results
    if(i %in% c(3,4)){
      saveWkYrs <- rev(sort(unique(res$wkyr)))[1:8]
      res <- res[wkyr %in% saveWkYrs]
      SaveData(res, DashboardFolder("results",
                                    sprintf("archive_%s_%s.RDS",LatestDatasets()$date,dataFiles[i])))
    }
  }

  ## GENERATE LIST OF OUTBREAKS
  cat(sprintf("%s/%s/R/SYKDOMSPULS Generate list of outbreaks",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  GenerateOutbreakListInternal()
  GenerateOutbreakListExternal()

  # Done with analyses
  cat(sprintf("%s/%s/R/SYKDOMSPULS Done with all analyses",Sys.time(),Sys.getenv("COMPUTER")),"\n")

  CreateLatestDoneFile()
  cat("done",file="/data_app/sykdomspuls/done.txt")

  ## SENDING OUT EMAILS
  EmailNotificationOfNewResults()

  cat(sprintf("%s/%s/R/SYKDOMSPULS Finished analyses and exiting",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  quit(save="no", status=0)
}

