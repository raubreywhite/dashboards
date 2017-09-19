con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/NORMOMO STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_normomo/", export_all=FALSE)
} else {
  library(normomo)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="normomo"
)

tryCatch({
  # Your code starts here

  f <- list.files(DashboardFolder("data_clean"),"data_")
  f <- gsub("data_","",f)
  f <- gsub(".RDS$","",f)
  if(length(f)>0){
    fClean <- max(f)
  } else fClean <- ""

  f <- list.files(DashboardFolder("data_raw"),"FHIDOD2")
  f <- gsub("FHIDOD2_","",f)
  f <- gsub(".txt$","",f)
  f <- max(f)
  dateData <- as.Date(f,format="%Y%m%d")
  dateDataMinusOneWeek <- dateData - 7

  if(fClean==f){
  	cat(sprintf("%s/%s/R/NORMOMO No new data",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  } else if(!RAWmisc::IsFileStable(DashboardFolder("data_raw",paste0("FHIDOD2_",f,".txt")))){
  	cat(sprintf("%s/%s/R/NORMOMO Unstable data file",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  } else {
  	cat(sprintf("%s/%s/R/NORMOMO Stable data file",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  
    masterData <- fread(DashboardFolder("data_raw",paste0("FHIDOD2_",f,".txt")))
    masterData[,DoD:=as.Date(as.character(DODS_DATO),format="%Y%m%d")]
    masterData[,DoR:=as.Date(as.character(ENDR_DATO),format="%Y%m%d")]
    masterData[,DoB:=as.Date(as.character(FDATO_YYYYMMDD),format="%Y%m%d")]
    masterData[,age:=floor(as.numeric(difftime(DoD,DoB,units="days"))/365.25)]
    masterData[is.na(DoR),DoR:=DoD+1]
    masterData[DoR>="2015-09-03",DoR:=DoR+1]

    unlink(DashboardFolder("results",RAWmisc::YearWeek(dateDataMinusOneWeek)),recursive=TRUE,force=TRUE)
    dir.create(DashboardFolder("results",RAWmisc::YearWeek(dateDataMinusOneWeek)))
    dir.create(DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"Graphs")))
    dir.create(DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"MOMO")))
    dir.create(DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"Data")))

    allResults <- vector("list",100)
    pb <- RAWmisc::ProgressBarCreate(min=0,max=20,flush=TRUE)
    for(i in c(1:12,14:20,0)){
      if(i==0){
        dataAnalysis <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age"),with=F])
        saveRDS(dataAnalysis,file=DashboardFolder("data_clean",paste0("data_",f,".RDS")))
        runName <- "Norway"
      } else {
        dataAnalysis <- as.data.frame(masterData[!is.na(age) & FYLKE==i,c("DoD","DoR","age"),with=F])
        saveRDS(dataAnalysis,file=DashboardFolder("data_clean",sprintf("fylke_%s_%s.RDS",i,f)))
        runName <- sprintf("Fylke_%s",formatC(i,width=2,flag=0))
      }
      saveRDS(dataAnalysis,file=DashboardFolder("data_clean","data.RDS"))
      fwrite(dataAnalysis,file=DashboardFolder("data_clean","data.txt"))

      hfile <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
      hfile$date <- as.Date(hfile$date)
      fwrite(hfile,file=DashboardFolder("data_clean","bank_holidays.txt"))

      if(runName=="Norway"){
        plotGraphs <- TRUE
        MOMOgroups <- list(
          "0to4" =  "age >= 0 & age <=4",
          "5to14" = "age >= 5 & age <=14",
          "15to64" = "age >= 15 & age <=64",
          "65P" = "age >= 65 | is.na(age)",
          "Total" = "age >= 0 | is.na(age)"
        )
        MOMOmodels <- c(
          "0to4" = "LINE",
          "5to14" = "LINE",
          "15to64" = "LINE_SIN",
          "65P" = "LINE_SIN",
          "Total" = "LINE_SIN"
        )
      } else {
        plotGraphs <- FALSE
        MOMOgroups <- list(
          "Total" = "age >= 0 | is.na(age)"
        )
        MOMOmodels <- c(
          "Total" = "LINE_SIN"
        )
      }

      momo::SetOpts(
        DoA = dateData,
        DoPR = as.Date("2012-1-1"),
        WStart = 1,
        WEnd = 52,
        country = runName,
        source = "FHI",
        MFILE = "data.txt",
        HFILE = "bank_holidays.txt",
        INPUTDIR = DashboardFolder("data_clean"),
        WDIR = DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"MOMO")),
        back = 7,
        WWW = 290,
        Ysum = 2016,
        Wsum = 40,
        plotGraphs = plotGraphs,
        MOMOgroups = MOMOgroups,
        MOMOmodels = MOMOmodels,
        verbose=FALSE)

      momo::RunMoMo()

      data <- rbindlist(momo::dataExport$toSave, fill=TRUE)

      allResults[[i+1]] <- data
      allResults[[i+1]][,name:=runName]
      data <- data[,c("GROUP","wk","wk2","YoDi","WoDi","Pnb","nbc","UPIb2","UPIb4","UPIc","LPIc","UCIc","zscore"),with=F]
      data[,id:=paste0(GROUP,wk,wk2)]

      if(runName=="Norway"){
        if(file.exists(DashboardFolder("results","censoring.RDS"))){
          oldCensoring <- readRDS(DashboardFolder("results","censoring.RDS"))
          data <- merge(data,oldCensoring[,c("id","randomNoise"),with=F],by="id",all.x=TRUE)
        } else {
          data[,randomNoise:=as.numeric(NA)]
        }
        data[is.na(randomNoise),randomNoise:=as.numeric(sample(c(-3:3),size=.N,replace=TRUE))]
        saveRDS(data,DashboardFolder("results","censoring.RDS"))
        data[,nbc:=fhi::Censor(n=nbc,randomNoise=randomNoise,boundaries=list(data$UPIb2,data$UPIb4))]
      }
      minCorrectedWeek <- min(data[!is.na(UCIc)]$wk)
      data[is.na(UPIc) | UPIc < nbc,UPIc:=nbc]
      data[is.na(LPIc) | LPIc > nbc,LPIc:=nbc]
      data[wk >= minCorrectedWeek & UPIc==0,UPIc:=1]
      data[wk >= minCorrectedWeek & !is.na(UPIc),UPIc:=UPIc+3]
      data[wk >= minCorrectedWeek & !is.na(LPIc),LPIc:=LPIc-3]
      data[LPIc<0,LPIc:=0]

      data[,excess:=nbc-Pnb]

      saveRDS(data,DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"Data",paste0(runName,".RDS"))))
      if(runName=="Norway"){
        saveRDS(data,DashboardFolder("data_app","data.RDS"))
      }
      RunTemporaryGraphs(runName=runName,masterData=data,folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"Graphs")), yearWeek=RAWmisc::YearWeek(dateDataMinusOneWeek), dateData=dateData)
      
      RAWmisc::ProgressBarSet(pb,i)
    }

    allResults <- rbindlist(allResults)
    cat(sprintf("%s/%s/R/NORMOMO Saving data_processed.xlsx",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    openxlsx::write.xlsx(allResults,DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"Data","data_processed.xlsx")))

    ## extracting out raw data for later work
    masterData[,ageCat:=cut(age,c(0,4,14,64,200),include.lowest = TRUE)]
    masterData[,deathWeek:=momo:::isoweek(masterData$DoD, type="week")]
    masterData[,deathYear:=momo:::isoweek(masterData$DoD, type="year")]

    temp <- masterData[,.(N=.N),by=.(deathYear,deathWeek,FYLKE,ageCat)]
    skeleton <- data.table(expand.grid(
      deathYear=unique(temp$deathYear),
      deathWeek=unique(temp$deathWeek),
      FYLKE=unique(temp$FYLKE),
      ageCat=unique(temp$ageCat)))

    skeleton[,yd:=paste0(deathYear,"-",deathWeek)]
    temp[,yd:=paste0(deathYear,"-",deathWeek)]
    skeleton <- skeleton[yd %in% unique(temp$yd)]
    skeleton[,yd:=NULL]
    temp[,yd:=NULL]

    temp <- merge(temp,skeleton,all.y=T,by=c("deathYear","deathWeek","FYLKE","ageCat"))
    temp[is.na(N),N:=0]
    temp[,location:=as.character(FYLKE)]
    tempAll <- temp[,.(N=sum(N)),by=.(deathYear,deathWeek,ageCat)]
    tempAll[,location:="Norway"]
    temp <- temp[FYLKE %in% c(1:12,14:20)]
    temp[,FYLKE:=NULL]
    temp <- rbind(temp,tempAll)
    temp[,ageCat:=as.character(ageCat)]
    tempAll <- temp[,.(N=sum(N)),by=.(deathYear,deathWeek,location)]
    tempAll[,ageCat:="Total"]
    temp <- rbind(temp,tempAll)
    temp[,ageCat:=factor(ageCat,levels=c("[0,4]","(4,14]","(14,64]","(64,200]","Total"))]
    temp[,location:=factor(location,levels=c(1:12,14:20,"Norway"))]
    setcolorder(temp,c("deathYear","deathWeek","ageCat","location","N"))
    setorder(temp,deathYear,deathWeek,ageCat,location)
    
    cat(sprintf("%s/%s/R/NORMOMO Saving data_raw.xlsx",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    openxlsx::write.xlsx(temp,DashboardFolder("results",file.path(RAWmisc::YearWeek(dateDataMinusOneWeek),"Data","data_raw.xlsx"))) 

    cat(sprintf("%s/%s/R/NORMOMO Zipping results",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    ZipResults(
      folderResults=fhi::DashboardFolder("results"),
	  folderResultsYearWeek=fhi::DashboardFolder("results",RAWmisc::YearWeek(dateDataMinusOneWeek)),
	  folderResultsZip=fhi::DashboardFolder("results",paste0("archive_",RAWmisc::YearWeek(dateDataMinusOneWeek),".zip")),
	  folderDataAppZip=fhi::DashboardFolder("data_app",paste0("archive_",RAWmisc::YearWeek(dateDataMinusOneWeek),".zip"))
    )

    if(Sys.getenv("COMPUTER")=="smhb"){
      EmailNotificationOfNewResults()
    }
  }

  cat(sprintf("%s/%s/R/NORMOMO Exited successfully",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  quit(save="no", status=0)
  
}, warning=function(war) {
  cat("R/NORMOMO","WARN",paste0("UNEXPECTED exit with warning: ",war),"\n")
  quit(save="no", status=1)
}, error=function(err) {
  cat("R/NORMOMO","ERROR",paste0("UNEXPECTED exit with error: ",err),"\n")
  quit(save="no", status=1)
}, finally={
}
)

#saveRDS(dates,file="/log/status_normomo.RDS")

