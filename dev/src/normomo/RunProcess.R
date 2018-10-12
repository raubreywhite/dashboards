custom_delayMOMO_richard <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  setDT(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  modellingWeeks <- MOMO::momoAttr$PRWEEK:MOMO::momoAttr$WEEK2
  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+MOMO::momoAttr$delayCorr*2]

  aggr$sin52 <- sin(aggr$wk*2*pi/52.17857)
  aggr$cos52 <- cos(aggr$wk*2*pi/52.17857)

  aggr$sin26 <- sin(aggr$wk*2*pi/26)
  aggr$cos26 <- cos(aggr$wk*2*pi/26)

  aggr[,diff_WR1_minus_WR0:=WR1-WR0]

  # the first year needs to be the same as the second
  # the last year needs to be the same as the last
  # this is so we get stable estimates
  aggr[,modellingYear:=YoDi]
  #print(xtabs(~aggr$modellingYear))
  minYear <- min(aggr[aggr$wk %in% modellingWeeks,]$YoDi)
  maxYear <- max(aggr[aggr$wk %in% modellingWeeks,]$YoDi)
  #aggr[YoDi<=minYear,modellingYear:=minYear+1]
  #aggr[YoDi>=maxYear,modellingYear:=maxYear-1]
  aggr[,modellingYear:=modellingYear-2000]

  aggr[,summer:=WoDi %in% 21:39]
  aggr[YoDi>=maxYear,modellingYear:=maxYear-1]

  for(r in MOMO::momoAttr$delayCorr:0){
    #print(r)
    aggr[,CCC:=shift(closed,n=r)]
    aggr[,a:=get(sprintf("WR%s",r))/nb]
    aggr[,perc:=get(sprintf("WR%s",r))/nbr]

    if(r< (MOMO::momoAttr$delayCorr)){
      aggr[,shifted1:=shift(get(sprintf("pred%s",r+1)),n=1)]
    }
    if(r< (MOMO::momoAttr$delayCorr-1)){
      aggr[,shifted2:=shift(get(sprintf("pred%s",r+2)),n=2)]
    }

    if(r==0){
      form <- sprintf("nb2 ~ modellingYear+shifted2+WR%s+closed + sin52 + cos52", r)
      formLow <- sprintf("nb2 ~ modellingYear + shifted2+closed + sin52 + cos52")

      fit <- glm(as.formula(form), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
      if (od > 1) fit <- glm(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,], family=quasipoisson)
      #print(summary(fit))
      p <- predict(fit,aggr, type="response")
      if(!fit$converged | max(p,na.rm=T)>(max(aggr$nb,na.rm=T)*1.5) | min(p,na.rm=T)<(min(aggr$nb,na.rm=T)/1.5)){
        print("DOWNGRADING MODEL DUE TO ERROR")
        fit <- glm(as.formula(formLow), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      }
    } else if(r==1){
      form <- sprintf("nb2 ~ modellingYear+shifted1+WR%s+closed + sin52 + cos52", r)
      formLow <- sprintf("nb2 ~ modellingYear + shifted1+closed + sin52 + cos52")

      fit <- glm(as.formula(form), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
      if (od > 1) fit <- glm(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,], family=quasipoisson)
      #print(summary(fit))
      p <- predict(fit,aggr, type="response")
      if(!fit$converged | max(p,na.rm=T)>(max(aggr$nb,na.rm=T)*1.5) | min(p,na.rm=T)<(min(aggr$nb,na.rm=T)/1.5)){
        print("DOWNGRADING MODEL DUE TO ERROR")
        fit <- glm(as.formula(formLow), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      }
    } else {
      # original
      form1 <- sprintf("a ~ CCC + wk")
      form <- sprintf("nb2 ~ WR%s + Pa + wk",r)

      fit1 <- glm(as.formula(form1), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      p <- predict(fit1,aggr, type="response")
      aggr[,Pa:=p]

      fit <- glm(as.formula(form), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
      if (od > 1) fit <- glm(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,], family=quasipoisson)
    }

    p <- predict(fit,aggr, type="response")
    aggr[,(sprintf("pred%s",r)):=p]

    p <- predict(fit,aggr,se.fit=TRUE)$se.fit
    aggr[,(sprintf("stdp%s",r)):=p]

    # creating VpWRXX (theoretical)
    aggr[,(sprintf("VpWR%s",r)):=pmax(0,get(sprintf("pred%s",r))*od) + (get(sprintf("pred%s",r))*get(sprintf("stdp%s",r)))^2]

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    aggr[,(UPIvar):=as.numeric(NA)]
    aggr[,(LPIvar):=as.numeric(NA)]

    aggr[,(UCIvar):=as.numeric(NA)]
    aggr[,(LCIvar):=as.numeric(NA)]

    # Prediction Interval
    aggr[,(UPIvar):=(get(sprintf("pred%s",r))^(2/3) + zvalue*((4/9)*(get(sprintf("pred%s",r))^(1/3))*(od+(get(sprintf("stdp%s",r))^2)*(get(sprintf("pred%s",r)))))^(1/2))^(3/2)]
    aggr[,(LPIvar):=(get(sprintf("pred%s",r))^(2/3) - zvalue*((4/9)*(get(sprintf("pred%s",r))^(1/3))*(od+(get(sprintf("stdp%s",r))^2)*(get(sprintf("pred%s",r)))))^(1/2))^(3/2)]

    aggr[,(UCIvar):=get(sprintf("pred%s",r)) + zvalue*get(sprintf("stdp%s",r))]
    aggr[,(LCIvar):=get(sprintf("pred%s",r)) - zvalue*get(sprintf("stdp%s",r))]
  }

  aggr[,VpWR:=0]
  aggr[,delay:=max(wk)-wk]
  for(r in 0:MOMO::momoAttr$delayCorr){
    aggr[delay==r,VpWR:=get(sprintf("VpWR%s",r))]
    aggr[delay==r,pred:=get(sprintf("pred%s",r))]
    aggr[delay==r,UPIc:=get(sprintf("UPI%s",r))]
    aggr[delay==r,LPIc:=get(sprintf("LPI%s",r))]
    aggr[delay==r,UCIc:=get(sprintf("UCI%s",r))]
    aggr[delay==r,LCIc:=get(sprintf("LCI%s",r))]
  }
  #** we generate the CORRECTED number of death
  aggr[wk<=MOMO::momoAttr$WEEK2,nbc:=nb]
  aggr[MOMO::momoAttr$WEEK2 < wk & wk<= MOMO::momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  aggr[,GROUP:=MOMO::momoAttr$group]

  aggr <- as.data.frame(aggr)
  return(aggr)
}




con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/NORMOMO STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_normomo/", export_all=FALSE)
  forceRun <- TRUE
} else {
  library(normomo)
  forceRun <- FALSE
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="normomo"
)

# Set up data

hfile <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
hfile$date <- as.Date(hfile$date)
#fwrite(hfile,file=DashboardFolder("data_clean","bank_holidays.txt"))

info <- GetDataInfo()

masterData <- GetData(
  fDone=info[["fDone"]],
  f=info[["f"]],
  forceRun=forceRun
  )

# Set up folders
SetupFolders(dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]])

# Plan out analyses
stack <- GenerateStack(
  f=info[["f"]],
  dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]],
  dateData=info[["dateData"]]
  )

stackStatistics <- stack[["stackStatistics"]]
stackAnalyses <- stack[["stackAnalyses"]]

#### STATISTICS

print("STARTING STATISTICS")

pb <- RAWmisc::ProgressBarCreate(min=0,max=nrow(stackStatistics),flush=TRUE)
allPlotData <- vector("list",length=nrow(stackStatistics))
dataAnalysis <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age"),with=F])

for(i in 1:nrow(stackStatistics)){
  RAWmisc::ProgressBarSet(pb,i)

  s <- stackStatistics[i,]
  #s <- stackStatistics[dateData=="2018-03-13" & delayVersion=="richard"]

  df <- NULL
  if(s[["delayVersion"]]=="richard"){
    df <- custom_delayMOMO_richard
  } else {
    #next
  }

  MOMO::SetOpts(
    DoA = s[["dateData"]],
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = s[["runName"]],
    source = "FHI",
    MDATA = dataAnalysis[dataAnalysis$age %in% 15:64,],
    HDATA = hfile,
    INPUTDIR = s[["MOMOFolderInput"]],
    WDIR = tempdir(),
    back = 7,
    WWW = 290,
    Ysum = s[["MOMOYsum"]],
    Wsum = 40,
    plotGraphs = FALSE,
    delayVersion = s[["delayVersion"]],
    delayFunction = df,
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  MOMO::RunMoMo()

  dataToSave <- rbindlist(MOMO::dataExport$toSave, fill=TRUE)

  #dataToSave[GROUP=="Total" & nbc!=nb]

  data <- CleanExportedMOMOData(
    data=dataToSave,
    s=s
  )

  allPlotData[[i]] <- data
  allPlotData[[i]][,DoA:=s[["dateData"]]]
  allPlotData[[i]][,delayVersion:=s[["delayVersion"]]]
}

allPlotData <- rbindlist(allPlotData)[!is.na(excess)]

print(allPlotData)

RunGraphsStatistics(
  runName=s[["runName"]],
  allPlotData=allPlotData,
  folder=s[["MOMOFolderResultsGraphsStatistics"]],
  yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
  dateData=max(s[["dateData"]][[1]])
)

### ANALYSES
print("STARTING ANALYSES")

pb <- RAWmisc::ProgressBarCreate(min=0,max=nrow(stackAnalyses),flush=TRUE)
allResults <- vector("list",100)

for(i in 1:nrow(stackAnalyses)){
  RAWmisc::ProgressBarSet(pb,i)

  s <- stackAnalyses[i,]

  if(s[["runName"]]=="Norway"){
    dataAnalysis <- as.data.frame(masterData[!is.na(age),
                                             c("DoD","DoR","age"),with=F])
    plotGraphs <- TRUE
  } else {
    dataAnalysis <- as.data.frame(masterData[!is.na(age) & FYLKE==s[["fylke"]],
                                             c("DoD","DoR","age"),with=F])
    plotGraphs <- FALSE
  }
  #saveRDS(dataAnalysis,file=s[["data_clean_name"]])
  #saveRDS(dataAnalysis,file=DashboardFolder("data_clean","data.RDS"))
  #fwrite(dataAnalysis,file=DashboardFolder("data_clean","data.txt"))


  MOMO::SetOpts(
    DoA = s[["dateData"]],
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = s[["runName"]],
    source = "FHI",
    MDATA = dataAnalysis,
    HDATA = hfile,
    INPUTDIR = s[["MOMOFolderInput"]],
    WDIR = s[["MOMOFolderResults"]],
    back = 7,
    WWW = 290,
    Ysum = s[["MOMOYsum"]],
    Wsum = 40,
    plotGraphs = s[["plotGraphs"]],
    delayVersion = "richard",
    delayFunction = custom_delayMOMO_richard,
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  MOMO::RunMoMo()

  dataToSave <- rbindlist(MOMO::dataExport$toSave, fill=TRUE)

  data <- CleanExportedMOMOData(
    data=dataToSave,
    s=s
  )

  allResults[[i]] <- dataToSave
  allResults[[i]][,name:=s[["runName"]]]

  saveRDS(data,s[["MOMOFolderResultsData"]])
  if(s[["runName"]]=="Norway"){
    saveRDS(data,DashboardFolder("data_app","data.RDS"))
  }

  RunGraphsDeaths(
    runName=s[["runName"]],
    data=data,
    folder=s[["MOMOFolderResultsGraphsStatus"]],
    yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
    dateData=max(s[["dateData"]][[1]]),
    dateReliable=max(s[["dateData"]][[1]])-7
  )

}

allResults <- rbindlist(allResults)
cat(sprintf("%s/%s/R/NORMOMO Saving data_processed.xlsx",Sys.time(),Sys.getenv("COMPUTER")),"\n")
openxlsx::write.xlsx(allResults,DashboardFolder("results",file.path(RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),"data","data_processed.xlsx")))

## Grid graph
RunStatusTiles(allResults=allResults,
               folder=s[["MOMOFolderResultsGraphsStatus"]],
               yearWeek=RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),
               dateData=info[["dateData"]])

SavingRawData(
  dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]],
  masterData=masterData
)

cat(sprintf("%s/%s/R/NORMOMO Zipping results",Sys.time(),Sys.getenv("COMPUTER")),"\n")
ZipResults(
  folderResults=fhi::DashboardFolder("results"),
  folderResultsYearWeek=fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]])),
  folderResultsZip=fhi::DashboardFolder("results",paste0("archive_",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),".zip")),
  folderDataAppZip=fhi::DashboardFolder("data_app",paste0("archive_",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),".zip"))
)

if(Sys.getenv("COMPUTER")=="smhb"){
  TEST_EMAILS <- FALSE
} else {
  TEST_EMAILS <- TRUE
}
EmailInternal(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))),
              isTest=TEST_EMAILS)
EmailSSI(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))),
         dateReliable=info$dateData-CONFIG$WEEKS_UNRELIABLE*7,
         isTest=TEST_EMAILS)

CreateLatestDoneFile(f=info$f)

cat(sprintf("%s/%s/R/NORMOMO Exited successfully",Sys.time(),Sys.getenv("COMPUTER")),"\n")
quit(save="no", status=0)
