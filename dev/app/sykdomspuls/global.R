library(flexdashboard)
library(ggplot2)
library(ggrepel)
library(data.table)
library(fhi)

# load data in 'global' chunk so it can be shared by all users of the dashboard
resYearLine <- readRDS("/data_app/sykdomspuls/resYearLine.RDS")
CONFIG <- readRDS("/data_app/sykdomspuls/config.RDS")

GetLatestDataTime <- function() {
  if(file.exists("/data_app/sykdomspuls/done.txt")){
    return(file.info("/data_app/sykdomspuls/done.txt")$mtime[1])
  } else {
  	return(Sys.time())
  }
}
latestDataTime <- GetLatestDataTime()

# specifying types and ages from config file
weeklyTypes <- dailyTypes <- CONFIG$SYNDROMES[CONFIG$SYNDROMES %in% CONFIG$SYNDROMES_ALERT_INTERNAL]
weeklyAges <- dailyAges <- CONFIG$AGES

dateMax <- max(resYearLine$displayDay)
dateMinRestrictedRecent <- dateMax-365
dateMinRestrictedLine <- dateMax-365*15

outbreaks <- readRDS("/data_app/sykdomspuls/outbreaks.RDS")
resRecentLine <- readRDS("/data_app/sykdomspuls/resRecentLine.RDS")[
  date>=dateMinRestrictedRecent & date<=dateMax & 
  type %in% weeklyTypes]
resYearLineMunicip <- readRDS("/data_app/sykdomspuls/resYearLineMunicip.RDS")[
  displayDay>=dateMinRestrictedLine & displayDay<=dateMax & 
  type %in% weeklyTypes]
resRecentLine[,date:=as.Date(date)]

#resYearLineMunicip[location=="municip1151"]

dailyCounties <- unique(resRecentLine$location)
names(dailyCounties)  <- unique(resRecentLine$locationName)
weeklyCounties <- unique(resYearLine$location)
names(weeklyCounties) <- unique(resYearLine$locationName)
weeklyWkyr <- rev(unique(resYearLine$wkyr))

municipToCounty <- unique(resYearLineMunicip[,c("location","locationName","county"),with=F])

resRecentLineStack <- unique(resRecentLine[,c("type","location","age"),with=F])
resYearLineStack <- unique(resYearLine[,c("type","location","age"),with=F])
resYearLineMunicipStack <- unique(resYearLineMunicip[,c("type","location","age","county"),with=F])

# removing external syndromes
outbreaks[["df"]] <- outbreaks[["df"]][type %in% weeklyTypes]
outbreaks[["dk"]] <- outbreaks[["dk"]][type %in% weeklyTypes]

weeklyValues <- c(
  "Konsultasjoner"="consults",
  "1 uke eksess"="excess1"
)
