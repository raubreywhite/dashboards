if(.Platform$OS.type == "unix"){
  HOME <- "/src/sykdomspuls"
  PLUMB <- "plumb"
  DATA <- "/data_app/sykdomspuls"
} else {
  HOME <- "C:/Sykdomspulsen"
  PLUMB <- file.path("src","R","plumb")
  DATA <- "C:/Sykdomspulsen/data"
}

setwd(HOME)

suppressMessages(library(data.table))
suppressMessages(library(plumber))

SubstrRight.int <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
SubstrRight <- Vectorize(SubstrRight.int)

if(!file.exists(file.path(DATA,"log"))) dir.create(file.path(DATA,"log"))

CONFIG <- readRDS(file.path(DATA,"config.RDS"))
types <- CONFIG$SYNDROMES[CONFIG$SYNDROMES %in% CONFIG$SYNDROMES_ALERT_EXTERNAL]
typesShort <- CONFIG$SYNDROMES_SHORT[CONFIG$SYNDROMES_SHORT %in% CONFIG$SYNDROMES_ALERT_EXTERNAL]
ages <- CONFIG$AGES

dr <- readRDS(file.path(DATA,"resRecentLine.RDS"))
dateMax <- max(dr$date)
dateMin <- as.character(as.Date(dateMax)-365)
dr <- dr[date>=dateMin]
d <- readRDS(file.path(DATA,"resYearLine.RDS"))
dk <- readRDS(file.path(DATA,"resYearLineMunicip.RDS"))

dr <- dr[type %in% types]
d <- d[type %in% types]
dk <- dk[type %in% types]

#dr[,age:=gsub("\\+","p",age)]
#d[,age:=gsub("\\+","p",age)]
#dk[,age:=gsub("\\+","p",age)]

dr[,xRaw:=1:.N,by=.(type,location,age)]
d[,xRaw:=1:.N,by=.(type,location,age)]
dk[,xRaw:=1:.N,by=.(type,location,age)]

dr[,statusNum:=0]
dr[status=="Medium",statusNum:=1]
dr[status=="High",statusNum:=2]

d[,statusNum:=0]
d[status=="Medium",statusNum:=1]
d[status=="High",statusNum:=2]

dk[,statusNum:=0]
dk[status=="Medium",statusNum:=1]
dk[status=="High",statusNum:=2]

setnames(d,"displayDay","date")
setnames(dk,"displayDay","date")

dr[,year:=as.numeric(strftime(date,"%Y"))]
dr[,month:=as.numeric(strftime(date,"%m"))]
dr[,day:=as.numeric(strftime(date,"%d"))]

dr[,lab1:=0]
dr[(day%%2-1)==0,lab1:=1]

dr[,lab2:=0]
dr[(day%%7-1)==0,lab2:=1]

dr[,lab3:=0]
dr[(day%%7-1)==0,lab3:=1]

dr[,lab4:=0]
dr[day==1,lab4:=1]

dr[,lab5:=0]
dr[day==1,lab5:=1]

dr[,lab6:=0]
dr[(month%%2-1)==0 & day==1,lab6:=1]

dr[,lab7:=0]
dr[(month%%3-1)==0 & day==1,lab7:=1]

dr[,vlines:=0]
dr[month==1 & day==1,vlines:=1]
dr[,label:=strftime(date,"%d/%m/%y")]

d[,lab1:=1]

d[,lab2:=0]
d[(week%%2-1)==0,lab2:=1]

d[,lab3:=0]
d[(week%%4-1)==0,lab3:=1]

d[,lab4:=0]
d[(week%%8-1)==0,lab4:=1]

d[,lab5:=0]
d[(week%%13-1)==0,lab5:=1]

d[,lab6:=0]
d[(week%%26-1)==0,lab6:=1]

d[,lab7:=0]
d[(week%%52-1)==0,lab7:=1]

d[,vlines:=lab7]
d[,label:=paste0(week,"/",SubstrRight(year,2))]

dk[,lab1:=1]

dk[,lab2:=0]
dk[(week%%2-1)==0,lab2:=1]

dk[,lab3:=0]
dk[(week%%4-1)==0,lab3:=1]

dk[,lab4:=0]
dk[(week%%8-1)==0,lab4:=1]

dk[,lab5:=0]
dk[(week%%13-1)==0,lab5:=1]

dk[,lab6:=0]
dk[(week%%26-1)==0,lab6:=1]

dk[,lab7:=0]
dk[(week%%52-1)==0,lab7:=1]

dk[,vlines:=lab7]
dk[,label:=paste0(week,"/",SubstrRight(year,2))]

drStack <- unique(dr[,c("type","location","age"),with=F])
dStack <- unique(d[,c("type","location","age"),with=F])
dkStack <- unique(dk[,c("type","location","age","county"),with=F])

outbreaks <- readRDS(file.path(DATA,"outbreaks.RDS"))
for(i in c("df","dk")){
  outbreaks[[i]] <- outbreaks[[i]][type %in% types & !is.na(sumCum)]
  outbreaks[[i]][,numeric:=as.numeric(meanZScore)]
  setorder(outbreaks[[i]],-wkyr,-numeric)
  outbreaks[[i]][,numeric:=NULL]
  outbreaks[[i]][,Sykdom:=factor(type,levels=types)]
  levels(outbreaks[[i]]$Sykdom) <- names(types)
}

setcolorder(outbreaks[["df"]],c("type","wkyr","Sykdom","age","High","meanZScore","sumCum"))
setnames(outbreaks[["df"]],c("type","wkyr","Sykdom","Alder","Fylker","Gj. Z verdi","Eksess tilfeller"))
setcolorder(outbreaks[["dk"]],c("type","wkyr","Sykdom","age","countyName","High","meanZScore","sumCum"))
setnames(outbreaks[["dk"]],c("type","wkyr","Sykdom","Alder","Fylke","Kommuner","Gj. Z verdi","Eksess tilfeller"))

r <- plumb(file.path(PLUMB,"plumb.R"))
r$run(host='0.0.0.0',port=8000)

# curl http://localhost:8000/test?x=3

# curl http://localhost:10001/api/test?x=3
# curl http://localhost:10002/test?x=3

# curl http://localhost:10001/api/v
# curl http://localhost:10002/v




