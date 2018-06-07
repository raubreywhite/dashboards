con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/sykdomspuls_log STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_sykdomspuls_log/", export_all=FALSE)
} else {
  library(sykdomspulslog)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="sykdomspuls_log"
)

dir.create(fhi::DashboardFolder("results",lubridate::today()))

Colours <- function(n){
  retval <- c()
  for(i in 1:ceiling(n/8)){
    if(i==1){
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set1"))
    } else if(i==2){
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set2"))
    } else {
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set3"))
    }
  }
  return(retval[1:n])
}

files <- list.files(file.path(fhi::DashboardFolder("data_raw")))
if(length(files)<2) quit(save="no")

d <- vector("list",length=length(files))
for(i in 2:length(files)){

  normalFunction <- function(){
    return(fread(fhi::DashboardFolder("data_raw",files[i]),header=FALSE,sep=' '))
  }

  exceptionFunction <- function(err){
    newFile <- tempfile()
    con = file(fhi::DashboardFolder("data_raw",files[i]), "r")
    while ( TRUE ) {
      line = readLines(con, n = 1)
      line <- stringr::str_replace(line, ", ","")
      if ( length(line) == 0 ) {
        break
      }
      # if no arguments are provided, put them in
      line <- stringr::str_replace(line,"-  $","- NOARGS ")

      # save file
      cat(line,"\n",file=newFile, append=TRUE)
    }
    close(con)
    return(fread(newFile,header=FALSE,sep=' '))
  }
  d[[i]] <- tryCatch(normalFunction(), error=exceptionFunction, warning=exceptionFunction)
  if(ncol(d[[i]])==10){
    setnames(d[[i]],c("date","time","x","ipForwarded","x","ipRaw","x","page","x","args"))
    d[[i]] <- d[[i]][,-which(names(d[[i]])=="x"),with=F]
  } else {
    d[[i]] <- NULL
  }
}

d <- rbindlist(d)
d[,ipForwarded:=stringr::str_extract(ipForwarded,"^[0-9]*.[0-9]*.[0-9]*.[0-9]*")]

ips <- unique(d$ipForwarded)
ips <- ips[!ips %in% ips[stringr::str_detect(ips,"[0-9][0-9][0-9][0-9]$")]]

previousIPs <- c()
if(file.exists(fhi::DashboardFolder("data_clean","ips.RDS"))){
  previousLocations <- readRDS(file=fhi::DashboardFolder("data_clean","ips.RDS"))
  previousIPs <- previousLocations$ipForwarded

  ips <- ips[!ips %in% previousIPs]
}

if(length(ips)>0){
  locations <- list()
  desiredIPS <- split(ips, ceiling(seq_along(ips)/20))
  for(i in 1:length(desiredIPS)){
    print(i)
    try({
      locations[[i]] <- data.table(rgeolocate::ip_api(desiredIPS[[i]]))
      locations[[i]][,ipForwarded:=desiredIPS[[i]]]
    },TRUE)
    Sys.sleep(2)
  }
  locations <- rbindlist(locations)
  if(exists("previousLocations")){
    locations <- rbind(locations,previousLocations)
  }
  saveRDS(locations,file=fhi::DashboardFolder("data_clean","ips.RDS"))
} else if(exists("previousLocations")){
  locations <- previousLocations
} else {
  stop("No locations (IPs) available")
}

d <- merge(d,locations,by="ipForwarded")
setorder(d,ipForwarded,ipRaw,date,time)
d <- d[status=="success"]
d[page=="/test",session:=c(1:.N),by=ipForwarded]
d[,session:=zoo::na.locf(session)]
d <- d[page!="/test"]
xtabs(~d$country_code)
d <- d[country_code=="NO"]
xtabs(~d$city_name)
xtabs(~d$region_name)
xtabs(~d$page)

xtabs(~d$date)
d[,yrwk:=RAWmisc::YearWeek(date)]
xtabs(~d$yrwk)
d <- d[date>="2017-08-15" & !(city_name=="Oslo" & isp=="UNINETT AS")]


page <- d[,.(pageVisits=.N),by=.(yrwk,page)]
arg <- d[,.(pageVisits=.N),by=.(yrwk,args,page)]

arg[,xname:=stringr::str_extract(args,"xname=[a-zA-Z0-9]*")]
arg[,xname:=stringr::str_replace(xname,"xname=","")]

arg[,xage:=stringr::str_extract(args,"xage=[a-zA-Z0-9]*")]
arg[,xage:=stringr::str_replace(xage,"xage=","")]

arg[,xtype:=stringr::str_extract(args,"xtype=[a-zA-Z0-9]*")]
arg[,xtype:=stringr::str_replace(xtype,"xtype=","")]

people <- unique(d[,c("ipForwarded","region_name","yrwk")])
peopleTotal <- unique(d[,c("ipForwarded","region_name")])
peopleTotal <- peopleTotal[,.(uniqueIPs=.N),by=.(region_name)]
people <- people[,.(uniqueIPs=.N),by=.(region_name,yrwk)]
q <- ggplot(people,aes(x=yrwk,y=uniqueIPs,fill=region_name))
q <- q + geom_bar(stat="identity",colour="black",alpha=0.75)
q <- q + scale_fill_manual(values=Colours(length(unique(peopleTotal$region_name))))
q <- q + labs(caption=sprintf("%s unique IPs in total",sum(peopleTotal$uniqueIPs)))
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
RAWmisc::saveA4(q, file.path(fhi::DashboardFolder("results",lubridate::today()),"unique_ips_per_week.png"))

