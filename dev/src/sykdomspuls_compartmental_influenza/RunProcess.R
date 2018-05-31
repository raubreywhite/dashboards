con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/sykdomspuls_compartmental_influenza STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(optimParallel))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/git/dashboards_compartmental_influenza/", export_all=FALSE)
} else {
  library(sykdomspulscompartmentalinfluenza)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="sykdomspuls_compartmental_influenza"
)


dir.create(fhi::DashboardFolder("results",lubridate::today()))
#cl <- makeCluster(detectCores()); setDefaultCluster(cl = cl)

x <- SetupCPPAndStructure()
regions <- x[["regions"]]
d <- x[["d"]]

seasons <- unique(d$season)[-1]
s=seasons[3]

a <- Sys.time()
res <- RunSim(
  param=c(0.5,0.6,0.7,0.8,0.9),
  regions=regions,
  betaShape=5,
  betaScale=40,
  doctorVisitingProb=0.3,
  d=d,
  s=s,
  startWeek=30,
  verbose=F)
b <- Sys.time()
b-a

a <- Sys.time()
(x <- OptimFunction(c(0.5,0.40,0.5),
             regions=regions,
             d=d,
             s=s,
             startWeek=30))
b <- Sys.time()
b-a

OptimizeSeason <- function(regions,doctorVisitingProb=0.3,d,s,startWeek=30,parallel=F){
  if(parallel){
    useFunction = optimParallel
  } else {
    useFunction = optim
  }
  p <- useFunction(par=c(0.5,0.4,0.52),
             fn=OptimFunction,
             regions=regions,
             d=d,
             s=s,
             startWeek=startWeek,
             method="L-BFGS-B",
             lower=c(0.1,0.1,0.4),
             upper=c(1,1,0.7),
              control=list(
                trace=6,
                maxit=5
              ))
  return(p$par)
}
cl <- makeCluster(detectCores()); setDefaultCluster(cl = cl)
b <- OptimizeSeason(regions=regions,doctorVisitingProb=0.3,d=d,s=s,startWeek=40,parallel=F)
b <- OptimizeSeason(regions=regions,doctorVisitingProb=0.3,d=d,s=s,startWeek=40,parallel=T)

#0850



CalcR0 <- function(beta){
  r0 <- (beta*CONFIG_PAR$gamma) * (0.5 * 0.33 + 0.67)
  return(r0)
}

skeleton <- data.frame(season=NA,beta=NA,shiftX)
retval <- vector("list",length=length(seasons))
retval <- vector("list",length=2)
pb <- RAWmisc::ProgressBarCreate(min=1,max=length(retval))
for(i in 1:length(retval)){
  RAWmisc::ProgressBarSet(pb,i)
  skeleton$season <- seasons[i]
  temp <- OptimizeSeason(d=d,s=seasons[i])
  skeleton$beta <- temp[1]
  skeleton$shiftX <- temp[2]
  
  retval[[i]] <- skeleton
}

retval <- rbindlist(retval)
retval[,R0:=CalcR0(beta)]

sims <- vector("list",length=nrow(retval))
sims <- vector("list",length=2)
pb <- RAWmisc::ProgressBarCreate(min=1,max=length(sims))
for(i in 1:length(sims)){
  RAWmisc::ProgressBarSet(pb,i)
  
  temp <- RunSim(
    retval$beta[i],
    d=d,
    s=retval$season[i],
    startWeek=30,
    shiftX=retval$shiftX,
    doctorVisitingProb=0.3)
  
  nat <- temp[,.(
  n=sum(nMinusOffSeason),
  S=sum(S),
  E=sum(E),
  SI=sum(SI),
  AI=sum(AI),
  R=sum(R),
  INCIDENCE=sum(INCIDENCE)
  ),by=.(x,wkyr,week,season)]
  
  sims[[i]] <- nat
}

sims <- rbindlist(sims)

for(s in unique(sims$season)){
  q <- ggplot(sims[season==s], aes(x=x))
  q <- q + geom_point(mapping=aes(y=n))
  q <- q + geom_line(mapping=aes(y=SI*doctorVisitingProb),col="red")
  q <- q + geom_line(mapping=aes(y=SI),col="orange")
  q <- q + facet_wrap(~season)
  
  RAWmisc::saveA4(q,
                  filename=fhi::DashboardFolder("results",
                                                sprintf("%s/%s.png",
                                                        lubridate::today(),
                                                        stringr::str_replace(s,"/","-"))))
}



quit(save="no")

d <- readRDS(fhi::DashboardFolder("data_raw","resYearLineMunicip_influensa.RDS"))[age=="Totalt"]
d[week>=30,season:=sprintf("%s/%s",year,year+1)]
d[is.na(season),season:=sprintf("%s/%s",year-1,year)]
d[week %in% c(20:40),offSeason:=mean(n),by=.(season,location)]
d[,offSeason:=mean(offSeason,na.rm=T),by=.(season,location)]
d[,n:=floor(n-offSeason)]
d[n<0,n:=0]

seasons <- unique(d$season)[-1]

SetupCPPAndStructure()

for(s in seasons[2]){
  print(s)
  temp <- RunSim(
      0.74,
      d=d,
      s=s,
      startWeek=30,
      doctorVisitingProb=0.3)
  nat <- temp[!is.na(S),.(
    n=sum(n),
    S=sum(S),
    E=sum(E),
    SI=sum(SI),
    AI=sum(AI),
    R=sum(R),
    INCIDENCE=sum(INCIDENCE)
    ),by=.(x,wkyr,week,season)]
  
  q <- ggplot(nat, aes(x=x))
  q <- q + geom_point(mapping=aes(y=0.75*n))
  q <- q + geom_line(mapping=aes(y=SI*doctorVisitingProb),col="red")
  q <- q + geom_line(mapping=aes(y=SI),col="orange")
    RAWmisc::saveA4(q,
                  filename=fhi::DashboardFolder("results",
                                                sprintf("%s/%s.png",
                                                        lubridate::today(),
                                                        stringr::str_replace(s,"/","-"))))
}

s=seasons[4]
SetupCPPAndStructure()
b <- OptimizeSeason(regions=regions,doctorVisitingProb=0.3,d=d,s=s)

x <- b-0.01
x[3] <- x[3]-0.07
x[4] <- x[4]-0.035
x[5] <- x[5]-0.01
x <- b

SetupCPPAndStructure()

temp <- RunSim(
      param=rep(0.2,5),
      regions=regions,
      betaShape=1000,
      betaScale=60,
      doctorVisitingProb=0.2,
      d=d,
      s=s,
      startWeek=30,
      verbose=T)

nat <- temp[!is.na(S),.(
  pop=mean(pop),
  n=sum(n),
  S=sum(S),
  E=sum(E),
  SI=sum(SI),
  AI=sum(AI),
  R=sum(R),
  INCIDENCE=sum(INCIDENCE),
  doc_INCIDENCE=sum(doc_INCIDENCE)
  ),by=.(x,wkyr,week,season,age,region)]

q <- ggplot(nat[x>10], aes(x=x))
q <- q + geom_point(mapping=aes(y=(n/pop*100000)))
q <- q + geom_line(mapping=aes(y=(doc_INCIDENCE/pop*100000)),col="red")
q <- q + facet_grid(age~region,scales="free")
#q <- q + geom_line(mapping=aes(y=SI),col="orange")
q

q <- ggplot(nat, aes(x=x))
q <- q + geom_point(mapping=aes(y=log(n)))
q <- q + stat_smooth(mapping=aes(y=log(n)),col="red")
#q <- q + geom_line(mapping=aes(y=(doc_INCIDENCE)),col="red")
q <- q + facet_grid(~region)
#q <- q + geom_line(mapping=aes(y=SI),col="orange")
q


d <- readRDS(fhi::DashboardFolder("data_raw","resYearLineMunicip_influensa.RDS"))
d[,location:=NULL]
d[,pop:=NULL]
setnames(d,"county","location")
d <- merge(d,regions,by="location")
d <- merge(d,pop,by=c("age","location"))
d <- d[,.(n=sum(n),pop=sum(pop)),by=.(x,week,year,wkyr,region,age)]
setnames(d,"region","location")
d[week>=30,season:=sprintf("%s/%s",year,year+1)]
d[is.na(season),season:=sprintf("%s/%s",year-1,year)]
d[week %in% c(25:35),offSeason:=mean(n),by=.(season,location)]
d[,offSeason:=mean(offSeason,na.rm=T),by=.(season,location)]
d[,nMinusOffSeason:=floor(n-offSeason)]
d[nMinusOffSeason<0,nMinusOffSeason:=0]

q <- ggplot(d[season==s], aes(x=x))
q <- q + geom_point(mapping=aes(y=(n/pop*10000)))
#q <- q + stat_smooth(mapping=aes(y=log(n)),col="red")
#q <- q + geom_line(mapping=aes(y=(doc_INCIDENCE)),col="red")
q <- q + facet_grid(age~location)
#q <- q + geom_line(mapping=aes(y=SI),col="orange")
q

