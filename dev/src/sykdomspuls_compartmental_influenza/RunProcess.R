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


#if(RAWmisc::IsFileChanged(
#  fileToCheck=fhi::DashboardFolder("data_raw","resYearLineMunicip_influensa.RDS"),
#  fileDetails=fhi::DashboardFolder("data_raw","details_resYearLineMunicip_influensa.RDS"))
d <- readRDS(fhi::DashboardFolder("data_raw","resYearLineMunicip_influensa.RDS"))
d <- d[age=="Totalt",.(n=sum(n),pop=sum(pop)),by=.(x,week,year,wkyr,county)]
setnames(d,"county","location")
d[week>=30,season:=sprintf("%s/%s",year,year+1)]
d[is.na(season),season:=sprintf("%s/%s",year-1,year)]
d[week %in% c(25:35),offSeason:=mean(n),by=.(season,location)]
d[,offSeason:=mean(offSeason,na.rm=T),by=.(season,location)]
d[,nMinusOffSeason:=floor(n-offSeason)]
d[nMinusOffSeason<0,nMinusOffSeason:=0]

seasons <- unique(d$season)[-1]
s=seasons[3]

regions <- SetupCPPAndStructure()

a <- Sys.time()
res <- RunSim(
  param=c(0.5,0.6,0.7,0.8,0.9),
  regions=regions,
  betaFlat=0.2,
  betaDecreaseSpeed=0.005,
  doctorVisitingProb=0.3,
  d=d,
  s=s,
  startWeek=40)
b <- Sys.time()
b-a

(a <- OptimFunction(c(0.2,0.007,rep(0.52,5)),
             regions=regions,
             d=d,
             s=s,
             startWeek=30))

OptimizeSeason <- function(regions,doctorVisitingProb=0.3,d,s,startWeek=30){
  p <- optimParallel(par=c(0.2,0.007,rep(0.52,5)),
             fn=OptimFunction,
             regions=regions,
             d=d,
             s=s,
             startWeek=startWeek,
             method="L-BFGS-B",
             lower=c(0.05,0.001,rep(0.4,5)),
             upper=c(0.3,0.015,rep(0.7,5)),
              control=list(
                trace=6,
                maxit=5
              ))
  return(p$par)
}
cl <- makeCluster(detectCores()); setDefaultCluster(cl = cl)
b <- OptimizeSeason(regions=regions,doctorVisitingProb=0.3,d=d,s=s,startWeek=40)

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

x <- c(0.2,0.007,rep(0.52,5))
x <- b
temp <- RunSim(
      param=x[3:7],
      regions=regions,
      betaFlat=x[1],
      betaDecreaseSpeed=x[2],
      doctorVisitingProb=0.2,
      d=d,
      s=s,
      startWeek=40)

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
  ),by=.(x,wkyr,week,season,region)]

q <- ggplot(nat, aes(x=x))
q <- q + geom_point(mapping=aes(y=(n/pop*100000)))
q <- q + geom_line(mapping=aes(y=(doc_INCIDENCE/pop*100000)),col="red")
q <- q + facet_grid(~region)
#q <- q + geom_line(mapping=aes(y=SI),col="orange")
q

