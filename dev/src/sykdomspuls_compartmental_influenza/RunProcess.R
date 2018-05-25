con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/sykdomspuls_compartmental_influenza STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_compartmental_influenza/", export_all=FALSE)
} else {
  library(sykdomspulscompartmentalinfluenza)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="sykdomspuls_compartmental_influenza"
)

d <- readRDS(fhi::DashboardFolder("data_raw","resYearLineMunicip_influensa.RDS"))
d[week>=30,season:=sprintf("%s/%s",year,year+1)]
d[is.na(season),season:=sprintf("%s/%s",year-1,year)]

seasons <- unique(d$season)[-1]

dirTmp <- "/tmp/sykdomspuls_compartmental_influenza"
dirSrc <- system.file("src", package = "sykdomspulscompartmentalinfluenza")
dirData <- system.file("extdata", package = "sykdomspulscompartmentalinfluenza")

unlink(dirTmp, recursive = TRUE, force = TRUE)
dir.create(dirTmp)

file.copy(file.path(dirSrc,list.files(dirSrc)), dirTmp)
file.copy(file.path(dirData,list.files(dirData,pattern="xlsx")), dirTmp)

pop_wo_com <- data.table(readxl::read_excel(file.path(dirTmp,sprintf("%s.xlsx","pop_wo_com"))))
di_edge_list <- data.table(readxl::read_excel(file.path(dirTmp,sprintf("%s.xlsx","di_edge_list"))))

loc <- pop_wo_com[,c("kommuneNameOld","location")]
setnames(loc,"kommuneNameOld","from")
loc[,from:=factor(from,levels=from)]

nrow(di_edge_list)
di_edge_list <- merge(di_edge_list,loc,by="from")
nrow(di_edge_list)
di_edge_list[,from:=NULL]
setnames(di_edge_list,"location","from")

setnames(loc,"from","to")
nrow(di_edge_list)
di_edge_list <- merge(di_edge_list,loc,by="to")
nrow(di_edge_list)
di_edge_list[,to:=NULL]
setnames(di_edge_list,"location","to")
setcolorder(di_edge_list,c("from","to","n"))
setorder(di_edge_list,from,to)

pop_wo_com[,kommuneNameOld:=NULL]
setcolorder(pop_wo_com,c("location","pop"))

di_edge_list <- di_edge_list[from %in% unique(d$location) & to %in% unique(d$location)]
pop_wo_com <- pop_wo_com[location %in% unique(d$location)]
d <- d[location %in% pop_wo_com$location]

for(i in c("di_edge_list","pop_wo_com")){
  fwrite(get(i),
         file=file.path(dirTmp,sprintf("%s.txt",i)),
         sep=" ",
         col.names=F)
}

res <- withr::with_dir(dirTmp,{
  processx::run(
    command="g++",
    args=c("-std=c++11","-oinfl_kommuner.exe","infl_kommuner.cpp"),echo=T)
})


s = seasons[1]
doctorVisitingProb <- 0.3
startWeek <- 40
startX <- d[season==s & week==startWeek & age=="Totalt"]$x[1]

fwrite(data.frame(floor(d[season==s & week==startWeek & age=="Totalt"]$n*doctorVisitingProb)),
       file=file.path(dirTmp,"start_infected.txt"),
       sep=" ",
       col.names=F)

RunSim <- function(param=0.6){
  res <- withr::with_dir(dirTmp,{
    processx::run(
      command=file.path(dirTmp,"infl_kommuner.exe"),
      args=c(as.character(param),"3","1.9"))
  })
  
  list.files(dirTmp)
  
  loc <- fread(file.path(dirTmp,"pop_wo_com.txt"))
  setnames(loc,c("location","pop"))
  loc[,kn:=1:.N-1]
  
  m <- fread(file.path(dirTmp,"cpp_res_series.txt"))
  setnames(m,c("kn","S","E","SI","AI","R"))
  m <- m[seq(1,nrow(d),2)]
  m[,day:=1:.N,by=kn]
  
  m <- merge(m,loc,by="kn")
  m[,x:=floor(day/7)+startX]
  m <- m[,.(
    S=mean(S),
    E=mean(E),
    SI=mean(SI),
    AI=mean(AI),
    R=mean(R)),
    by=.(
      location,x
    )]
  
  res <- merge(d[season==s & age=="Totalt",],m,by=c("location","x"))
  
  return(res)
}

OptimFunction <- function(param=0.6){
  res <- RunSim(param=param)
  res[,error:=n-SI*doctorVisitingProb]
  
  totalError <- mean(res$error^2)

  return(totalError)
}

p <- optim(0.6, OptimFunction, method="L-BFGS-B", lower=0.4, upper=1,
  control=list(
    trace=3,
    maxit=30
))

sim <- RunSim(param=p$par)

q <- ggplot(sim[location=="municip0301"], aes(x=x))
q <- q + geom_point(mapping=aes(y=n))
q <- q + geom_line(mapping=aes(y=SI*doctorVisitingProb),col="red")
q <- q + geom_line(mapping=aes(y=SI),col="orange")
q


quit(save="no")
