con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/sykdomspuls_pdf STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
#devtools::use_package("odfWeave")

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_sykdomspuls_pdf/", export_all=FALSE)
} else {
  library(sykdomspulspdf)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="sykdomspuls_pdf"
)



files <- IdentifyDatasets()
infiles <- IdentifyInOutDoc()

if(nrow(files)==0){
    cat(sprintf("%s/%s/R/SYKDOMSPULS_pdf No new data",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  return(FALSE)

} else {
  mydate <<- format(Sys.time(), "%d.%m.%y")
  d <- fread(fhi::DashboardFolder("data_raw",files$raw))
  fylke <-fread(system.file("extdata", "fylke.csv", package = "sykdomspulspdf"))
  lastestUpdate <- as.Date(gsub("_","-",LatestRawID()))

    cat(sprintf("%s/%s/R/SYKDOMSPULSSYKDOMSPULS_pdf Generating monthly pdf",Sys.time(),Sys.getenv("COMPUTER")),"\n")


#Alle konsultasjoner:
  data <- CleanData(d)
  alle <- tapply(data$gastro, data[, c("year","week")], sum)
  weeknow <-findLastWeek(lastestUpdate,alle) ### need to be fixed
  cat(paste("Last week",weeknow,sep = " "))

##BY FYLKE
for (SYNDROM in CONFIG$SYNDROMES) {
    ###########################################
    for (f in fylke$Fylkename) {

        Fylkename=f
        In <- infiles[V1==f]

        #All consultaion per fylke:
        data <- CleanDataByFylke(d, fylke,f)
        alle <- tapply(getdataout(data,SYNDROM), data[, c("year","week")], sum)
        yrange <- max(alle,na.rm=T)+(roundUpNice(max(alle,na.rm=T))*.20)

        #PLOT ALLE per fylke:
          CreatePlots1(alle,weeknow = weeknow, Ukenummer = Ukenummer,
                     title=paste(firstup(SYNDROM),"-tarminfeksjoner, ",f,", alle aldersgrupper", sep ="" ),
                     yrange=yrange)
          p <<-recordPlot()


        #PLOT BY AGE per fylke:
          CreatePlots2(d1=data,weeknow = weeknow, Ukenummer = Ukenummer,Fylkename=f,S=SYNDROM)

          k <<- recordPlot()

        graphics.off()

        ## Add to template
        odfWeave(fhi::DashboardFolder("data_raw",getIN(In,SYNDROM)),
                 fhi::DashboardFolder("results",paste(f,"_",SYNDROM,"tarm.odt", sep="")))

        dev.off()
    }

}

## whole norway
for (SYNDROM in CONFIG$SYNDROMES) {  
  Fylkename="Norge"
  #All consultaion per fylke:
  data <- CleanData(d)
  alle <- tapply(getdataout(data,SYNDROM), data[, c("year","week")], sum)
  yrange <- max(alle,na.rm=T)+(roundUpNice(max(alle,na.rm=T))*.20)
  
  #PLOT ALLE
  CreatePlots1(alle,weeknow = weeknow, Ukenummer = Ukenummer,
               title=paste(firstup(SYNDROM),"-tarminfeksjoner, Norge, alle aldersgrupper", sep ="" ),
               yrange=yrange)
  p <<-recordPlot()
  
  #PLOT BY AGE
  CreatePlots2(d1=data,weeknow = weeknow, Ukenummer = Ukenummer,Fylkename="Norge",S=SYNDROM)
  
  k <<- recordPlot()
  
  graphics.off()
  
  ## Add to template
  odfWeave(fhi::DashboardFolder("data_raw",paste("in_default_",SYNDROM,".odt", sep="")),
           fhi::DashboardFolder("results",paste("Norge_",SYNDROM,"tarm.odt", sep="")))
  
  dev.off()
  
   
}
  
  Sys.setenv(LD_LIBRARY_PATH="/usr/lib/libreoffice/program")
  withr::with_dir(fhi::DashboardFolder("results"), system("loffice --headless --convert-to pdf *.odt"))
  
  cat(sprintf("%s/%s/R/SYKDOMSPULS New monthly pdf are ready",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  return(TRUE)
}



DeleteOldDatasets()
#quit(save="no")











