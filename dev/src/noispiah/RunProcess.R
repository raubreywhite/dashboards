con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/noispiah STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(foreach))
suppressMessages(library(doSNOW))
suppressMessages(library(iterators))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_noispiah/", export_all=FALSE)
  SetConfig("FORCE_TESTING",TRUE)
} else {
  library(noispiah)
}

#DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="noispiah"
)

StackIterator <- function(stack, progressFunction) {
  library(data.table)
  it <- icount(nrow(stack))

  nextEl <- function() {
    i <- nextElem(it)
    progressFunction(i)
    stack[i]
    #list("stack"=stack[i],"data"=data[variable==stack$type[i] & location==stack$location[i] & age==stack$age[i]])
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}

CheckData()

stack <- GenStack(dev=CONFIG$FORCE_TESTING,
                FILES_RMD_USE_SYKEHJEM=CONFIG$FILES_RMD_USE_SYKEHJEM,
                FILES_RMD_USE_SYKEHUS=CONFIG$FILES_RMD_USE_SYKEHUS)

pb <- RAWmisc::ProgressBarCreate(max=nrow(stack))
assign("pb", pb, envir = .GlobalEnv)

progressIndex <- 0
assign("progressIndex", progressIndex, envir = .GlobalEnv)

ProgressFunction <- function(n) RAWmisc::ProgressBarSet(pb, progressIndex + n)
assign("ProgressFunction", ProgressFunction, envir = .GlobalEnv)

opts <- list(progress=ProgressFunction)
assign("opts", opts, envir = .GlobalEnv)

cl <- makeCluster(parallel::detectCores())
registerDoSNOW(cl)

res <- foreach(analysisIter=StackIterator(stack, ProgressFunction)) %dopar% {
  Sys.sleep(1)
  noispiah::RenderExternally(input=analysisIter$RMD,
                   output_file=sprintf("%s.pdf",analysisIter$location),
                   output_dir=analysisIter$outputDirUse,
                   params=sprintf("dev=\"%s\",level=\"%s\",location=\"%s\"",
                                  analysisIter$dev,
                                  analysisIter$level,
                                  analysisIter$location))
}

file.create(DashboardFolder("data_raw","DONE.txt"))


quit(save="no")

#"Møre og Romsdal - Fræna" -
#"Møre og Romsdal - Haram"
