con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/noispiah STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_noispiah/", export_all=FALSE)
} else {
  library(noispiah)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="noispiah"
)

quit(save="no")