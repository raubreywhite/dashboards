con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/noispiah STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_noispiah/", export_all=FALSE)
  SetConfig("FORCE_TESTING",TRUE)
} else {
  library(noispiah)
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="noispiah"
)

CheckData()

(FILES_RMD_USE_SYKEHJEM <- CONFIG$FILES_RMD_USE_SYKEHJEM)
(FILES_RMD_USE_SYKEHUS <- CONFIG$FILES_RMD_USE_SYKEHUS)
(dev<-CONFIG$FORCE_TESTING)

Render(dev=dev,
       FILES_RMD_USE_SYKEHJEM=FILES_RMD_USE_SYKEHJEM,
       FILES_RMD_USE_SYKEHUS=FILES_RMD_USE_SYKEHUS)

file.create(DashboardFolder("data_raw","DONE.txt"))


quit(save="no")

#"Møre og Romsdal - Fræna"
#"Møre og Romsdal - Haram"
