con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat("##teamcity[testStarted name='Sykdomspuls.RunAll']\n")
cat("##teamcity[testStdOut name='Sykdomspuls.RunAll' out='We hope this runs correctly']\n")

# Cleaning up previous runs data
for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"sykdomspuls"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"sykdomspuls",f))
  }
}

cat("##teamcity[testStdOut name='Sykdomspuls.RunAll' out='Listing raw data files']\n")
cat("##teamcity[testStdOut name='Sykdomspuls.RunAll' out='",list.files(file.path("/data_raw","sykdomspuls")),"']\n")

# Run process
time1 <- Sys.time()
output <- processx::run("Rscript","/src/sykdomspuls/RunProcess.R", error_on_status=F)
time2 <- Sys.time()
duration <- difftime(time1,time2,units="secs")*1000

cat("##teamcity[testStdOut name='Sykdomspuls.RunAll' out='",output$stdout,"']\n")
cat("##teamcity[testStdOut name='Sykdomspuls.RunAll' out='",output$stderr,"']\n")

cat("stdout")
cat(output$stdout)
cat("stderr")
cat(output$stderr)

if(output$status==0){
  cat("##teamcity[testStdOut name='Sykdomspuls.RunAll' out='Looks ok']\n")
} else {
  cat("##teamcity[testFailed name='Sykdomspuls.RunAll' details='FAILED']\n")
}
cat("##teamcity[testFinished name='Sykdomspuls.RunAll' duration='",duration,"']\n")

## Run API
cat("##teamcity[testStarted name='Sykdomspuls.RunAPI']\n")
cat("##teamcity[testStdOut name='Sykdomspuls.RunAPI' out='We hope this runs correctly']\n")

time1 <- Sys.time()
process <- processx::process$new("Rscript","/src/sykdomspuls/RunAPI.R")
if(!process$is_alive()) cat("##teamcity[testFailed name='Sykdomspuls.RunAPI' details='Died immediately']\n")
Sys.sleep(120)
if(!process$is_alive()) cat("##teamcity[testFailed name='Sykdomspuls.RunAPI' details='Server is not alive after 2 min']\n")


req <- httr::GET("http://localhost:8000/test?x=0")
json <- httr::content(req, as = "text", encoding="UTF-8")
res <- jsonlite::fromJSON(json)

if(res=="0"){
  cat("##teamcity[testStdOut name='Sykdomspuls.RunAPI' out='Received 0 from API']\n")
} else {
  cat("##teamcity[testFailed name='Sykdomspuls.RunAPI' details='Did not receive 0 from API']\n")
}

x <- process$kill()

time2 <- Sys.time()
duration <- difftime(time1,time2,units="secs")*1000
cat("##teamcity[testFinished name='Sykdomspuls.RunAPI' duration='",duration,"']\n")




