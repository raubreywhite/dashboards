con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat("##teamcity[testStarted name='Normomo.RunAll']\n")
cat("##teamcity[testStdOut name='Normomo.RunAll' out='We hope this runs correctly']\n")

for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"normomo"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"normomo",f))
  }
}

time1 <- Sys.time()
output <- processx::run("Rscript","/src/normomo/RunProcess.R", error_on_status=F)
time2 <- Sys.time()
duration <- difftime(time1,time2,units="secs")*1000

if(output$status==0){
  cat("##teamcity[testStdOut name='Normomo.RunAll' out='Looks ok']\n")
} else {
  cat("##teamcity[testFailed name='Normomo.RunAll' details='FAILED']\n")
}
cat("##teamcity[testFinished name='Normomo.RunAll' duration='",duration,"']\n")



