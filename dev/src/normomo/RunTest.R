con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"normomo"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"normomo",f))
  }
}

unlink(file.path("/junit","normomo.xml"))
a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","normomo.xml"), "w+")
a$start_context("normomo")
time1 <- Sys.time()
output <- processx::run("Rscript","/src/normomo/RunProcess.R", error_on_status=F)
time2 <- Sys.time()
duration <- difftime(time1,time2,units="secs")*1000

if(output$status==0){
  a$add_result("normomo","RunNew",testthat::expectation("success","Pass"))
} else {
  a$add_result("normomo","RunAll",testthat::expectation("error","Fail"))
}
a$add_result("normomo","Test",testthat::expectation("error","Test fail"))
a$end_context("normomo")
a$end_reporter()
close(a$out)

