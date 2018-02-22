con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

for(baseFolder in c("/results","/data_app")){
  files <- list.files(file.path(baseFolder,"sykdomspuls_log"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"sykdomspuls_log",f))
  }
}

unlink(file.path("/junit","sykdomspuls_log.xml"))
Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","sykdomspuls_log.xml"), "w+")
a$start_context("sykdomspuls_log")

output <- processx::run("Rscript","/src/sykdomspuls_log/RunProcess.R", error_on_status=F, echo=T)
cat("\n\nstdout\n\n")
cat(output$stdout)
cat("\n\nstderr\n\n")
cat(output$stderr)

if(output$status==0){
  a$add_result("sykdomspuls_log","RunAll",testthat::expectation("success","Pass"))
} else {
  a$add_result("sykdomspuls_log","RunAll",testthat::expectation("error","Fail"))
}

a$end_context("sykdomspuls_log")
a$end_reporter()
close(a$out)



