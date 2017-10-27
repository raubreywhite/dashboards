con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

# Cleaning up previous runs data
for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"sykdomspuls"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"sykdomspuls",f))
  }
}

unlink(file.path("/junit","sykdomspuls.xml"))
Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","sykdomspuls.xml"), "w+")
a$start_context("sykdomspuls")

# Run process

output <- processx::run("Rscript","/src/sykdomspuls/RunProcess.R", error_on_status=F)
cat("stdout")
cat(output$stdout)
cat("stderr")
cat(output$stderr)

if(output$status==0){
  a$add_result("sykdomspuls","RunAll",testthat::expectation("success","Pass"))
} else {
  a$add_result("sykdomspuls","RunAll",testthat::expectation("error","Fail"))
}

## Run API
process <- processx::process$new("Rscript","/src/sykdomspuls/RunAPI.R")
if(process$is_alive()){
  a$add_result("sykdomspuls","API_0min",testthat::expectation("success","Pass"))
} else {
  a$add_result("sykdomspuls","API_0min",testthat::expectation("error","Fail"))
}
Sys.sleep(120)
if(process$is_alive()){
  a$add_result("sykdomspuls","API_2min",testthat::expectation("success","Pass"))
} else {
  a$add_result("sykdomspuls","API_2min",testthat::expectation("error","Fail"))
}

req <- httr::GET("http://localhost:8000/test?x=0")
json <- httr::content(req, as = "text", encoding="UTF-8")
res <- jsonlite::fromJSON(json)

if(res=="0"){
  a$add_result("sykdomspuls","API_received_0",testthat::expectation("success","Pass"))
} else {
  a$add_result("sykdomspuls","API_received_0",testthat::expectation("error","Fail"))
}

x <- process$kill()

a$end_context("sykdomspuls")
a$end_reporter()
close(a$out)

