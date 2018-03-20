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

output <- processx::run("Rscript","/src/sykdomspuls/RunProcess.R", error_on_status=F, echo=T)
cat("\n\nstdout\n\n")
cat(output$stdout)
cat("\n\nstderr\n\n")
cat(output$stderr)

if(output$status==0){
  cat("\n**PASS 1**\n")
  a$add_result("sykdomspuls","RunAll",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 1**\n")
  a$add_result("sykdomspuls","RunAll",testthat::expectation("error","Fail"))
}

## Run API
process <- processx::process$new("Rscript","/src/sykdomspuls/RunAPI.R")
if(process$is_alive()){
  cat("\n**PASS 2**\n")
  a$add_result("sykdomspuls","API_0min",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 2**\n")
  a$add_result("sykdomspuls","API_0min",testthat::expectation("error","Fail"))
}
Sys.sleep(60*10)
if(process$is_alive()){
  cat("\n**PASS 3**\n")
  a$add_result("sykdomspuls","API_10min",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 3**\n")
  a$add_result("sykdomspuls","API_10min",testthat::expectation("error","Fail"))
}

req <- httr::GET("http://localhost:8000/test?x=0")
json <- httr::content(req, as = "text", encoding="UTF-8")
res <- jsonlite::fromJSON(json)

print(res)

if(res=="0"){
  cat("\n**PASS 4**\n")
  a$add_result("sykdomspuls","API_received_0",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 4**\n")
  a$add_result("sykdomspuls","API_received_0",testthat::expectation("error","Fail"))
}

x <- process$kill()

# check external email works
library(sykdomspuls)
DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="sykdomspuls"
)


# check internal email works
res <- tryCatch(
  EmailInternal(isTest=TRUE),
  warning=function(war){
    print(war)
    return(-1)
  },
  error=function(err){
    print(err)
    return(-1)
  })
if(res==0){
  cat("\n**PASS 5**\n")
  a$add_result("sykdomspuls","EmailInternal",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 5**\n")
  a$add_result("sykdomspuls","EmailInternal",testthat::expectation("error","Fail"))
}


res <- tryCatch(
  EmailExternal(
    alerts = readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert_test.xlsx")),
    isTest=TRUE,
    forceNoOutbreak=TRUE
  ),
  warning=function(war){
    print(war)
    return(-1)
  },
  error=function(err){
    print(err)
    return(-1)
  })
if(res==0){
  cat("\n**PASS 6**\n")
  a$add_result("sykdomspuls","EmailExternal_forceNoOutbreak",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 6**\n")
  a$add_result("sykdomspuls","EmailExternal_forceNoOutbreak",testthat::expectation("error","Fail"))
}

res <- tryCatch(
  EmailExternal(
    alerts = readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert_test.xlsx")),
    isTest=TRUE,
    forceYesOutbreak=TRUE
  ),
  warning=function(war){
    print(war)
    return(-1)
  },
  error=function(err){
    print(err)
    return(-1)
  })
if(res==0){
  cat("\n**PASS 7**\n")
  a$add_result("sykdomspuls","EmailExternal_forceYesOutbreak",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 7**\n")
  a$add_result("sykdomspuls","EmailExternal_forceYesOutbreak",testthat::expectation("error","Fail"))
}

res <- tryCatch(
  EmailNotificationOfNewData(
    files="test_file_name.txt",
    isTest=TRUE
  ),
  warning=function(war){
    print(war)
    return(-1)
  },
  error=function(err){
    print(err)
    return(-1)
  })
if(res==0){
  cat("\n**PASS 8**\n")
  a$add_result("sykdomspuls","EmailNotificationOfNewData",testthat::expectation("success","Pass"))
} else {
  cat("\n**FAIL 8**\n")
  a$add_result("sykdomspuls","EmailNotificationOfNewData",testthat::expectation("error","Fail"))
}


a$end_context("sykdomspuls")
a$end_reporter()
close(a$out)

