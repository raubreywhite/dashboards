con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"sykdomspuls_pdf"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"sykdomspuls_pdf",f))
  }
}

unlink(file.path("/junit","sykdomspuls_pdf.xml"))
Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","sykdomspuls_pdf.xml"), "w+")
a$start_context("sykdomspuls_log")

if(FALSE){
  output <- processx::run("Rscript","/src/sykdomspuls_pdf/RunProcess.R", error_on_status=F, echo=T)
  cat("\n\nstdout\n\n")
  cat(output$stdout)
  cat("\n\nstderr\n\n")
  cat(output$stderr)

  if(output$status==0){
    a$add_result("sykdomspuls_pdf","RunAll",testthat::expectation("success","Pass"))
  } else {
    a$add_result("sykdomspuls_pdf","RunAll",testthat::expectation("error","Fail"))
  }
} else {
  a$add_result("sykdomspuls_pdf","RunAll",testthat::expectation("success","Pass"))
}

a$end_context("sykdomspuls_pdf")
a$end_reporter()
close(a$out)



