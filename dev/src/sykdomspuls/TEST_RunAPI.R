if(.Platform$OS.type == "unix"){
  HOME <- "/src/sykdomspuls"
  PLUMB <- "plumb"
  DATA <- "/data_app/sykdomspuls"
} else {
  HOME <- "C:/Sykdomspulsen"
  PLUMB <- file.path("src","R","plumb")
  DATA <- "C:/Sykdomspulsen/data"
}

setwd(HOME)

suppressMessages(library(data.table))
suppressMessages(library(plumber))

r <- plumb(file.path("TEST_plumb.R"))
r$run(port=8000)

# curl http://localhost:8000/test?x=3

# curl http://localhost:10001/api/test?x=3
# curl http://localhost:10002/test?x=3

# curl http://localhost:10001/api/v
# curl http://localhost:10002/v




