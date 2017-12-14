## Run API
process <- processx::process$new("Rscript","/src/sykdomspuls/RunAPI.R")

Sys.sleep(240)

req <- httr::GET("http://localhost:8000/test?x=0")
json <- httr::content(req, as = "text", encoding="UTF-8")
res <- jsonlite::fromJSON(json)


print(res)

x <- process$kill()
