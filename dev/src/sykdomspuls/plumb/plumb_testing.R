
#* @get /mean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  samples
}

#* @get /test
test <- function(x){
  x
  #unique(d$locationName)
}

# curl http://localhost:10002/v1_0_DataWeeklyOverviewKommune?name=municip0101


