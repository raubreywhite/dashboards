library(ggplot2)
library(data.table)
data <- readRDS("/data_app/sykdomspuls/resYearLine.RDS")

data <- data[location=="Norge" & age=="15-19" & type=="influensa"]
data[,years:=as.numeric(displayDay-min(displayDay))/365]
q <- ggplot(data,aes(x=displayDay,y=consult))
q <- q + geom_line()
q

summary(lm(consult~years,data=data[year %in% c(2008:2012)]))

summary(lm(consult~years,data=data))
temp <- data[,.(consult=mean(consult),influensa=mean(n)),by=.(year)]
temp[,influensaPerc:=influensa/consult*100]
temp
summary(lm(influensaPerc~as.numeric(year),data=temp))
