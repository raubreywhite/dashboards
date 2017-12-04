library(data.table)
library(ggplot2)

d <- readRDS("/data_app/sykdomspuls/resYearLineMunicip.RDS")
d <- d[location=="municip1711" & age=="Totalt" & type=="gastro"]
d[,day:=.I]

x <- d[,.(n=mean(n),consult=mean(consult)),by=year]
x[,ratio:=n/consult]
x

q <- ggplot(d,aes(x=day))
q <- q + geom_line(mapping=aes(y=threshold2))
q
