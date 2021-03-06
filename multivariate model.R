library(vars)
library(RODBC)
channel <- odbcConnectExcel2007("C:/Users/sabaric/Documents/Revolution/CPI PROJECT/ProcessedData.xlsx")
mydata <- sqlFetch(channel, "Sheet1",colnames=TRUE)
odbcClose(channel) 
colnames(mydata)<-c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti","liprod")
ldat<-(mydata[,c(9,2:8,10)])
head(ldat)
ldat<-ts(ldat,start=c(1974,1),freq=12)
VARselect(ldat[,c("lrwti","lrap")])
 varrap <- VAR(ldat[,c("lrwti","lrap")], p= 2, type="const")
summary(varrap)
roots(varrap)
system.time(x <- irf(varrap, impulse ="lrwti", response=c("lrap"), boot=T, runs=100, n.ahead=48, ci=.68, ortho=F ))

plot(x, plot.type="single", main="ortho=FALSE")

