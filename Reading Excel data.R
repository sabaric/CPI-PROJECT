#first row contains variable names
# we will read in workSheet 
library(RODBC)
channel <- odbcConnectExcel2007("C:/Users/sabaric/Documents/Revolution/CPI PROJECT/ProcessedData.xlsx")
mydata <- sqlFetch(channel, "Sheet1",colnames=TRUE)
odbcClose(channel) 
colnames(mydata)<-c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti","liprod")
ldat<-mydata[,c(9,2:8,10)]
str(ldat)
head(ldat)

ts(ldat,start=c(1974,1),freq=12)


