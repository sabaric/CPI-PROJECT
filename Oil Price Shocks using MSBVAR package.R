rm(list=ls(all=T))
library(MSBVAR)

options(scipen=8, digits=8)  #This options command prints output in standard notation
setwd("C:/Users/Public/Documents/Revolution/CPI PROJECT/CPI PROJECT")  # set working directory 

lrprice <- ts(read.delim("ProcessedData.txt",header=F,skip=2, col.names=c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti"))[,2:9],start=c(1974,1),freq=12)
trend=1:nrow(lrprice)
run1<-lrprice[,c("lrwti","lrap")]
rfv<-reduced.form.var(run1,p=2)
summary(rfv)
rfv.mc<-mc.irf(rfv,nstep=40,draws=2000)
op <- par(no.readonly = TRUE)
plot(rfv.mc,probs=c(.025,.975),method=c("Sims-Zha3"))
#pldat<-plot(rfv.mc,probs=c(.025,.975),method=c("Sims-Zha3"))

par(op)
ts.plot(as.ts(pldat$responses[,,3][,3]),lty=1,col="red", ylim=c(min(pldat$responses[,,3][,1]),max(pldat$responses[,,3][,2]) ) )
abline(h=0)
lines(as.ts(pldat$responses[,,3][,2]),lty=1,col="Black")
lines(as.ts(pldat$responses[,,3][,1]),lty=1,col="Black")

