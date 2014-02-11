# read in data 1967m1 t0 2011m3 531 10 vars
library(vars)
options(scipen=8, digits=8)  #This options command prints output in standard notation
setwd("C:/Users/sabaric/Documents/Revolution/CPI PROJECT")

lrprice <- ts(read.delim("ProcessedData.txt",header=F,skip=2, col.names=c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti"))[,2:9],start=c(1974,1),freq=12)
lrprice2<- lrprice[ ,c("lrwti","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrap")]
lrprice3<-lrprice[,c("lrwti","lrap")]
#alldat<-ts.intersect(cpidat,lrprice)
VARselect(lrprice3[,c("lrwti","lrap")])
 varrap <- VAR(lrprice3, p= 2, type="both")
system.time(irf.varrapt <- irf(varrap, impulse ="lrwti", response="lrap", boot=T, runs=1000, n.ahead=60, ci=.95, ortho=T ))

system.time(irf.varrapf <- irf(varrap, impulse ="lrwti", response="lrap", boot=TRUE,, runs=1000, n.ahead=22, ortho=F, ci=.95 ))
plot(irf.varrapt, plot.type="single", main="ortho=TRUE")
plot(irf.varrapf,plot.type="single", main="ortho=F")

