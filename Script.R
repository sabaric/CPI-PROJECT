# read in data 1967m1 t0 2011m3 531 10 vars
library(vars)
options(scipen=8, digits=8)  #This options command prints output in standard notation
setwd("C:/Users/sabaric/Documents/Revolution/CPI PROJECT")
getwd()
lrprice <- ts(read.delim("ProcessedData.txt",header=F,skip=2, col.names=c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti"))[,2:9],start=c(1974,1),freq=12)

# spot oil price: texas intermediate spot and spot in 2005 dollas.
#spot <- ts(read.csv("Spot_Oil_Price__West_Texas_Intermediate.csv")[,2:3],start=c(1946,1),freq=12)
#plot(spot[,1])

 varrap <- VAR(lrprice[,c(1,8)], p= 2, type="both")
summary(varrap)
plot(varrap)
roots(varrap.ser)
varrap.ser <- restrict(varrap, method = "ser", thresh = 2)
system.time(irf.varrap <- irf(varrap, impulse ="lrwti", response="lrap", boot=TRUE,, runs=200, n.ahead=24,ci=.68, ortho=F ))
plot(irf.varrap,plot.type ="single", ylim=c(-.1, .1),  xlab="Months")