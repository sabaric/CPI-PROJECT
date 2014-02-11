rm(list=ls(all=T))
library(vars)
library(foreign)  # load to read STATA dta sets
  
lutk<-ts(read.dta("http://www.stata-press.com/data/r11/lutkepohl2.dta"),start=c(1960,1),freq=4)
summary(lutk)
lutk1<-window(lutk,start=c(1960,2),end=c(1978,4))
var.2c <- VAR(lutk1[,c("dln_inv","dln_inc","dln_consump")], p = 2, type = "const")
summary(var.2c)
x<-irf(var.2c, implus="dln_inc",response="dln_consump", boot =TRUE, runs=200, n.ahead=10,ci=.95, ortho=T, seed=123456789)
plot(x, plot.type="single")
fevd.out<-fevd(var.2c,n.ahead=8)
msey<-vars:::.fecov(var.2c,n.ahead=8)
msey[,,1]
fevd.out
for (i in 1:8) {
	
	print(sqrt(diag(msey[,,i])))
}

