library(foreign)  # load to read STATA dta sets
library(MSBVAR)
   # panal data package
lutk<-ts(read.dta("http://www.stata-press.com/data/r11/lutkepohl2.dta"),start=c(1960,1),freq=4)
summary(lutk)
lutk1<-window(lutk,start=c(1960,2),end=c(1978,4))
rfv<-reduced.form.var(lutk1[,c("dln_inv", "dln_inc", "dln_consump")],p=2)
summary(rfv)
rfv
rfv.mc<-mc.irf(rfv,nstep=10, draws=2000)
op <- par(no.readonly = TRUE) # the whole list of settable par's.
#plot(rfv.mc,probs=c(.025,.975),method=c("Sims-Zha3"))  # MSBVAR program plot function
pldat<-plot(rfv.mc,probs=c(.025,.975),method=c("Sims-Zha3"))

par(op)
ts.plot(as.ts(pldat$responses[,,1][,3]),lty=1,col="red", ylim=c(min(pldat$responses[,,1][,1]),max(pldat$responses[,,1][,2]) ) )
abline(h=0)
lines(as.ts(pldat$responses[,,1][,2]),lty=1,col="Black")
lines(as.ts(pldat$responses[,,1][,1]),lty=1,col="Black")
