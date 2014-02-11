rm(list=ls(all=T))
library(MSBVAR)
const=rep(1,nrow(Canada))
trend=1:nrow(Canada)
det<-ts(matrix(cbind(const,trend),nrow=84,ncol=2), start=c(1980,1),freq=4);dim(det)
rfv<-reduced.form.var(Canada[,c("U","prod")],p=1,z=as.matrix(trend))
summary(rfv)
rfv.mc<-mc.irf(rfv,nstep=32, draws=200)
pldat<-plot(rfv.mc,probs=c(.025,.975),method=c("Sims-Zha3"))

ts.plot(as.ts(pldat$responses[,,3][,3]),lty=1,col="red", ylim=c(min(pldat$responses[,,3][,1]),max(pldat$responses[,,3][,2]) ) )
abline(h=0)
lines(as.ts(pldat$responses[,,3][,2]),lty=1,col="Black")
lines(as.ts(pldat$responses[,,3][,1]),lty=1,col="Black")




