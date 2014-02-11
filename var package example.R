library(vars)
data(Canada)
library(boot)
#plot(Canada)
#write.table(Canada,"canada.txt",col.names=T,row.names=F,sep = ',')
## For VAR
var.2c <- VAR(Canada, p = 2, type = "both")

x<-irf(var.2c,  boot =TRUE, runs=200, n.ahead=40,ci=.90, ortho=F, seed=123456789)
plot(x, plot.type="single")
fevd.out <-fevd(varrap, n.ahead=60)$lrap
 boot.fevd<- function(data,indices) {
	 d<-data[indices,] 
	fevd.boot<-fevd(data[-1,], n.ahead=10)$U
	return(fevd.boot)
	}
results<-boot(data=var.2c, boot.fevd, R=20)
results

names(fevd.out)
msey<-vars:::.fecov(var.2c,n.ahead=10)
msey[,,1]
fevd.out[,,1]
sqrt(diag(msey[,,2]))