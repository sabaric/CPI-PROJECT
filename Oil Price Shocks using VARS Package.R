# read in data 1967m1 t0 2011m3 531 10 vars
rm(list=ls(all=T))

library(vars)
options(scipen=8, digits=8)  #This options command prints output in standard notation
options(graphics.record=TRUE)
setwd("C:/Users/Public/Documents/Revolution/CPI PROJECT")  # set working directory 
library(RODBC)
channel <- odbcConnectExcel2007("C:/Users/Public/Documents/Revolution/CPI PROJECT/ProcessedData.xlsx")
cpidat <- sqlFetch(channel, "Sheet1",colnames=TRUE)
odbcClose(channel) 

cpidat<-ts(cpidat,start=c(1974,1),freq=12)
colnames(cpidat)<-c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti","liprod")

# Apparel
varrap <- VAR(cpidat[,c("lrwti","lrap","liprod")], p=2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse ="lrwti", response=c("lrap","liprod"), boot=T, runs=50, n.ahead=40, ci=.95, ortho=F, seed=123456789)) 
irfOutApparel<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutApparel)<-c("ApparelFevd","ProdFevd", "LowerCiApparel", "LowerCiProd", "UpperCiApparel", "UpperCiProd")
write.table(irfOutApparel,"Apparelout3.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$lrap
write.table(fevd.out,"fevd3_apparal.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Energy
varrap <- VAR(cpidat[,c("lrwti","lrenergy","liprod")], p= 2, type="both") 
roots(varrap)
irf.varap <- irf(varrap, impulse ="lrwti", response=c("lrenergy","liprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) 
irfOutEnergy<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutEnergy)<-c("EnergyFevd","ProdFevd", "LowerCiEnergy", "LowerCiProd", "UpperCiEnergy", "UpperCiProd")
write.table(irfOutEnergy,"Energyout3.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$lrenergy
write.table(fevd.out,"fevd3_energy.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Food
varrap <- VAR(cpidat[,c("lrwti","lrfood","liprod")], p= 2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrfood","liprod"), boot=T, runs=50, n.ahead=40, ci=.95, ortho=F, seed=123456789) ) 
irfOutFood<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutFood)<-c("FoodFevd","ProdFevd", "LowerCiFood", "LowerCiProd", "UpperCiFood", "UpperCiProd")
write.table(irfOutFood,"Foodout3.csv",col.names=TRUE,row.names=F,sep=",")
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$lrfood
write.table(fevd.out,"fevd3_food.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Housing
varrap <- VAR(cpidat[,c("lrwti","lrhousing","liprod")], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrhousing","liprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) )
irfOutHousing<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutHousing)<-c("HousingFevd","ProdFevd", "LowerCiHousing", "LowerCiProd", "UpperCiHousing", "UpperCiProd")
write.table(irfOutHousing,"Housingout3.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$lrhousing
write.table(fevd.out,"fevd3_housing.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Medical
varrap <- VAR(cpidat[,c("lrwti","lrmedical","liprod")], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrmedical","liprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) )
irfOutMedical<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutMedical)<-c("MedicalFevd","ProdFevd", "LowerCiMedical", "LowerCiProd", "UpperCiMedical", "UpperCiProd")
write.table(irfOutFood,"Medicalout3.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$lrmedical
write.table(fevd.out,"fevd3_medical.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Transportation
varrap <- VAR(cpidat[,c("lrwti","lrtrans","liprod")], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrtrans","liprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) )
irfOutTransportation<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
colnames(irfOutTransportation)<-c("TransportationFevd","ProdFevd", "LowerCiTransportation", "LowerCiProd", "UpperCiTransportation", "UpperCiProd")
write.table(irfOutTransportation,"Transportationout3.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$lrtrans
write.table(fevd.out,"fevd3_transportation.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)


