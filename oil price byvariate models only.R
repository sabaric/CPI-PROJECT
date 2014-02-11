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
colnames(cpidat)<-c("date","lrap","lrenergy","lffoodbev","lrfood","lrhousing","lrmedical","lrtrans","lrwti","lrprod" )


# Apparel
varrap <- VAR(cpidat[,c("lrwti","lrap" )], p= 2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse ="lrwti", response=c("lrap" ), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789)) 
irfOutApparel<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutApparel)<-c("ApparelFevd", "LowerCiApparel",  "UpperCiApparel")
write.table(irfOutApparel,"Apparelout2.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="single")
fevd.out <-fevd(varrap, n.ahead=60)$lrap
write.table(fevd.out,"fevd_apparal.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)
# Energy
varrap <- VAR(cpidat[,c("lrwti","lrenergy" )], p= 2, type="both") 
roots(varrap)
irf.varap <- irf(varrap, impulse ="lrwti", response=c("lrenergy" ), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) 
irfOutEnergy<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutEnergy)<-c("EnergyFevd", "LowerCiEnergy", "UpperCiEnergy")
write.table(irfOutEnergy,"Energyout2.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="single")
fevd.out <-fevd(varrap, n.ahead=60)$lrenergy
write.table(fevd.out,"fevd_energy.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Food
varrap <- VAR(cpidat[,c("lrwti","lrfood" )], p= 2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrfood" ), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) ) 
irfOutFood<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutFood)<-c("foodFevd", "LowerCiFood", "UpperCiFood")
write.table(irfOutFood,"Foodout2.csv",col.names=TRUE,row.names=F,sep=",")

plot(irf.varap, plot.type="single")
fevd.out <-fevd(varrap, n.ahead=60)$lrfood
write.table(fevd.out,"fevd_food.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)
# Housing
varrap <- VAR(cpidat[,c("lrwti","lrhousing" )], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrhousing" ), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) )
irfOutHousing<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutHousing)<-c("HousingFevd", "LowerCiHousing", "UpperCiHousing")
write.table(irfOutHousing,"Housingout2.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="single")
fevd.out <-fevd(varrap, n.ahead=60)$lrhousing
write.table(fevd.out,"fevd_housing.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Medical
varrap <- VAR(cpidat[,c("lrwti","lrmedical" )], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrmedical" ), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) )
irfOutFood<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutFood)<-c("MedicalFevd", "LowerCiMedical", "UpperCiMedical")
write.table(irfOutFood,"Medicalout.csv",col.names=TRUE,row.names=F,sep=",") 



plot(irf.varap, plot.type="single")

fevd.out <-fevd(varrap, n.ahead=60)$lrmedical
write.table(fevd.out,"fevd_medical.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Transportation
varrap <- VAR(cpidat[,c("lrwti","lrtrans" )], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("lrwti"), response=c("lrtrans" ), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, seed=123456789) )
irfOutTransportation<- cbind(irf.varap$irf$lrwti,irf.varap$Lower$lrwti,irf.varap$Upper$lrwti) #Save fevd and confidence intervales
 colnames(irfOutTransportation)<-c("TransportationFevd", "LowerCiTransportation", "UpperCiTransportation")
write.table(irfOutTransportation,"Transportationout.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="single")
fevd.out <-fevd(varrap, n.ahead=60)$lrtrans
write.table(fevd.out,"fevd_transportation.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)


