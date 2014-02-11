# read in data 1967m1 t0 2011m3 531 10 vars
rm(list=ls(all=T))
library(XLConnect)
library(vars)
options(scipen=8, digits=8)  #This options command prints output in standard notation
options(graphics.record=TRUE)
#setwd("C:/Users/sabaric/Documents/Revolution/CPI PROJECT")  # set working directory 
# library(RODBC)
# channel <- odbcConnectExcel2007("C:/Users/sabaric/Documents/Revolution/CPI PROJECT/rawdata.xlsx")
# rawcpidat <- sqlFetch(channel, "Sheet1",colnames=TRUE)
# odbcClose(channel) 
wb <- loadWorkbook("rawdata.xlsx")
rawcpidat <-readWorksheet(wb,sheet ="sheet1")

rawcpidat<-ts(rawcpidat[,2:11],start=c(1974,1),freq=12)
rawcpidat1 <- ts.intersect(rawcpidat,rawcpidat[,10]/rawcpidat[,1])
colnames(rawcpidat1)<-c("CPI","logdap","logdenergy","logdfoodbev","logdfood","logdhousing","logdmedical","logdtrans","logdwti","logdprod","logdrealprod")

lcpidat <- log(rawcpidat1)  # take logs of all variable
ldcpidat<-na.omit(diff(lcpidat))	#    Take first differences of logs	
options(graphics.record=TRUE)
acf(ldcpidat[,c(4)],type="correlation");acf(ldcpidat[,c(4)],type="partial")	
# Apparel
varrap <- VAR(ldcpidat[,c("logdwti","logdap","logdprod")], p=2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse ="logdwti", response=c("logdap","logdprod"), boot=T, runs=5000 , n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789)) 
irfOutApparel<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
 colnames(irfOutApparel)<-c("ApparelFevd","ProdFevd", "LowerCiApparel", "LowerCiProd", "UpperCiApparel", "UpperCiProd")
write.table(irfOutApparel,"Apparelout3_logd.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdap
write.table(fevd.out,"fevd3_apparal_logd.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)



# Energy
varrap <- VAR(ldcpidat[,c("logdwti","logdenergy","logdprod")], p= 2, type="both") 

roots(varrap)
irf.varap <- irf(varrap, impulse ="logdwti", response=c("logdenergy","logdprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789) 
irfOutEnergy<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
 colnames(irfOutEnergy)<-c("EnergyFevd","ProdFevd", "LowerCiEnergy", "LowerCiProd", "UpperCiEnergy", "UpperCiProd")
write.table(irfOutEnergy,"Energyout3_logd.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdenergy
write.table(fevd.out,"fevd3_energy_logd.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Food
varrap <- VAR(ldcpidat[,c("logdwti","logdfood","logdprod")], p= 2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse =c("logdwti"), response=c("logdfood","logdprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789) ) 
irfOutFood<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
 colnames(irfOutFood)<-c("FoodFevd","ProdFevd", "LowerCiFood", "LowerCiProd", "UpperCiFood", "UpperCiProd")
write.table(irfOutFood,"Foodout3_logd.csv",col.names=TRUE,row.names=F,sep=",")
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdfood
write.table(fevd.out,"fevd3_food_logd.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Housing
varrap <- VAR(ldcpidat[,c("logdwti","logdhousing","logdprod")], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("logdwti"), response=c("logdhousing","logdprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789) )
irfOutHousing<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
 colnames(irfOutHousing)<-c("HousingFevd","ProdFevd", "LowerCiHousing", "LowerCiProd", "UpperCiHousing", "UpperCiProd")
write.table(irfOutHousing,"Housingout3_logd.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdhousing
write.table(fevd.out,"fevd3_housing_logd.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Medical
varrap <- VAR(ldcpidat[,c("logdwti","logdmedical","logdprod")], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("logdwti"), response=c("logdmedical","logdprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789) )
irfOutMedical<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
 colnames(irfOutMedical)<-c("MedicalFevd","ProdFevd", "LowerCiMedical", "LowerCiProd", "UpperCiMedical", "UpperCiProd")
write.table(irfOutFood,"Medicalout3_logd.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdmedical
write.table(fevd.out,"fevd3_medical_logd.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Transportation
varrap <- VAR(ldcpidat[,c("logdwti","logdtrans","logdprod")], p= 2, type="both") 
roots(varrap)
system.time( irf.varap <- irf(varrap, impulse =c("logdwti"), response=c("logdtrans","logdprod"), boot=T, runs=5000, n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789) )
irfOutTransportation<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
colnames(irfOutTransportation)<-c("TransportationFevd","ProdFevd", "LowerCiTransportation", "LowerCiProd", "UpperCiTransportation", "UpperCiProd")
write.table(irfOutTransportation,"Transportationout3_logd.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdtrans
write.table(fevd.out,"fevd3_transportation_logd.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)

# Apparel
varrap <- VAR(ldcpidat[,c("logdwti","logdap","logdrealprod")], p=2, type="both") 
roots(varrap)
system.time(irf.varap <- irf(varrap, impulse ="logdwti", response=c("logdap","logdrealprod"), boot=T, runs=200, n.ahead=40, ci=.95, ortho=F, cumulative=TRUE, seed=123456789)) 
irfOutApparel<- cbind(irf.varap$irf$logdwti,irf.varap$Lower$logdwti,irf.varap$Upper$logdwti) #Save fevd and confidence intervales
 colnames(irfOutApparel)<-c("ApparelFevd","ProdFevd", "LowerCiApparel", "LowerCiProd", "UpperCiApparel", "UpperCiProd")
write.table(irfOutApparel,"Apparelout3_logdr.csv",col.names=TRUE,row.names=F,sep=",") 
plot(irf.varap, plot.type="multiple")
fevd.out <-fevd(varrap, n.ahead=60)$logdap
write.table(fevd.out,"fevd3_apparal_logdr.csv",col.names=TRUE,row.names=F,sep=",")
fevd.out[c(1,3,6,12,24,36,48,60),] # only print rows specified rows. 
pred.out <- predict(varrap,n.ahead=10,ci=0.95)
