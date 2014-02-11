rm(list=ls(all=T))

library(vars)
library(tsDyn)
options(scipen=8, digits=8)  #This options command prints output in standard notation

setwd("C:/Users/sabaric/Documents/Revolution/CPI PROJECT")  # set working directory 

cpidat <- read.csv("C:/Users/sabaric/Documents/Revolution/CPI PROJECT/SABADAT.csv", header = TRUE)
cpidat<-ts(cpidat,start=c(1974,1),freq=12)


colnames(cpidat)

lvar <- VAR(cpidat[ ,c("ROP","RAP","RCN")], p = 2, type = "both")
summary(lvar)
names(lvar$varresult[1])
showMethods("summary")
lvar$varresult[[3]]
str(lvar)
sig<-crossprod(resid(lvar))
P <- chol(sig)
P%*%t(P)
sig
Pinv <- solve(P)
Pinv%*%sig%*%t(Pinv)