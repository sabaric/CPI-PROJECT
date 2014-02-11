
rm(list=ls(all=T))

library(vars)
library(tsDyn)
#options(scipen=8, digits=8)  #This options command prints output in standard notation

setwd("C:/Users/sabaric/Documents/Revolution/CPI PROJECT")  # set working directory 

cpidat <- read.csv("C:/Users/sabaric/Documents/Revolution/CPI PROJECT/SABADAT.csv", header = TRUE)
cpidat<-ts(cpidat,start=c(1974,1),freq=12)
lvar <- VAR(cpidat[ ,c("ROP","RAP","RCN")], p = 2, type = "both")

x <- lvar 

irf1 <- vars::irf(x, impulse = "ROP", response = c("RAP", "RCN"), boot=F, n.ahead=40, ortho=T)


nstep <- 4
K <- 4
p <- 2
nstep <- abs(as.integer(nstep))
A <- as.array(Acoef(lvar))

if ( nstep >= p ) {
  As <- array(0, dim = c(K, K, nstep + 1))
  for (i in (p + 1):(nstep + 1)) {
    As[, , i] <- matrix(0, nrow = K, ncol = K)
                                 }
  }   else  As <- array(0, dim = c(K, K, p))
     
  for (i in 1:p) {
  As[, , i] <- A[[i]]
}
As

Phi <- array(0, dim = c(K, K, nstep + 1))
Phi[, , 1] <- diag(K)
Phi[, , 2] <- Phi[, , 1] %*% As[, , 1]
Phi
if (nstep > 1) {
  for (i in 3:(nstep + 1)) {
    stmp1 <- Phi[, , 1] %*% A[, , i - 1]
    tmp2 <- matrix(0, nrow = K, ncol = K)
    idx <- (i - 2):1
    for (j in 1:(i - 2)) {
      tmp2 <- tmp2 + Phi[, , j + 1] %*% As[, , idx[j]]
    }
    Phi[, , i] <- tmp1 + tmp2
  }
}



if ( nstep> 0 ){
   x<=10
   } else {
     x <- 5
}

