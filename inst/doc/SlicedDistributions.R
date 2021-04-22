## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----sliced mean--------------------------------------------------------------
library("NetSimR")
round(SlicedLNormParetoMean(6,1.6,10000,1.2),0)

## ----sliced distribution simulation-------------------------------------------
#set seed
set.seed(1)

#set parameters
numberOfSimulations=20000
freq=15
mu=6
sigma=1.6
s=1000
alpha=1.2
Deductible=10000
limit=100000
XoLCededClaims <- numeric(length = numberOfSimulations)

#loop simulates frequency then severity and applies layer deductible and limit
for (i in 1 :numberOfSimulations){
  x=qSlicedLNormPareto(runif(rpois(1,freq)),mu,sigma,s,alpha)
  x=ifelse(x<Deductible,0,ifelse(x-Deductible>limit,limit,x-Deductible))
  XoLCededClaims[i]=round(sum(x),0)
}

#Visualising Simulation results
head(XoLCededClaims)
hist(XoLCededClaims, breaks = 100, xlim = c(0,100000))
summary(XoLCededClaims)
  

