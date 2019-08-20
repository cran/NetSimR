## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----capped mean example-------------------------------------------------
#required packages
library("NetSimR")
library("crch")

#Set parameters
n<-10000
mu<-6
sigma<-1.7
Cap <- 10000

#Set seed to keep simulations constant
set.seed(10)

#Simulate data
x<-round(rlnorm(n,mu,sigma),0)
head(x)
summary(x)
plot(x)
hist(x, breaks = 400, xlim = c(0,10000))


#cap data
z<-ifelse(x>Cap,Cap,x)
head(z)
summary(z)
plot(z)
hist(z, breaks = 200)


#fit linear regression
lmLinear<-lm(log(z)~1)
summary(lmLinear)


#fit right censored regression
lmCensored<-crch(log(z)~1, right = log(Cap), link.scale="identity", dist = "gaussian")
summary(lmCensored)


#Compare regression outputs
cbind(coefficients(lmLinear),summary(lmLinear)$sigma)
coefficients(lmCensored)


#Compare regressions' attritional capped cost to empirical
round(cbind(
  Empirical=mean(z)
  ,LinearModel=exp(coefficients(lmLinear)+0.5*summary(lmLinear)$sigma*summary(lmLinear)$sigma)
  ,CensoredModel=LNormCappedMean(Cap,coefficients(lmCensored)[1],coefficients(lmCensored)[2])
),0)

## ----Exposure curve LogNormal--------------------------------------------
ExposureCurveLNorm(1000,5,1.6)

## ----ILF curve LogNormal-------------------------------------------------
ILFLNorm(1000,1500,5,1.6)

