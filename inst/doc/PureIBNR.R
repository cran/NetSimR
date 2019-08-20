## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----pure IBNR example---------------------------------------------------
library("NetSimR")
#set the seed
set.seed(0)

#Set the reporting delay distribution parameters
mu <- 3
sigma <- 1

#Generate reporting delays and preview the distribution
x<-rlnorm(1000,mu,sigma)
summary(x)
hist(x, breaks = 100)

## ----pure IBNR example continued-----------------------------------------
#Generate dates data
PoliciesPerDay<-100
PeriodLength<-365*2
InceptionDateStart<-as.Date("2011/1/1")
InceptionDate<-seq(InceptionDateStart, by = "day", length.out = PeriodLength)
InceptionDate<-rep(InceptionDate,PoliciesPerDay)
#assume all policies are yearly
DayPolicyDuration<-365
ExpiryDate<-InceptionDate+DayPolicyDuration
ValuationDate<-max(InceptionDate)
Data<-data.frame(InceptionDate,ExpiryDate)
summary(Data)
ValuationDate

#Generate Claims Counts Data and reporting delays
#Assume only 1 claim per policy possible with probability of claiming 10%
ClaimFreq<-0.1
Data$ClaimCount<-ifelse(runif(nrow(Data))<ClaimFreq,1,0)
#generate accident date assuming uniformly happening throughout the year
Data$AccidentDate<-Data$InceptionDate+runif(nrow(Data))*(Data$ExpiryDate-Data$InceptionDate)
#generate reporting delays
Data$ReportingDelay<-round(Data$ClaimCount*rlnorm(nrow(Data),mu,sigma),1)
#generate reporting days
Data$NotificationDate<-Data$AccidentDate+Data$ReportingDelay
#remove claims reported beyond valuation day
Data$ClaimReportedBeforeValuation<-ifelse(Data$NotificationDate>ValuationDate,0,Data$ClaimCount)
#Maximum reporting delays
Data$MaxReportingDelay<-as.numeric(ValuationDate-Data$AccidentDate)+1

#Earned Duration
Data$EarnedDuration<-ifelse(Data$ExpiryDate>ValuationDate,as.numeric(ValuationDate-Data$InceptionDate),as.numeric(Data$ExpiryDate-Data$InceptionDate))/365

#Filter claims only data
ModelData<-Data[Data$ClaimReportedBeforeValuation==1,]

#model reporting delays with a linear model
lm<-lm(log(ModelData$ReportingDelay)~1)
#summary(lm)
cbind(coefficients(lm),summary(lm)$sigma)

#model reporting delays with a truncated model (splitting to monthly periods would be more accurate in terms of maximum reporting delay)
library("crch")
rTLm<-crch(log(ModelData$ReportingDelay)~1, truncated = TRUE, right = log(ModelData$MaxReportingDelay), dist = "gaussian", link.scale = "identity")
#summary(rTLm)
coefficients(rTLm)

#Generate and predict pure IBNR and UPR Claims
Data$PureIBNRClaims<-ifelse(Data$AccidentDate>ValuationDate,0,Data$ClaimCount-Data$ClaimReportedBeforeValuation)
Data$UPRClaims<-Data$ClaimCount-Data$ClaimReportedBeforeValuation-Data$PureIBNR
PureIBNRPrediction<-PureIBNRLNorm(Data$InceptionDate,Data$ExpiryDate,ValuationDate,coefficients(rTLm)[1],coefficients(rTLm)[2])
Data$PredictedPureIBNRYears<-PureIBNRPrediction$PureIBNRDuration/DayPolicyDuration
Data$PredictedUPRYears<-PureIBNRPrediction$UnearnedDuration/DayPolicyDuration
Data$NetEarnedDuration<-1-Data$PredictedUPRYears-Data$PredictedPureIBNRYears+0.00001

#Predict Claim Frequency with reduced exposure to adjust for pure IBNR
AdjFreqGLM<-glm(Data$ClaimReportedBeforeValuation~1, offset = log(Data$NetEarnedDuration), family = "poisson")
#summary(AdjFreqGLM)
PredictedFrequency<-exp(AdjFreqGLM$coefficients)

#Compare as if not adjusting for pure IBNR
UnAdjFreqGLM<-glm(Data$ClaimReportedBeforeValuation~1, offset = log(Data$EarnedDuration+0.00001), family = "poisson")
#summary(UnAdjFreqGLM)

cbind(AdjustedModel=round(exp(AdjFreqGLM$coefficients),3),UnAdjustedModel=round(exp(UnAdjFreqGLM$coefficients),3),Actual=ClaimFreq)


#Compare UPR Claim predictions
cbind(Actual=sum(Data$UPRClaims),Predicted=round(sum(Data$PredictedUPRYears)*PredictedFrequency,0))

#Compare Pure IBNR Claim predictions
cbind(Actual=sum(Data$PureIBNRClaims),Predicted=round(sum(Data$PredictedPureIBNRYears)*PredictedFrequency,0), Theoretical=327)

#Theoretical comes from re-running with the theoretical parameter values

#Compare Pure IBNR predictions with Chain Ladder
ClaimsAY1RY1<-sum(Data[Data$AccidentDate<as.Date("2012/1/1") & Data$NotificationDate<as.Date("2012/1/1"), ]$ClaimReportedBeforeValuation)

ClaimsAY1RY2<-sum(Data[Data$AccidentDate<as.Date("2012/1/1") & Data$NotificationDate<as.Date("2013/1/1"), ]$ClaimReportedBeforeValuation)

ClaimsAY2RY1<-sum(Data$ClaimReportedBeforeValuation)-ClaimsAY1RY1-ClaimsAY1RY2

PredictedPureIBNRClaimsChainLadder<-(ClaimsAY1RY2/ClaimsAY1RY1-1)*ClaimsAY2RY1

cbind(Actual=sum(Data$PureIBNRClaims),PredictedGLM=round(sum(Data$PredictedPureIBNRYears)*PredictedFrequency,0),PredictedCL=round(PredictedPureIBNRClaimsChainLadder,0))

#Note: Chain ladder can predict better if periods brake down to months or quarters

#Sliced LogNormal-Pareto claim Severity assumption
mu=5.6
sigma=1.65
s=10000
alpha=1.5

#Simulate the reserve distribution
numberOfSimulations=10000
SimulatedYears <- numeric(length = numberOfSimulations)
ExpectedFrequency <- sum(Data$PredictedPureIBNRYears)*PredictedFrequency

for (i in 1 :numberOfSimulations){
  SimulatedYears[i]=round(sum(qSlicedLNormPareto(runif(rpois(1,ExpectedFrequency)),mu,sigma,s,alpha)),0)
}

#Visualising pure IBNR reserve distribution
head(SimulatedYears)
hist(SimulatedYears, breaks = 1000, xlim = c(0,1.5e6))
summary(SimulatedYears)

#Note: the strength of the method illustrated is that it can have GLM factors for reporting delay, frequency and severity, and run the simulations per risk


