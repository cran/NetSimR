
#Set Parameters
mu = 4
sigma = 1.5

#test pure IBNR duration
inceptionDate<-as.POSIXct("01/01/2007", format="%d/%m/%Y")

expiryDate<-as.POSIXct("31/12/2007", format="%d/%m/%Y")

ValuationDate<-as.POSIXct("30/10/2007", format="%d/%m/%Y")

PureIBNRLNorm(inceptionDate,expiryDate,ValuationDate,mu,sigma)

MaxRepDelay <- 0:as.numeric(ValuationDate-inceptionDate)

pureIBNRDays<-sum(plnorm(MaxRepDelay,mu,sigma,lower.tail = F))
pureIBNRDays

duration=as.numeric(expiryDate-inceptionDate)

UPRDays<-as.numeric(expiryDate-ValuationDate)
UPRDays

UPRDays/duration

pureIBNRDays/duration

