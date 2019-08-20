
#Set Parameters
shape = 7
rate = 0.15

#test pure IBNR duration
inceptionDate<-as.POSIXct("01/01/2007", format="%d/%m/%Y")

expiryDate<-as.POSIXct("31/12/2007", format="%d/%m/%Y")

ValuationDate<-as.POSIXct("30/10/2007", format="%d/%m/%Y")

PureIBNRGamma(inceptionDate,expiryDate,ValuationDate,shape,rate)

MaxRepDelay <- 0:as.numeric(ValuationDate-inceptionDate)

pureIBNRDays<-sum(pgamma(MaxRepDelay,shape,rate,lower.tail = F))
pureIBNRDays

duration=as.numeric(expiryDate-inceptionDate)

UPRDays<-as.numeric(expiryDate-ValuationDate)
UPRDays

UPRDays/duration

pureIBNRDays/duration

