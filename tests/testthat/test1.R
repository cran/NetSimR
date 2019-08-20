
library("NetSimR")

LNormCappedMean(1000,c(6,6.5,7),1.5)
#502.97
ExposureCurveLNorm(1000,6,1.5)
#0.40476
ExposureCurveLNorm(c(1000,1100,1200),6,c(1.5,1.5,1.6))
ILFLNorm(1000,c(1200,1300,1400),6,1.5)


GammaCappedMean(1000, 1000*1000/100/100, 1000/100/100)
#960.14
ExposureCurveGamma(1000, 1000*1000/100/100, 1000/100/100)
#0.9601
ExposureCurveGamma(c(1000,500,200),1000*1000/100/100, 1000/100/100)
ILFGamma(500,1000,1000*1000/100/100, 1000/100/100)


ParetoCappedMean(800,100,1.1)
#288
ParetoCappedMean(c(800,900,1000),100,1.1)
ExposureCurvePareto(800,100,1.1)
#26.16
ILFPareto(800,900,100,1.1)
#1.033
ILFPareto(800,900,c(100,110),1.1)


SlicedLNormParetoMean(6,1.5,2000,1.8)
#700+(4500-2000)*0.1429253=1057
SlicedLNormParetoMean(c(5,5.5,6),1.5,1000,1.1)
ExposureCurveSlicedLNormPareto(c(800,900,1000),5,1.5,1000,1.1)


Dates = data.frame(
  inceptionDate = c("01/01/2006", "01/07/2006", "01/01/2007")
  ,expiryDate = c("31/12/2006", "30/06/2007", "31/12/2007")
)
Dates$inceptionDate<-as.POSIXct(Dates$inceptionDate, format="%d/%m/%Y")
Dates$expiryDate<-as.POSIXct(Dates$expiryDate, format="%d/%m/%Y")
ValuationDate<-as.POSIXct("30/10/2007", format="%d/%m/%Y")

ifelse(ValuationDate<Dates$expiryDate,0,difftime(ValuationDate, Dates$expiryDate, units="days"))
difftime(Dates$expiryDate,Dates$inceptionDate, units="days")

PureIBNRLNorm(Dates$inceptionDate,Dates$expiryDate,ValuationDate,4,1.5)
