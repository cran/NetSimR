
####################
#LogNormal Functions
####################


#' Error function
#'
#' @param x A real number.
#' @return The value of the error function at \code{x}.
#' @examples
#' erf(0.1)
#' erf(0.5)
erf<-function(x){2*pnorm(sqrt(2)*x)-1}



#' Lognormal capped mean
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param mu A real number - the first parameter of the Claim Severity's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the Claim Severity's LogNormal distribution.
#' @return The mean of the claim severity capped at \code{cap} with a LogNormal distribution with parameters \code{mu} and \code{sigma}.
#' @examples
#' LNormCappedMean(2000,6,1.5)
#' LNormCappedMean(1000,5,1.6)
LNormCappedMean<- function(cap,mu,sigma){
  cap-(0.5*(exp(mu+0.5*sigma*sigma)*erf((mu+sigma*sigma-log(cap))/(sigma*sqrt(2)))+cap+cap*erf((log(cap)-mu)/(sqrt(2)*sigma)))-0.5*exp(mu+0.5*sigma*sigma))
}



#' Exposure Curve from LogNormal a severity distribution
#'
#' @param x A positive real number -  the claim amount where the exposure curve will be evaluated.
#' @param mu A real number - the first parameter of the Claim Severity's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the Claim Severity's LogNormal distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a LogNormal distribution with parameters \code{mu} and \code{sigma}.
#' @examples
#' ExposureCurveLNorm(2000,6,1.5)
#' ExposureCurveLNorm(1000,5,1.6)
ExposureCurveLNorm<-function(x,mu,sigma){
  LNormCappedMean(x,mu,sigma)/(exp(mu+0.5*sigma*sigma))
}



#' Increased Limit Factor Curve from a LogNormal severity distribution
#'
#' @param xLow A positive real number -  the claim amount where the Increased Limit Factor Curve will be evaluated from.
#' @param xHigh A positive real number -  the claim amount where the Increased Limit Factor Curve will be evaluated to.
#' @param mu A real number - the first parameter of the Claim Severity's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the Claim Severity's LogNormal distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with Claim Severity from a LogNormal distribution with parameters \code{mu} and \code{sigma}.
#' @examples
#' ILFLNorm(1000,2000,6,1.5)
#' ILFLNorm(1000,1500,5,1.6)
ILFLNorm<-function(xLow,xHigh,mu,sigma){
  LNormCappedMean(xHigh,mu,sigma)/LNormCappedMean(xLow,mu,sigma)
}



#' Pure IBNR exposure from a LogNormal reporting delay distribution
#'
#' @param IncDate A date -  the inception date of the period.
#' @param ExpDate A date -  the expiry date of the period. Must be greater than inception date.
#' @param ValDate A date -  the valuation date.
#' @param mu A real number - the first parameter of the reporing delay's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the reporing delay's LogNormal distribution.
#' @return Unearned and Pure IBNR exposure in days and as a percentage of the period's duration, where the reporting delay has a LogNormal distribution with parameters \code{mu} and \code{sigma}.
#' @examples
#' Dates = data.frame(
#'     inceptionDate = c("01/01/2006", "01/07/2006", "01/01/2007")
#'     ,expiryDate = c("31/12/2006", "30/06/2007", "31/12/2007")
#' )
#'
#' Dates$inceptionDate<-as.POSIXct(Dates$inceptionDate, format="%d/%m/%Y")
#'
#' Dates$expiryDate<-as.POSIXct(Dates$expiryDate, format="%d/%m/%Y")
#'
#' ValuationDate<-as.POSIXct("30/10/2007", format="%d/%m/%Y")
#'
#' PureIBNRLNorm(Dates$inceptionDate,Dates$expiryDate,ValuationDate,4,1.5)
PureIBNRLNorm <- function(IncDate, ExpDate, ValDate, mu, sigma){
  MinRepDelay<-ifelse(ValDate<ExpDate,0,difftime(ValDate, ExpDate, units="days"))
  MaxRepDelay<-ifelse(ValDate<IncDate,0,difftime(ValDate, IncDate, units="days"))
  Duration<-as.numeric(difftime(ExpDate,IncDate, units="days"))
  EarnedDuration<-MaxRepDelay-MinRepDelay
  UnearnedDuration<-Duration-EarnedDuration
  UnearnedDurationRatio<-ifelse(Duration==0,0,round(UnearnedDuration/Duration,5))
  PureIBNRDuration<-round(LNormCappedMean(MaxRepDelay,mu,sigma)-LNormCappedMean(MinRepDelay,mu,sigma),2)
  PureIBNRDurationRatio<-ifelse(Duration==0,0,round(PureIBNRDuration/Duration,5))
  data.frame(UnearnedDuration,PureIBNRDuration,UnearnedDurationRatio,PureIBNRDurationRatio)
}
