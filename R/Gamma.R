################
#Gamma Functions
################

#' Lower incomplete gamma function
#'
#' @param x A positive real number.
#' @param a A positive real number.
#' @return The value of the lower incomplete gamma function at \code{x} with shape parameter \code{a}.
#' @export
#' @examples
#' IGamma(1,1)
#' IGamma(0.1,2)
IGamma<-function(a,x){
  gamma(a) * pgamma(x, shape = a, scale = 1, lower.tail = FALSE)
}



#' Gamma capped mean
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the Claim Severity's Gamma distribution.
#' @return The mean of the claim severity capped at \code{cap} with a Gamma distribution with parameters \code{shape} and \code{rate}.
#' @export
#' @examples
#' GammaCappedMean(700,1,0.0005)
#' GammaCappedMean(1000,1.5,0.0006)
GammaCappedMean<- function(cap,shape,rate){
  (shape * gamma(shape) + rate * cap * IGamma(shape, rate * cap) - IGamma(1 + shape, rate * cap))/(rate * gamma(shape))
}



#' Exposure Curve from a Gamma severity distribution
#'
#' @param x A positive real number -  the claim amount where the exposure curve will be evaluated.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the Claim Severity's Gamma distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a Gamma distribution with parameters \code{shape} and \code{rate}.
#' @export
#' @examples
#' ExposureCurveGamma(700,1,0.0005)
#' ExposureCurveGamma(1000,1.5,0.0006)
ExposureCurveGamma<-function(x,shape,rate){
  GammaCappedMean(x,shape,rate)*rate/shape
}



#' Increased Limit Factor Curve from a Gamma severity distribution
#'
#' @param xLow A positive real number -  the claim amount where the Increased Limit Factor Curve will be evaluated from.
#' @param xHigh A positive real number -  the claim amount where the Increased Limit Factor Curve will be evaluated to.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the Claim Severity's Gamma distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with Claim Severity from a Gamma distribution with parameters \code{shape} and \code{rate}.
#' @export
#' @examples
#' ILFGamma(1000,700,1,0.0005)
#' ILFGamma(1200,1000,1.5,0.0006)
ILFGamma<-function(xLow,xHigh,shape,rate){
  GammaCappedMean(xHigh,shape,rate)/GammaCappedMean(xLow,shape,rate)
}



#' Pure IBNR exposure from a Gamma reporting delay distribution
#'
#' @param IncDate A date -  the inception date of the period.
#' @param ExpDate A date -  the expiry date of the period. Must be greater than inception date.
#' @param ValDate A date -  the valuation date.
#' @param shape A positive real number - the shape parameter of the reporing delay's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the reporing delay's Gamma distribution.
#' @return Unearned and Pure IBNR exposure in days and as a percentage of the period's duration, where the reporting delay has a Gamma distribution with parameters \code{shape} and \code{rate}.
#' @export
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
#' PureIBNRGamma(Dates$inceptionDate,Dates$expiryDate,ValuationDate,7,0.15)
##Pure IBNR Gamma
PureIBNRGamma <- function(IncDate, ExpDate, ValDate, shape, rate){
  MinRepDelay<-ifelse(ValDate<ExpDate,0,difftime(ValDate, ExpDate, units="days"))
  MaxRepDelay<-ifelse(ValDate<IncDate,0,difftime(ValDate, IncDate, units="days"))
  Duration<-as.numeric(difftime(ExpDate,IncDate, units="days"))
  EarnedDuration<-MaxRepDelay-MinRepDelay
  UnearnedDuration<-Duration-EarnedDuration
  UnearnedDurationRatio<-ifelse(Duration==0,0,round(UnearnedDuration/Duration,5))
  PureIBNRDuration<-round(GammaCappedMean(MaxRepDelay,shape,rate)-GammaCappedMean(MinRepDelay,shape,rate),2)
  PureIBNRDurationRatio<-ifelse(Duration==0,0,round(PureIBNRDuration/Duration,5))
  data.frame(UnearnedDuration,PureIBNRDuration,UnearnedDurationRatio,PureIBNRDurationRatio)
}
