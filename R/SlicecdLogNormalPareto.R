##################################
#Sliced LogNormal-Pareto Functions
##################################

#' Sliced LogNormal Pareto mean
#'
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' SlicedLNormParetoMean(6,1.5,1000,1.2)
#' SlicedLNormParetoMean(6.5,1.4,2000,1.6)
#' SlicedLNormParetoMean(7,1.6,3000,1.4)
SlicedLNormParetoMean<-function(mu, sigma, SlicePoint, shape){
  df<-data.frame(mu, sigma, SlicePoint, shape)
  ifelse(df$shape>1
         ,LNormCappedMean(SlicePoint, mu, sigma)+plnorm(SlicePoint, mu, sigma, FALSE)*((shape*SlicePoint)/(shape-1)-SlicePoint)
         ,Inf
  )
}



#' Sliced LogNormal Pareto capped mean
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' SlicedLNormParetoCappedMean(1200,6,1.5,1000,1.2)
#' SlicedLNormParetoCappedMean(2500,6.5,1.4,2000,1.6)
#' SlicedLNormParetoCappedMean(4000,7,1.6,3000,1.4)
SlicedLNormParetoCappedMean<-function(cap,mu, sigma, SlicePoint, shape){
  df<-data.frame(cap,mu, sigma, SlicePoint, shape)
  ifelse(df$cap<=df$SlicePoint
         ,LNormCappedMean(cap, mu, sigma)
         ,LNormCappedMean(SlicePoint, mu, sigma)+plnorm(SlicePoint,mu,sigma,lower.tail = FALSE)*(ParetoCappedMean(cap, SlicePoint, shape)-SlicePoint)
  )
}



#' Exposure Curve from a Sliced LogNormal Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the exposure curve will be evaluated.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' ExposureCurveSlicedLNormPareto(1200,6,1.5,1000,1.2)
#' ExposureCurveSlicedLNormPareto(4000,7,1.6,3000,1.4)
ExposureCurveSlicedLNormPareto<-function(x, mu, sigma, SlicePoint, shape){
  df<-data.frame(x, mu, sigma, SlicePoint, shape)
  ifelse(df$shape>1
         ,SlicedLNormParetoCappedMean(x, mu, sigma, SlicePoint, shape)/SlicedLNormParetoMean(mu, sigma, SlicePoint, shape)
         ,0
  )
}



#' Increased Limit Factor Curve from a Sliced LogNormal Pareto severity distribution
#'
#' @param xLow A positive real number -  the claim amount where the Limit Factor Curve will be evaluated from.
#' @param xHigh A positive real number -  the claim amount where the Limit Factor Curve will be evaluated to.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' ILFSlicedLNormPareto(800,1200,6,1.5,1000,1.2)
#' ILFSlicedLNormPareto(2000,4000,7,1.6,3000,1.4)
ILFSlicedLNormPareto<-function(xLow,xHigh, mu, sigma, SlicePoint, shape){
  SlicedLNormParetoCappedMean(xHigh, mu, sigma, SlicePoint, shape)/SlicedLNormParetoCappedMean(xLow, mu, sigma, SlicePoint, shape)
}



#' The cumulative density function (cdf) of a Sliced LogNormal Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the cumulative density function (cdf) will be evaluated.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the cumulative density function (cdf) at \code{x} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' pSlicedLNormPareto(1200,6,1.5,1000,1.2)
#' pSlicedLNormPareto(4000,7,1.6,3000,1.4)
pSlicedLNormPareto<-function(x, mu, sigma, SlicePoint, shape){
  ifelse(x>SlicePoint
         ,plnorm(SlicePoint, mu, sigma)+plnorm(SlicePoint, mu, sigma, FALSE)*(1-(SlicePoint/x)^shape)
         ,plnorm(x, mu, sigma)
  )
}



#' The inverse cumulative density function of a Sliced LogNormal Pareto severity distribution
#'
#' @param q A real number between 0 and 1 -  the probability where the inverse cumulative density function will be evaluated.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the inverse cumulative density function at \code{q} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' qSlicedLNormPareto(0.5,6,1.5,1000,1.2)
#' qSlicedLNormPareto(0.7,7,1.6,3000,1.4)
qSlicedLNormPareto<-function(q, mu, sigma, SlicePoint, shape){
  lp<-plnorm(SlicePoint, mu, sigma)
  up<-1-lp
  ifelse(q>lp
         ,SlicePoint/((1-((q-lp)/up))^(1/shape))
         ,qlnorm(q,mu, sigma)
  )
}



#' The probability density function (pdf) of a Sliced LogNormal Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the probability density function (pdf) will be evaluated.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the probability density function (pdf) at \code{x} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @export
#' @examples
#' dSlicedLNormPareto(1200,6,1.5,1000,1.2)
#' dSlicedLNormPareto(4000,7,1.6,3000,1.4)
dSlicedLNormPareto<-function(x, mu, sigma, SlicePoint, shape){
  ifelse(x>SlicePoint
         ,plnorm(SlicePoint, mu, sigma, FALSE)*(shape*SlicePoint^shape)/(x^(shape+1))
         ,dlnorm(x, mu, sigma)
  )
}
