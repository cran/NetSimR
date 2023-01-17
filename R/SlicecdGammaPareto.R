##############################
#Sliced Gamma-Pareto Functions
##############################

#' Sliced Gamma Pareto mean
#'
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the Shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' SlicedGammaParetoMean(1,0.0005,1000,1.2)
#' SlicedGammaParetoMean(1.1,0.0006,2000,1.6)
#' SlicedGammaParetoMean(1.2,0.0004,3000,1.4)
SlicedGammaParetoMean<-function(GShape, GRate, SlicePoint, PShape){
  df<-data.frame(GShape, GRate, SlicePoint, PShape)
  ifelse(df$PShape>1
         ,GammaCappedMean(SlicePoint,GShape,GRate)+pgamma(SlicePoint,GShape,GRate,lower.tail = FALSE)*((PShape*SlicePoint)/(PShape-1)-SlicePoint)
         ,Inf
  )
}



#' Sliced Gamma Pareto capped mean
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' SlicedGammaParetoCappedMean(3000,1,0.0005,1000,1.2)
#' SlicedGammaParetoCappedMean(1000,1.1,0.0006,2000,1.6)
#' SlicedGammaParetoCappedMean(2000,1.2,0.0004,3000,1.4)
SlicedGammaParetoCappedMean<-function(cap, GShape, GRate, SlicePoint, PShape){
  ifelse(cap<=SlicePoint
         ,GammaCappedMean(cap,GShape,GRate)
         ,GammaCappedMean(SlicePoint,GShape,GRate)+pgamma(SlicePoint,GShape,GRate,lower.tail = FALSE)*(ParetoCappedMean(cap, SlicePoint, PShape)-SlicePoint)
  )
}



#' Exposure Curve from a Sliced Gamma Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the exposure curve will be evaluated.
#' @param GShape A positive real number -  the shape parameter of the Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' ExposureCurveSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#' ExposureCurveSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#' ExposureCurveSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
ExposureCurveSlicedGammaPareto<-function(x, GShape, GRate, SlicePoint, PShape){
  df<-data.frame(x, GShape, GRate, SlicePoint, PShape)
  ifelse(df$PShape>1
         ,SlicedGammaParetoCappedMean(x, GShape, GRate, SlicePoint, PShape)/SlicedGammaParetoMean(GShape, GRate, SlicePoint, PShape)
         ,0
  )
}



#' Increased Limit Factor Curve from a Sliced Gamma Pareto severity distribution
#'
#' @param xLow A positive real number -  the claim amount where the Limit Factor Curve will be evaluated from.
#' @param xHigh A positive real number -  the claim amount where the Limit Factor Curve will be evaluated to.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' ILFSlicedGammaPareto(2000,3000,1,0.0005,1000,1.2)
#' ILFSlicedGammaPareto(800,1000,1.1,0.0006,2000,1.6)
#' ILFSlicedGammaPareto(1200,2000,1.2,0.0004,3000,1.4)
ILFSlicedGammaPareto<-function(xLow, xHigh, GShape, GRate, SlicePoint, PShape){
  SlicedGammaParetoCappedMean(xHigh, GShape, GRate, SlicePoint, PShape)/SlicedGammaParetoCappedMean(xLow, GShape, GRate, SlicePoint, PShape)
}



#' The cumulative density function (cdf) of a Sliced Gamma-Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the cumulative density function (cdf) will be evaluated.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the cumulative density function (cdf) at \code{x} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' pSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#' pSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#' pSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
pSlicedGammaPareto<-function(x, GShape, GRate, SlicePoint, PShape){
  ifelse(x>SlicePoint
         ,pgamma(SlicePoint, GShape, GRate)+pgamma(SlicePoint, GShape, GRate, lower.tail = FALSE)*(1-(SlicePoint/x)^PShape)
         ,pgamma(x, GShape, GRate)
  )
}



#' The inverse cumulative density function of a Sliced Gamma Pareto severity distribution
#'
#' @param q A real number between 0 and 1 -  the probability where the inverse cumulative density function will be evaluated.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the inverse cumulative density function at \code{q} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' qSlicedGammaPareto(0.5,1,0.0005,1000,1.2)
#' qSlicedGammaPareto(0.2,1.1,0.0006,2000,1.6)
#' qSlicedGammaPareto(0.8,1.2,0.0004,3000,1.4)
qSlicedGammaPareto<-function(q, GShape, GRate, SlicePoint, PShape){
  lp<-pgamma(SlicePoint, GShape, GRate)
  up<-1-lp
  ifelse(q>lp
         ,SlicePoint/((1-((q-lp)/up))^(1/PShape))
         ,qgamma(q, GShape, GRate)
  )
}



#' The probability density function (pdf) of a Sliced Gamma Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the probability density function (pdf) will be evaluated.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the probability density function (pdf) at \code{x} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @export
#' @examples
#' dSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#' dSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#' dSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
dSlicedGammaPareto<-function(x, GShape, GRate, SlicePoint, PShape){
  ifelse(x>SlicePoint
         ,pgamma(SlicePoint, GShape, GRate, lower.tail = FALSE)*(PShape*SlicePoint^PShape)/(x^(PShape+1))
         ,dgamma(x, GShape, GRate)
  )
}

