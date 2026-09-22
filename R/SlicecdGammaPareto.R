##############################
#Sliced Gamma-Pareto Functions
##############################

#' Sliced Gamma Pareto mean
#'
#' @description Gives the expected claim amount of a sliced severity distribution, with Gamma attritional claims below the slice point and a Pareto tail above it.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}. The mean is \code{Inf} when \code{PShape <= 1} (and \code{SlicePoint} is finite), as the Pareto tail then has no finite mean. A non-numeric or non-positive parameter is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' SlicedGammaParetoMean(1,0.0005,1000,1.2)
#' SlicedGammaParetoMean(1.1,0.0006,2000,1.6)
#' SlicedGammaParetoMean(1.2,0.0004,3000,1.4)
SlicedGammaParetoMean<-function(GShape, GRate, SlicePoint, PShape){
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  # the Pareto tail adds S(SlicePoint) * SlicePoint / (PShape - 1) to the capped mean at the
  # slice point; it adds nothing when the tail is never reached (S(SlicePoint) = 0, as for
  # an infinite slice point, where 0 * Inf would be NaN)
  up<-pgamma(SlicePoint,GShape,GRate,lower.tail = FALSE)
  tailTerm<-up*SlicePoint/(PShape-1)
  tailTerm[which(up==0)]<-0
  restore_shape(ifelse(PShape>1 | SlicePoint==Inf
                       ,GammaCappedMean(SlicePoint,GShape,GRate)+tailTerm
                       ,Inf
  ), args)
}



#' Sliced Gamma Pareto capped mean
#'
#' @description Gives the expected claim amount when each claim from a sliced severity distribution, with Gamma attritional claims below the slice point and a Pareto tail above it, is capped at \code{cap}.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param cap A non-negative real number -  the claim severity cap.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}. A non-numeric argument, a negative \code{cap} or a non-positive parameter is an error; \code{NA} values give \code{NA}.
#' @family capped mean functions
#' @export
#' @examples
#' SlicedGammaParetoCappedMean(3000,1,0.0005,1000,1.2)
#' SlicedGammaParetoCappedMean(1000,1.1,0.0006,2000,1.6)
#' SlicedGammaParetoCappedMean(2000,1.2,0.0004,3000,1.4)
SlicedGammaParetoCappedMean<-function(cap, GShape, GRate, SlicePoint, PShape){
  check_positive(cap = cap, allow_zero = TRUE)
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(cap = cap, GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  cap<-args$cap; GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  # the Pareto tail adds S(SlicePoint) * (Pareto capped mean - SlicePoint) to the capped mean
  # at the slice point; it adds nothing when the tail is never reached (S(SlicePoint) = 0),
  # unless the Pareto capped mean is infinite (an infinite cap with PShape <= 1), where the
  # result is the infinite mean, as in SlicedGammaParetoMean (0 * Inf would be NaN)
  up<-pgamma(SlicePoint,GShape,GRate,lower.tail = FALSE)
  paretoTail<-ParetoCappedMean(cap, SlicePoint, PShape)-SlicePoint
  tailTerm<-up*paretoTail
  tailTerm[which(up==0)]<-0
  tailTerm[which(paretoTail==Inf)]<-Inf
  restore_shape(ifelse(cap<=SlicePoint
                       ,GammaCappedMean(cap,GShape,GRate)
                       ,GammaCappedMean(SlicePoint,GShape,GRate)+tailTerm
  ), args)
}



#' Exposure Curve from a Sliced Gamma Pareto severity distribution
#'
#' @description Gives the share of the expected claim cost of a sliced Gamma-Pareto severity distribution that falls below the amount \code{x} (the capped mean divided by the mean), as used to exposure rate a layer.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param x A non-negative real number -  the claim amount where the exposure curve will be evaluated.
#' @param GShape A positive real number -  the shape parameter of the Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}. The exposure curve divides by the mean, which is infinite when \code{PShape <= 1} (and \code{SlicePoint} is finite); the function returns 0 in that case.
#' @family exposure curve functions
#' @export
#' @examples
#' ExposureCurveSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#' ExposureCurveSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#' ExposureCurveSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
ExposureCurveSlicedGammaPareto<-function(x, GShape, GRate, SlicePoint, PShape){
  check_positive(x = x, allow_zero = TRUE)
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  x<-args$x; GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  slicedMean<-SlicedGammaParetoMean(GShape, GRate, SlicePoint, PShape)
  restore_shape(ifelse(slicedMean<Inf
                       ,SlicedGammaParetoCappedMean(x, GShape, GRate, SlicePoint, PShape)/slicedMean
                       ,0
  ), args)
}



#' Increased Limit Factor Curve from a Sliced Gamma Pareto severity distribution
#'
#' @description Gives the ratio of the sliced Gamma-Pareto capped mean at \code{xHigh} to that at \code{xLow}, the factor that takes the expected cost of a policy limit of \code{xLow} to that of a limit of \code{xHigh}.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param xLow A non-negative real number -  the claim amount where the Limit Factor Curve will be evaluated from.
#' @param xHigh A non-negative real number -  the claim amount where the Limit Factor Curve will be evaluated to.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}.
#' @family ILF functions
#' @export
#' @examples
#' ILFSlicedGammaPareto(2000,3000,1,0.0005,1000,1.2)
#' ILFSlicedGammaPareto(800,1000,1.1,0.0006,2000,1.6)
#' ILFSlicedGammaPareto(1200,2000,1.2,0.0004,3000,1.4)
ILFSlicedGammaPareto<-function(xLow, xHigh, GShape, GRate, SlicePoint, PShape){
  check_positive(xLow = xLow, xHigh = xHigh, allow_zero = TRUE)
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that both capped means have it
  args<-recycle_arguments(xLow = xLow, xHigh = xHigh, GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  xLow<-args$xLow; xHigh<-args$xHigh; GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  restore_shape(SlicedGammaParetoCappedMean(xHigh, GShape, GRate, SlicePoint, PShape)/SlicedGammaParetoCappedMean(xLow, GShape, GRate, SlicePoint, PShape), args)
}



#' The cumulative distribution function (cdf) of a Sliced Gamma-Pareto severity distribution
#'
#' @description Gives the probability that a claim from a sliced severity distribution, with Gamma attritional claims below the slice point and a Pareto tail above it, is at most \code{x}.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param x A real number -  the claim amount where the cumulative distribution function (cdf) will be evaluated. The cdf is 0 for negative \code{x}.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the cumulative distribution function (cdf) at \code{x} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}. A non-numeric argument or a non-positive parameter is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' pSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#' pSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#' pSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
pSlicedGammaPareto<-function(x, GShape, GRate, SlicePoint, PShape){
  check_numeric(x = x)
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  x<-args$x; GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  restore_shape(ifelse(x>SlicePoint
                       ,pgamma(SlicePoint, GShape, GRate)+pgamma(SlicePoint, GShape, GRate, lower.tail = FALSE)*(1-(SlicePoint/x)^PShape)
                       ,pgamma(x, GShape, GRate)
  ), args)
}



#' The inverse cumulative distribution function of a Sliced Gamma Pareto severity distribution
#'
#' @description Gives the claim amount that a claim from a sliced severity distribution, with Gamma attritional claims below the slice point and a Pareto tail above it, stays at or below with probability \code{q}; the inverse of \code{\link{pSlicedGammaPareto}}.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param q A real number between 0 and 1 -  the probability where the inverse cumulative distribution function will be evaluated. Values outside [0, 1] give \code{NaN} with a warning, as in \code{qgamma()}.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the inverse cumulative distribution function at \code{q} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}. A non-numeric argument or a non-positive parameter is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' qSlicedGammaPareto(0.5,1,0.0005,1000,1.2)
#' qSlicedGammaPareto(0.2,1.1,0.0006,2000,1.6)
#' qSlicedGammaPareto(0.8,1.2,0.0004,3000,1.4)
qSlicedGammaPareto<-function(q, GShape, GRate, SlicePoint, PShape){
  check_numeric(q = q)
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(q = q, GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  q<-args$q; GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  lp<-pgamma(SlicePoint, GShape, GRate)
  up<-pgamma(SlicePoint, GShape, GRate, lower.tail = FALSE)
  # above the slice point 1 - q = up * (SlicePoint / x)^PShape; using 1 - q directly
  # (rather than 1 - (q - lp) / up) keeps the precision as q -> 1. Probabilities above 1
  # are left to qgamma(), which gives NaN with a warning; the Pareto branch would give a
  # real number for a negative (1 - q) when 1 / PShape is a whole number
  restore_shape(ifelse(q>lp & q<=1
                       ,SlicePoint/(((1-q)/up)^(1/PShape))
                       ,qgamma(q, GShape, GRate)
  ), args)
}



#' The probability density function (pdf) of a Sliced Gamma Pareto severity distribution
#'
#' @description Gives the probability density at the claim amount \code{x} of a sliced severity distribution, with Gamma attritional claims below the slice point and a Pareto tail above it.
#'
#' @details \code{PShape} is the Pareto shape parameter, usually written alpha; the sliced LogNormal-Pareto functions call the same parameter \code{shape}.
#'
#' @param x A real number -  the claim amount where the probability density function (pdf) will be evaluated. The pdf is 0 for negative \code{x}.
#' @param GShape A positive real number -  the shape parameter of the attritional Claim Severity's Gamma distribution.
#' @param GRate A positive real number -  the rate parameter of the attritional Claim Severity's Gamma distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the Gamma distribution.
#' @param PShape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the probability density function (pdf) at \code{x} with an attritional claim Gamma distribution with parameters \code{GShape} and \code{GRate} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{PShape}. A non-numeric argument or a non-positive parameter is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' dSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#' dSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#' dSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
dSlicedGammaPareto<-function(x, GShape, GRate, SlicePoint, PShape){
  check_numeric(x = x)
  check_positive(GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, GShape = GShape, GRate = GRate, SlicePoint = SlicePoint, PShape = PShape)
  x<-args$x; GShape<-args$GShape; GRate<-args$GRate; SlicePoint<-args$SlicePoint; PShape<-args$PShape
  # the Pareto density PShape * SlicePoint^PShape / x^(PShape + 1), written with the ratio
  # SlicePoint / x so that the powers do not overflow for large slice points or shapes
  restore_shape(ifelse(x>SlicePoint
                       ,pgamma(SlicePoint, GShape, GRate, lower.tail = FALSE)*PShape/x*(SlicePoint/x)^PShape
                       ,dgamma(x, GShape, GRate)
  ), args)
}

