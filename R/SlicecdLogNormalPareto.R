##################################
#Sliced LogNormal-Pareto Functions
##################################

#' Sliced LogNormal Pareto mean
#'
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}. A non-numeric argument or a non-positive \code{sigma}, \code{SlicePoint} or \code{shape} is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' SlicedLNormParetoMean(6,1.5,1000,1.2)
#' SlicedLNormParetoMean(6.5,1.4,2000,1.6)
#' SlicedLNormParetoMean(7,1.6,3000,1.4)
SlicedLNormParetoMean<-function(mu, sigma, SlicePoint, shape){
  check_numeric(mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  # the Pareto tail adds S(SlicePoint) * SlicePoint / (shape - 1) to the capped mean at the
  # slice point; it adds nothing when the tail is never reached (S(SlicePoint) = 0, as for
  # an infinite slice point, where 0 * Inf would be NaN)
  up<-plnorm(SlicePoint, mu, sigma, lower.tail = FALSE)
  tailTerm<-up*SlicePoint/(shape-1)
  tailTerm[which(up==0)]<-0
  restore_shape(ifelse(shape>1 | SlicePoint==Inf
                       ,LNormCappedMean(SlicePoint, mu, sigma)+tailTerm
                       ,Inf
  ), args)
}



#' Sliced LogNormal Pareto capped mean
#'
#' @param cap A non-negative real number -  the claim severity cap.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}. A non-numeric argument, a negative \code{cap} or a non-positive \code{sigma}, \code{SlicePoint} or \code{shape} is an error; \code{NA} values give \code{NA}.
#' @family capped mean functions
#' @export
#' @examples
#' SlicedLNormParetoCappedMean(1200,6,1.5,1000,1.2)
#' SlicedLNormParetoCappedMean(2500,6.5,1.4,2000,1.6)
#' SlicedLNormParetoCappedMean(4000,7,1.6,3000,1.4)
SlicedLNormParetoCappedMean<-function(cap,mu, sigma, SlicePoint, shape){
  check_positive(cap = cap, allow_zero = TRUE)
  check_numeric(mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(cap = cap, mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  cap<-args$cap; mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  restore_shape(ifelse(cap<=SlicePoint
                       ,LNormCappedMean(cap, mu, sigma)
                       ,LNormCappedMean(SlicePoint, mu, sigma)+plnorm(SlicePoint,mu,sigma,lower.tail = FALSE)*(ParetoCappedMean(cap, SlicePoint, shape)-SlicePoint)
  ), args)
}



#' Exposure Curve from a Sliced LogNormal Pareto severity distribution
#'
#' @param x A non-negative real number -  the claim amount where the exposure curve will be evaluated.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}. The exposure curve divides by the mean, which is infinite when \code{shape <= 1} (and \code{SlicePoint} is finite); the function returns 0 in that case.
#' @family exposure curve functions
#' @export
#' @examples
#' ExposureCurveSlicedLNormPareto(1200,6,1.5,1000,1.2)
#' ExposureCurveSlicedLNormPareto(4000,7,1.6,3000,1.4)
ExposureCurveSlicedLNormPareto<-function(x, mu, sigma, SlicePoint, shape){
  check_positive(x = x, allow_zero = TRUE)
  check_numeric(mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  x<-args$x; mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  slicedMean<-SlicedLNormParetoMean(mu, sigma, SlicePoint, shape)
  restore_shape(ifelse(slicedMean<Inf
                       ,SlicedLNormParetoCappedMean(x, mu, sigma, SlicePoint, shape)/slicedMean
                       ,0
  ), args)
}



#' Increased Limit Factor Curve from a Sliced LogNormal Pareto severity distribution
#'
#' @param xLow A non-negative real number -  the claim amount where the Limit Factor Curve will be evaluated from.
#' @param xHigh A non-negative real number -  the claim amount where the Limit Factor Curve will be evaluated to.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
#' @family ILF functions
#' @export
#' @examples
#' ILFSlicedLNormPareto(800,1200,6,1.5,1000,1.2)
#' ILFSlicedLNormPareto(2000,4000,7,1.6,3000,1.4)
ILFSlicedLNormPareto<-function(xLow,xHigh, mu, sigma, SlicePoint, shape){
  check_positive(xLow = xLow, xHigh = xHigh, allow_zero = TRUE)
  check_numeric(mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that both capped means have it
  args<-recycle_arguments(xLow = xLow, xHigh = xHigh, mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  xLow<-args$xLow; xHigh<-args$xHigh; mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  restore_shape(SlicedLNormParetoCappedMean(xHigh, mu, sigma, SlicePoint, shape)/SlicedLNormParetoCappedMean(xLow, mu, sigma, SlicePoint, shape), args)
}



#' The cumulative density function (cdf) of a Sliced LogNormal Pareto severity distribution
#'
#' @param x A real number -  the claim amount where the cumulative density function (cdf) will be evaluated. The cdf is 0 for negative \code{x}.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the cumulative density function (cdf) at \code{x} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}. A non-numeric argument or a non-positive \code{sigma}, \code{SlicePoint} or \code{shape} is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' pSlicedLNormPareto(1200,6,1.5,1000,1.2)
#' pSlicedLNormPareto(4000,7,1.6,3000,1.4)
pSlicedLNormPareto<-function(x, mu, sigma, SlicePoint, shape){
  check_numeric(x = x, mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  x<-args$x; mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  restore_shape(ifelse(x>SlicePoint
                       ,plnorm(SlicePoint, mu, sigma)+plnorm(SlicePoint, mu, sigma, FALSE)*(1-(SlicePoint/x)^shape)
                       ,plnorm(x, mu, sigma)
  ), args)
}



#' The inverse cumulative density function of a Sliced LogNormal Pareto severity distribution
#'
#' @param q A real number between 0 and 1 -  the probability where the inverse cumulative density function will be evaluated. Values outside [0, 1] give \code{NaN}, as in \code{qlnorm()}.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.
#' @return The value of the inverse cumulative density function at \code{q} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}. A non-numeric argument or a non-positive \code{sigma}, \code{SlicePoint} or \code{shape} is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' qSlicedLNormPareto(0.5,6,1.5,1000,1.2)
#' qSlicedLNormPareto(0.7,7,1.6,3000,1.4)
qSlicedLNormPareto<-function(q, mu, sigma, SlicePoint, shape){
  check_numeric(q = q, mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(q = q, mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  q<-args$q; mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  lp<-plnorm(SlicePoint, mu, sigma)
  up<-plnorm(SlicePoint, mu, sigma, lower.tail = FALSE)
  # above the slice point 1 - q = up * (SlicePoint / x)^shape; using 1 - q directly
  # (rather than 1 - (q - lp) / up) keeps the precision as q -> 1
  restore_shape(ifelse(q>lp
                       ,SlicePoint/(((1-q)/up)^(1/shape))
                       ,qlnorm(q,mu, sigma)
  ), args)
}



#' The probability density function (pdf) of a Sliced LogNormal Pareto severity distribution
#'
#' @param x A real number -  the claim amount where the probability density function (pdf) will be evaluated. The pdf is 0 for negative \code{x}.
#' @param mu A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.
#' @param sigma A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.
#' @param SlicePoint A positive real number - the slice point and the scale parameter of the Claim Severity's Pareto distribution. An infinite slice point gives the LogNormal distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the probability density function (pdf) at \code{x} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}. A non-numeric argument or a non-positive \code{sigma}, \code{SlicePoint} or \code{shape} is an error; \code{NA} values give \code{NA}.
#' @family sliced distribution functions
#' @export
#' @examples
#' dSlicedLNormPareto(1200,6,1.5,1000,1.2)
#' dSlicedLNormPareto(4000,7,1.6,3000,1.4)
dSlicedLNormPareto<-function(x, mu, sigma, SlicePoint, shape){
  check_numeric(x = x, mu = mu)
  check_positive(sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, mu = mu, sigma = sigma, SlicePoint = SlicePoint, shape = shape)
  x<-args$x; mu<-args$mu; sigma<-args$sigma; SlicePoint<-args$SlicePoint; shape<-args$shape
  # the Pareto density shape * SlicePoint^shape / x^(shape + 1), written with the ratio
  # SlicePoint / x so that the powers do not overflow for large slice points or shapes
  restore_shape(ifelse(x>SlicePoint
                       ,plnorm(SlicePoint, mu, sigma, FALSE)*shape/x*(SlicePoint/x)^shape
                       ,dlnorm(x, mu, sigma)
  ), args)
}
