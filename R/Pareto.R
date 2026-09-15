#################
#Pareto Functions
#################

#' Pareto capped mean intermediary calculation
#'
#' @param cap A non-negative real number -  the claim severity cap.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return An interim calculation for the mean of the claim severity capped at \code{cap} with a Pareto distribution with parameters \code{scale} and \code{shape}. It is the closed form for \code{cap >= scale} and \code{shape != 1}; use \code{\link{ParetoCappedMean}} for the capped mean itself. The arguments are recycled to a common length.
#' @family capped mean functions
#' @export
#' @examples
#' ParetoCappedMeanCalc(800,100,1.1)
#' ParetoCappedMeanCalc(1000,500,0.9)
ParetoCappedMeanCalc<-function(cap,scale,shape){
  check_positive(cap = cap, allow_zero = TRUE)
  check_positive(scale = scale, shape = shape)
  # recycle every argument to a common length
  args<-recycle_arguments(cap = cap, scale = scale, shape = shape)
  cap<-args$cap; scale<-args$scale; shape<-args$shape
  # cap * (scale / cap)^shape = scale^shape * cap^(1 - shape), which is 0 for an
  # infinite cap when shape > 1 (finite mean) and Inf when shape < 1 (infinite mean)
  capTerm <- cap * (scale/cap)^shape
  infiniteCap <- which(cap == Inf)
  if (length(infiniteCap) > 0) {
    capTerm[infiniteCap] <- ifelse(shape[infiniteCap] > 1, 0, Inf)
  }
  restore_shape((shape * scale - capTerm)/(shape-1), args)
}



#' Pareto capped mean
#'
#' @param cap A non-negative real number -  the claim severity cap.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with a Pareto distribution with parameters \code{scale} and \code{shape}. A cap at or below \code{scale} is returned unchanged, as no claim is smaller than \code{scale}. The arguments are recycled to a common length. A non-numeric argument, a negative \code{cap} or a non-positive \code{scale} or \code{shape} is an error; \code{NA} values give \code{NA}.
#' @family capped mean functions
#' @export
#' @examples
#' ParetoCappedMean(600,200,1.2)
#' ParetoCappedMean(800,100,1)
#' ParetoCappedMean(1000,500,0.8)
#' ParetoCappedMean(50,100,2)
ParetoCappedMean<-function(cap,scale,shape){
  check_positive(cap = cap, allow_zero = TRUE)
  check_positive(scale = scale, shape = shape)
  # recycle every argument to a common length
  args<-recycle_arguments(cap = cap, scale = scale, shape = shape)
  cap<-args$cap; scale<-args$scale; shape<-args$shape
  # E[min(X, cap)] = scale + integral of (scale / x)^shape from scale to cap
  #                = scale * (1 + (exp((1 - shape) * L) - 1) / (1 - shape)), L = log(cap / scale).
  # expm1() keeps this accurate as shape -> 1, where the limit is scale * (1 + L).
  # pmax() avoids log() warnings for caps below the scale, which are returned unchanged.
  logRatio<-log(pmax(cap/scale,1))
  oneMinusShape<-1-shape
  tailTerm<-ifelse(oneMinusShape==0
                   ,logRatio
                   ,expm1(oneMinusShape*logRatio)/oneMinusShape
  )
  restore_shape(ifelse(cap<=scale
                       ,cap
                       ,scale*(1+tailTerm)
  ), args)
}



#' Exposure Curve from a Pareto severity distribution
#'
#' @param x A non-negative real number -  the claim amount where the exposure curve will be evaluated.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a Pareto distribution with parameters \code{scale} and \code{shape}. The exposure curve divides by the mean, which is infinite when \code{shape <= 1}; the function returns 0 in that case.
#' @family exposure curve functions
#' @export
#' @examples
#' ExposureCurvePareto(700,500,1.2)
#' ExposureCurvePareto(20000,200,1.1)
ExposureCurvePareto<-function(x,scale,shape){
  check_positive(x = x, allow_zero = TRUE)
  check_positive(scale = scale, shape = shape)
  # recycle every argument to a common length, so that ifelse() keeps them all
  args<-recycle_arguments(x = x, scale = scale, shape = shape)
  x<-args$x; scale<-args$scale; shape<-args$shape
  restore_shape(ifelse(shape>1
                       ,ParetoCappedMean(x,scale,shape)*(shape-1)/shape/scale
                       ,0
  ), args)
}



#' Increased Limit Factor Curve from a Pareto severity distribution
#'
#' @param xLow A non-negative real number -  the claim amount where the Increased Limit Factor Curve will be evaluated from.
#' @param xHigh A non-negative real number -  the claim amount where the Increased Limit Factor Curve will be evaluated to.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with Claim Severity from a Pareto distribution with parameters \code{scale} and \code{shape}.
#' @family ILF functions
#' @export
#' @examples
#' ILFPareto(700,1200,500,1.2)
#' ILFPareto(1200,20000,200,1.1)
ILFPareto<-function(xLow,xHigh,scale,shape){
  check_positive(xLow = xLow, xHigh = xHigh, allow_zero = TRUE)
  check_positive(scale = scale, shape = shape)
  # recycle every argument to a common length, so that both capped means have it
  args<-recycle_arguments(xLow = xLow, xHigh = xHigh, scale = scale, shape = shape)
  xLow<-args$xLow; xHigh<-args$xHigh; scale<-args$scale; shape<-args$shape
  restore_shape(ParetoCappedMean(xHigh,scale,shape)/ParetoCappedMean(xLow,scale,shape), args)
}
