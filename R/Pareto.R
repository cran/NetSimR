#################
#Pareto Functions
#################

#' Pareto capped mean intermediary calculation
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return An interim calculation for the mean of the claim severity capped at \code{cap} with a Pareto distribution with parameters \code{scale} and \code{shape}. It is the closed form for \code{cap >= scale} and \code{shape != 1}; use \code{\link{ParetoCappedMean}} for the capped mean itself.
#' @export
#' @examples
#' ParetoCappedMeanCalc(800,100,1.1)
#' ParetoCappedMeanCalc(1000,500,0.9)
ParetoCappedMeanCalc<-function(cap,scale,shape){
  # cap * (scale / cap)^shape = scale^shape * cap^(1 - shape), which is 0 for an
  # infinite cap when shape > 1 (finite mean) and Inf when shape < 1 (infinite mean)
  capTerm <- cap * (scale/cap)^shape
  # the arguments are recycled to the length of capTerm, so recycle cap and shape the same way
  infiniteCap <- which(rep_len(cap, length(capTerm)) == Inf)
  if (length(infiniteCap) > 0) {
    capTerm[infiniteCap] <- ifelse(rep_len(shape, length(capTerm))[infiniteCap] > 1, 0, Inf)
  }
  (shape * scale - capTerm)/(shape-1)
}



#' Pareto capped mean
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with a Pareto distribution with parameters \code{scale} and \code{shape}. A cap at or below \code{scale} is returned unchanged, as no claim is smaller than \code{scale}. The arguments are recycled to a common length.
#' @export
#' @examples
#' ParetoCappedMean(600,200,1.2)
#' ParetoCappedMean(800,100,1)
#' ParetoCappedMean(1000,500,0.8)
#' ParetoCappedMean(50,100,2)
ParetoCappedMean<-function(cap,scale,shape){
  # recycle every argument to a common length
  df<-data.frame(cap,scale,shape)
  cap<-df$cap; scale<-df$scale; shape<-df$shape
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
  ifelse(cap<=scale
         ,cap
         ,scale*(1+tailTerm)
  )
}



#' Exposure Curve from a Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the exposure curve will be evaluated.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a Pareto distribution with parameters \code{scale} and \code{shape}. The exposure curve divides by the mean, which is infinite when \code{shape <= 1}; the function returns 0 in that case.
#' @export
#' @examples
#' ExposureCurvePareto(700,500,1.2)
#' ExposureCurvePareto(20000,200,1.1)
ExposureCurvePareto<-function(x,scale,shape){
  df<-data.frame(x,scale,shape)
  ifelse(df$shape>1
         ,ParetoCappedMean(x,scale,shape)*(shape-1)/shape/scale
         ,0
  )
}



#' Increased Limit Factor Curve from a Pareto severity distribution
#'
#' @param xLow A positive real number -  the claim amount where the Increased Limit Factor Curve will be evaluated from.
#' @param xHigh A positive real number -  the claim amount where the Increased Limit Factor Curve will be evaluated to.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with Claim Severity from a Pareto distribution with parameters \code{scale} and \code{shape}.
#' @export
#' @examples
#' ILFPareto(700,1200,500,1.2)
#' ILFPareto(1200,20000,200,1.1)
ILFPareto<-function(xLow,xHigh,scale,shape){
  ParetoCappedMean(xHigh,scale,shape)/ParetoCappedMean(xLow,scale,shape)
}
