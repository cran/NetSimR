#################
#Pareto Functions
#################

#' Pareto capped mean intermediary calculation
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return An interim calculation for the mean of the claim severity capped at \code{cap} with a Pareto distribution with parameters \code{scale} and \code{shape}.
#' @examples
#' ParetoCappedMeanCalc(800,100,1.1)
#' ParetoCappedMeanCalc(1000,500,0.9)
ParetoCappedMeanCalc<-function(cap,scale,shape){
  (shape * scale - cap * (scale/cap)^shape)/(shape-1)
}



#' Pareto capped mean
#'
#' @param cap A positive real number -  the claim severity cap.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The mean of the claim severity capped at \code{cap} with a Pareto distribution with parameters \code{scale} and \code{shape}.
#' @examples
#' ParetoCappedMean(600,200,1.2)
#' ParetoCappedMean(800,100,1)
#' ParetoCappedMean(1000,500,0.8)
ParetoCappedMean<-function(cap,scale,shape){
  df<-data.frame(cap,scale,shape)
  ifelse(df$shape==1
         ,(ParetoCappedMeanCalc(cap,scale,shape+0.0001)+ParetoCappedMeanCalc(cap,scale,shape-0.0001))/2
         ,ParetoCappedMeanCalc(cap,scale,shape)
  )
}



#' Exposure Curve from a Pareto severity distribution
#'
#' @param x A positive real number -  the claim amount where the exposure curve will be evaluated.
#' @param scale A positive real number - the scale parameter of the Claim Severity's Pareto distribution.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Pareto distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a Pareto distribution with parameters \code{scale} and \code{shape}.
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
#' @examples
#' ILFPareto(700,1200,500,1.2)
#' ILFPareto(1200,20000,200,1.1)
ILFPareto<-function(xLow,xHigh,scale,shape){
  ParetoCappedMean(xHigh,scale,shape)/ParetoCappedMean(xLow,scale,shape)
}
