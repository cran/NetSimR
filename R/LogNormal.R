
####################
#LogNormal Functions
####################


#' Error function
#'
#' @param x A real number.
#' @return The value of the error function at \code{x}. A non-numeric \code{x} is an error.
#' @seealso \code{\link{LNormCappedMean}}
#' @export
#' @examples
#' erf(0.1)
#' erf(0.5)
erf<-function(x){
  check_numeric(x = x)
  # erf(x) = P(Z^2 <= 2 x^2) for a standard normal Z, where Z^2 is chi-squared with one
  # degree of freedom; unlike 2 * pnorm(sqrt(2) * x) - 1 this keeps the precision near 0
  sign(x)*pchisq(2*x*x,1)
}



#' Lognormal capped mean
#'
#' @param cap A non-negative real number -  the claim severity cap.
#' @param mu A real number - the first parameter of the Claim Severity's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the Claim Severity's LogNormal distribution.
#' @return The mean of the claim severity capped at \code{cap} with a LogNormal distribution with parameters \code{mu} and \code{sigma}. The arguments are recycled to a common length. A non-numeric argument, a negative \code{cap} or a non-positive \code{sigma} is an error; \code{NA} values give \code{NA}.
#' @family capped mean functions
#' @export
#' @examples
#' LNormCappedMean(2000,6,1.5)
#' LNormCappedMean(1000,5,1.6)
LNormCappedMean<- function(cap,mu,sigma){
  check_positive(cap = cap, allow_zero = TRUE)
  check_numeric(mu = mu)
  check_positive(sigma = sigma)
  # recycle every argument to a common length
  args<-recycle_arguments(cap = cap, mu = mu, sigma = sigma)
  cap<-args$cap; mu<-args$mu; sigma<-args$sigma
  # E[min(X, cap)] = exp(mu + sigma^2 / 2) * Phi((log(cap) - mu - sigma^2) / sigma)
  #                  + cap * (1 - Phi((log(cap) - mu) / sigma)).
  # Written with pnorm() directly (rather than erf() and cap - cap cancellations)
  # so that it stays accurate for caps far above the mean.
  logCap <- log(cap)
  capTerm <- cap * pnorm(logCap, mean = mu, sd = sigma, lower.tail = FALSE)
  # an infinite cap contributes nothing (Inf * 0 would be NaN)
  capTerm[which(cap == Inf)] <- 0
  # the first term is combined on the log scale, so that exp(mu + sigma^2 / 2) does not
  # overflow for large sigmas when the capped mean itself is finite
  restore_shape(exp(mu + 0.5 * sigma * sigma + pnorm(logCap, mean = mu + sigma * sigma, sd = sigma, log.p = TRUE)) + capTerm, args)
}



#' Exposure Curve from LogNormal a severity distribution
#'
#' @param x A non-negative real number -  the claim amount where the exposure curve will be evaluated.
#' @param mu A real number - the first parameter of the Claim Severity's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the Claim Severity's LogNormal distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a LogNormal distribution with parameters \code{mu} and \code{sigma}.
#' @family exposure curve functions
#' @export
#' @examples
#' ExposureCurveLNorm(2000,6,1.5)
#' ExposureCurveLNorm(1000,5,1.6)
ExposureCurveLNorm<-function(x,mu,sigma){
  check_positive(x = x, allow_zero = TRUE)
  check_numeric(mu = mu)
  check_positive(sigma = sigma)
  LNormCappedMean(x,mu,sigma)/(exp(mu+0.5*sigma*sigma))
}



#' Increased Limit Factor Curve from a LogNormal severity distribution
#'
#' @param xLow A non-negative real number -  the claim amount where the Increased Limit Factor Curve will be evaluated from.
#' @param xHigh A non-negative real number -  the claim amount where the Increased Limit Factor Curve will be evaluated to.
#' @param mu A real number - the first parameter of the Claim Severity's LogNormal distribution.
#' @param sigma A positive real number - the second parameter of the Claim Severity's LogNormal distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with Claim Severity from a LogNormal distribution with parameters \code{mu} and \code{sigma}.
#' @family ILF functions
#' @export
#' @examples
#' ILFLNorm(1000,2000,6,1.5)
#' ILFLNorm(1000,1500,5,1.6)
ILFLNorm<-function(xLow,xHigh,mu,sigma){
  check_positive(xLow = xLow, xHigh = xHigh, allow_zero = TRUE)
  check_numeric(mu = mu)
  check_positive(sigma = sigma)
  # recycle every argument to a common length, so that both capped means have it
  args<-recycle_arguments(xLow = xLow, xHigh = xHigh, mu = mu, sigma = sigma)
  xLow<-args$xLow; xHigh<-args$xHigh; mu<-args$mu; sigma<-args$sigma
  restore_shape(LNormCappedMean(xHigh,mu,sigma)/LNormCappedMean(xLow,mu,sigma), args)
}



#' Pure IBNR exposure from a LogNormal reporting delay distribution
#'
#' Durations are counted in days on each date's own calendar and clock: a \code{POSIXct} time of day counts as a fraction of a day, and daylight saving changes do not add fractions of a day, so \code{Date} and \code{POSIXct} dates (or a mix of them) give the same results.
#'
#' @param IncDate A \code{Date} or \code{POSIXct} vector -  the inception dates of the periods. Numbers and character strings are not accepted; convert them with \code{as.Date()} first.
#' @param ExpDate A \code{Date} or \code{POSIXct} vector -  the expiry dates of the periods. Must not be before the inception dates.
#' @param ValDate A \code{Date} or \code{POSIXct} date -  the valuation date.
#' @param mu A real number - the first parameter of the reporting delay's LogNormal distribution, with the delay measured in days.
#' @param sigma A positive real number - the second parameter of the reporting delay's LogNormal distribution.
#' @return A data frame with the unearned and pure IBNR exposure of each period in days (\code{UnearnedDuration}, and \code{PureIBNRDuration} rounded to 2 decimals) and as proportions between 0 and 1 of the period's duration (\code{UnearnedDurationRatio} and \code{PureIBNRDurationRatio}, rounded to 5 decimals), where the reporting delay has a LogNormal distribution with parameters \code{mu} and \code{sigma}. The dates and parameters are recycled to a common length, one row each; lengths that do not recycle are an error.
#' @family pure IBNR functions
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
#' PureIBNRLNorm(Dates$inceptionDate,Dates$expiryDate,ValuationDate,4,1.5)
PureIBNRLNorm <- function(IncDate, ExpDate, ValDate, mu, sigma){
  check_numeric(mu = mu)
  check_positive(sigma = sigma)
  pure_ibnr_exposure(IncDate, ExpDate, ValDate, list(mu = mu, sigma = sigma),
                     function(delay, p) LNormCappedMean(delay,p$mu,p$sigma))
}
