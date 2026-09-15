################
#Input checks
################

# Stops with a clear error when an argument is not numeric. Arguments that are all NA (of
# any type) pass, so that they give NA results like the base R distribution functions. The
# error names the calling function.
check_numeric <- function(...) {
  args <- list(...)
  for (name in names(args)) {
    value <- args[[name]]
    if (!is.numeric(value) && !all(is.na(value))) {
      stop(simpleError(paste(name, "must be numeric"), sys.call(-1)))
    }
  }
  invisible(NULL)
}

# Stops with a clear error when an argument is not numeric, or has a value that is not NA
# and not positive (negative, with allow_zero = TRUE). NA values pass, so that they give NA
# results like the base R distribution functions. The error names the calling function.
check_positive <- function(..., allow_zero = FALSE) {
  args <- list(...)
  for (name in names(args)) {
    value <- args[[name]]
    # without this, text reaches arithmetic ("non-numeric argument to binary operator") or
    # is compared as text
    if (!is.numeric(value) && !all(is.na(value))) {
      stop(simpleError(paste(name, "must be numeric"), sys.call(-1)))
    }
    invalid <- if (allow_zero) value < 0 else value <= 0
    if (any(invalid, na.rm = TRUE)) {
      stop(simpleError(paste(name, if (allow_zero) "must be non-negative" else "must be positive"), sys.call(-1)))
    }
  }
  invisible(NULL)
}

# Recycles every argument to a common length, like the base R distribution functions: the
# longest length, or 0 when any argument has length 0. Lengths that do not recycle are an
# error, which names the calling function (or .call, when given). The first argument of the
# common length is kept, so that restore_shape() can give the result its names or dimensions.
recycle_arguments <- function(..., .call = sys.call(-1)) {
  args <- list(...)
  argLengths <- lengths(args)
  n <- if (any(argLengths == 0)) 0L else max(argLengths)
  if (n > 0 && any(n %% argLengths != 0)) {
    stop(simpleError("the arguments' lengths are not multiples of each other, so they cannot be recycled", .call))
  }
  # the input checks let non-numeric arguments through only when they are all NA (e.g.
  # NA_character_); as numbers they give NA rather than an error in the arithmetic
  recycled <- lapply(args, function(arg) rep_len(if (is.numeric(arg)) as.vector(arg) else as.numeric(as.vector(arg)), n))
  attr(recycled, "template") <- args[[which(argLengths == n)[1]]]
  recycled
}

# Returns a numeric result (numeric(0) rather than ifelse()'s logical(0) for zero-length
# arguments) with the names or dimensions of the argument kept by recycle_arguments().
restore_shape <- function(result, args) {
  template <- attr(args, "template")
  result <- as.numeric(result)
  if (is.null(dim(template))) {
    names(result) <- names(template)
  } else {
    dim(result) <- dim(template)
    dimnames(result) <- dimnames(template)
  }
  result
}



################
#Gamma Functions
################

#' Upper incomplete gamma function
#'
#' @param a A positive real number - the shape parameter.
#' @param x A non-negative real number.
#' @return The value of the upper incomplete gamma function at \code{x} with shape parameter \code{a}, i.e. \code{gamma(a) * pgamma(x, a, lower.tail = FALSE)}. The arguments are recycled to a common length. A non-numeric or non-positive \code{a} or a negative \code{x} is an error.
#' @seealso \code{\link{GammaCappedMean}}
#' @export
#' @examples
#' IGamma(1,1)
#' IGamma(0.1,2)
IGamma<-function(a,x){
  check_positive(a = a)
  check_positive(x = x, allow_zero = TRUE)
  args<-recycle_arguments(a = a, x = x)
  a<-args$a; x<-args$x
  # combined on the log scale, so that gamma(a) does not overflow (a > 171.6) when the product is finite
  restore_shape(exp(lgamma(a) + pgamma(x, shape = a, scale = 1, lower.tail = FALSE, log.p = TRUE)), args)
}



#' Gamma capped mean
#'
#' @param cap A non-negative real number -  the claim severity cap.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the Claim Severity's Gamma distribution.
#' @return The mean of the claim severity capped at \code{cap} with a Gamma distribution with parameters \code{shape} and \code{rate}. The arguments are recycled to a common length. A non-numeric argument, a negative \code{cap} or a non-positive \code{shape} or \code{rate} is an error; \code{NA} values give \code{NA}.
#' @family capped mean functions
#' @export
#' @examples
#' GammaCappedMean(700,1,0.0005)
#' GammaCappedMean(1000,1.5,0.0006)
GammaCappedMean<- function(cap,shape,rate){
  check_positive(cap = cap, allow_zero = TRUE)
  check_positive(shape = shape, rate = rate)
  # recycle every argument to a common length
  args<-recycle_arguments(cap = cap, shape = shape, rate = rate)
  cap<-args$cap; shape<-args$shape; rate<-args$rate
  # E[min(X, cap)] = (shape / rate) * P(shape + 1, rate * cap) + cap * Q(shape, rate * cap),
  # where P and Q are the regularised lower and upper incomplete gamma functions.
  # Written with pgamma() directly (rather than gamma() and IGamma()) so that it
  # does not overflow for large shapes and does not lose precision for small caps.
  rateCap <- rate * cap
  capTerm <- cap * pgamma(rateCap, shape = shape, lower.tail = FALSE)
  # an infinite cap contributes nothing (Inf * 0 would be NaN)
  capTerm[which(cap == Inf)] <- 0
  restore_shape(shape / rate * pgamma(rateCap, shape = shape + 1) + capTerm, args)
}



#' Exposure Curve from a Gamma severity distribution
#'
#' @param x A non-negative real number -  the claim amount where the exposure curve will be evaluated.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the Claim Severity's Gamma distribution.
#' @return The value of the Exposure curve at \code{x} with Claim Severity from a Gamma distribution with parameters \code{shape} and \code{rate}.
#' @family exposure curve functions
#' @export
#' @examples
#' ExposureCurveGamma(700,1,0.0005)
#' ExposureCurveGamma(1000,1.5,0.0006)
ExposureCurveGamma<-function(x,shape,rate){
  check_positive(x = x, allow_zero = TRUE)
  check_positive(shape = shape, rate = rate)
  GammaCappedMean(x,shape,rate)*rate/shape
}



#' Increased Limit Factor Curve from a Gamma severity distribution
#'
#' @param xLow A non-negative real number -  the claim amount where the Increased Limit Factor Curve will be evaluated from.
#' @param xHigh A non-negative real number -  the claim amount where the Increased Limit Factor Curve will be evaluated to.
#' @param shape A positive real number - the shape parameter of the Claim Severity's Gamma distribution.
#' @param rate A positive real number - the rate parameter of the Claim Severity's Gamma distribution.
#' @return The value of the Increased Limit Factor curve from \code{xLow} to \code{xHigh} with Claim Severity from a Gamma distribution with parameters \code{shape} and \code{rate}.
#' @family ILF functions
#' @export
#' @examples
#' ILFGamma(700,1000,1,0.0005)
#' ILFGamma(1000,1200,1.5,0.0006)
ILFGamma<-function(xLow,xHigh,shape,rate){
  check_positive(xLow = xLow, xHigh = xHigh, allow_zero = TRUE)
  check_positive(shape = shape, rate = rate)
  # recycle every argument to a common length, so that both capped means have it
  args<-recycle_arguments(xLow = xLow, xHigh = xHigh, shape = shape, rate = rate)
  xLow<-args$xLow; xHigh<-args$xHigh; shape<-args$shape; rate<-args$rate
  restore_shape(GammaCappedMean(xHigh,shape,rate)/GammaCappedMean(xLow,shape,rate), args)
}



# Unearned and pure IBNR exposure of periods, given the capped mean of the reporting delay
# distribution (in days) as a function of the delay and the parameters; shared by
# PureIBNRGamma and PureIBNRLNorm. The dates and the parameters (a named list) are recycled
# to a common length, so each row has its own dates and parameters.
pure_ibnr_exposure <- function(IncDate, ExpDate, ValDate, params, delayCappedMean){
  call <- sys.call(-1)
  # days since 1970-01-01 counted on each date's own calendar and clock, so that Date and
  # POSIXct dates (or a mix of them) agree, and daylight saving changes do not add
  # fractions of a day. Plain numbers or strings are refused rather than guessed at.
  toDays <- function(date, name) {
    if (inherits(date, "Date")) return(as.numeric(date))
    if (inherits(date, "POSIXt")) {
      date <- as.POSIXlt(date)
      return(as.numeric(as.Date(date)) + (date$hour * 3600 + date$min * 60 + date$sec) / 86400)
    }
    stop(simpleError(paste(name, "must be a Date or POSIXct date"), call))
  }
  # recycled before any arithmetic, so that ifelse() below keeps every row
  args<-do.call(recycle_arguments, c(
    list(IncDays = toDays(IncDate, "IncDate"), ExpDays = toDays(ExpDate, "ExpDate"), ValDays = toDays(ValDate, "ValDate")),
    params, list(.call = call)
  ), quote = TRUE)
  IncDays<-args$IncDays; ExpDays<-args$ExpDays; ValDays<-args$ValDays
  params<-args[names(params)]
  if (any(ExpDays < IncDays, na.rm = TRUE)) stop(simpleError("ExpDate must not be before IncDate", call))
  MinRepDelay<-pmax(0, ValDays - ExpDays)
  MaxRepDelay<-pmax(0, ValDays - IncDays)
  Duration<-ExpDays - IncDays
  EarnedDuration<-MaxRepDelay-MinRepDelay
  UnearnedDuration<-Duration-EarnedDuration
  UnearnedDurationRatio<-ifelse(Duration==0,0,round(UnearnedDuration/Duration,5))
  PureIBNRDuration<-round(delayCappedMean(MaxRepDelay, params)-delayCappedMean(MinRepDelay, params),2)
  PureIBNRDurationRatio<-ifelse(Duration==0,0,round(PureIBNRDuration/Duration,5))
  data.frame(UnearnedDuration,PureIBNRDuration,UnearnedDurationRatio,PureIBNRDurationRatio)
}



#' Pure IBNR exposure from a Gamma reporting delay distribution
#'
#' Durations are counted in days on each date's own calendar and clock: a \code{POSIXct} time of day counts as a fraction of a day, and daylight saving changes do not add fractions of a day, so \code{Date} and \code{POSIXct} dates (or a mix of them) give the same results.
#'
#' @param IncDate A \code{Date} or \code{POSIXct} vector -  the inception dates of the periods. Numbers and character strings are not accepted; convert them with \code{as.Date()} first.
#' @param ExpDate A \code{Date} or \code{POSIXct} vector -  the expiry dates of the periods. Must not be before the inception dates.
#' @param ValDate A \code{Date} or \code{POSIXct} date -  the valuation date.
#' @param shape A positive real number - the shape parameter of the reporting delay's Gamma distribution, with the delay measured in days.
#' @param rate A positive real number - the rate parameter (per day) of the reporting delay's Gamma distribution.
#' @return A data frame with the unearned and pure IBNR exposure of each period in days (\code{UnearnedDuration}, and \code{PureIBNRDuration} rounded to 2 decimals) and as proportions between 0 and 1 of the period's duration (\code{UnearnedDurationRatio} and \code{PureIBNRDurationRatio}, rounded to 5 decimals), where the reporting delay has a Gamma distribution with parameters \code{shape} and \code{rate}. The dates and parameters are recycled to a common length, one row each; lengths that do not recycle are an error.
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
#' PureIBNRGamma(Dates$inceptionDate,Dates$expiryDate,ValuationDate,7,0.15)
##Pure IBNR Gamma
PureIBNRGamma <- function(IncDate, ExpDate, ValDate, shape, rate){
  check_positive(shape = shape, rate = rate)
  pure_ibnr_exposure(IncDate, ExpDate, ValDate, list(shape = shape, rate = rate),
                     function(delay, p) GammaCappedMean(delay,p$shape,p$rate))
}
