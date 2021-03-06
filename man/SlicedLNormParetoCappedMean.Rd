% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SlicecdLogNormalPareto.R
\name{SlicedLNormParetoCappedMean}
\alias{SlicedLNormParetoCappedMean}
\title{Sliced LogNormal Pareto capped mean}
\usage{
SlicedLNormParetoCappedMean(cap, mu, sigma, SlicePoint, shape)
}
\arguments{
\item{cap}{A positive real number -  the claim severity cap.}

\item{mu}{A real number -  the first parameter of the attritional Claim Severity's LogNormal distribution.}

\item{sigma}{A positive real number -  the second parameter of the attritional Claim Severity's LogNormal distribution.}

\item{SlicePoint}{A positive real number - the slice point and the scale parameter of the tail Claim Severity's Pareto distribution.}

\item{shape}{A positive real number - the shape parameter of the tail Claim Severity's Pareto distribution.}
}
\value{
The mean of the claim severity capped at \code{cap} with an attritional claim LogNormal distribution with parameters \code{mu} and \code{sigma} and a large claim Pareto distribution with parameters \code{SlicePoint} and \code{shape}.
}
\description{
Sliced LogNormal Pareto capped mean
}
\examples{
SlicedLNormParetoCappedMean(1200,6,1.5,1000,1.2)
SlicedLNormParetoCappedMean(2500,6.5,1.4,2000,1.6)
SlicedLNormParetoCappedMean(4000,7,1.6,3000,1.4)
}
