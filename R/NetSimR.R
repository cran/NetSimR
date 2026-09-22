#' @description The NetSimR package provides five categories of functions:
#' \enumerate{
#' \item Capped means, Exposure and ILF curves from various severity distributions, and the special functions behind them
#' \item Pure IBNR and UPR earned periods
#' \item Sliced distributions
#' \item Frequency-severity claims simulation, and applying a reinsurance layer to simulated claims
#' \item Shiny apps for simulating claims, fitting distributions and fitting GLMs
#' }
#'
#' @section NetSimR special functions:
#' \code{\link{IGamma}}
#' \code{\link{erf}}
#'
#' @section NetSimR mean functions:
#' \code{\link{SlicedGammaParetoMean}}
#' \code{\link{SlicedLNormParetoMean}}
#'
#' @section NetSimR capped mean functions:
#' \code{\link{GammaCappedMean}}
#' \code{\link{LNormCappedMean}}
#' \code{\link{ParetoCappedMean}}
#' \code{\link{ParetoCappedMeanCalc}}
#' \code{\link{SlicedGammaParetoCappedMean}}
#' \code{\link{SlicedLNormParetoCappedMean}}
#'
#' @section NetSimR exposure curve functions:
#' \code{\link{ExposureCurveGamma}}
#' \code{\link{ExposureCurveLNorm}}
#' \code{\link{ExposureCurvePareto}}
#' \code{\link{ExposureCurveSlicedGammaPareto}}
#' \code{\link{ExposureCurveSlicedLNormPareto}}
#'
#' @section NetSimR ILF curve functions:
#' \code{\link{ILFGamma}}
#' \code{\link{ILFLNorm}}
#' \code{\link{ILFPareto}}
#' \code{\link{ILFSlicedGammaPareto}}
#' \code{\link{ILFSlicedLNormPareto}}
#'
#' @section NetSimR pure IBNR functions:
#' \code{\link{PureIBNRGamma}}
#' \code{\link{PureIBNRLNorm}}
#'
#' @section NetSimR Sliced distribution functions:
#' \code{\link{dSlicedGammaPareto}}
#' \code{\link{dSlicedLNormPareto}}
#' \code{\link{pSlicedGammaPareto}}
#' \code{\link{pSlicedLNormPareto}}
#' \code{\link{qSlicedGammaPareto}}
#' \code{\link{qSlicedLNormPareto}}
#'
#' @section NetSimR claims simulation functions:
#' \code{\link{simulate_claims}}
#' \code{\link{simulate_function}}
#' \code{\link{apply_deductible_limit}}
#'
#' @section NetSimR Shiny apps:
#' \code{\link{run_shiny_simulator}}
#' \code{\link{run_shiny_distribution_fitting_tool}}
#' \code{\link{run_shiny_glm_fitting_tool}}
#'
#' @keywords internal
"_PACKAGE"
