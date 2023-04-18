#' intradayModel: Univariate State-Space Modeling for Intraday Trading Volume
#'
#' Maximum-likelihood parameter estimation for state-space models fit to time-series data.  
#' Fitting is primarily via the Expectation-Maximization (EM) algorithm. Functions for 
#' prediction and filtering are also provided. The package is based on the papers: Chen, Feng, and Palomar (2016).
#' 
#' @section Functions:
#' \code{\link{uniModelSpec}},
#' \code{\link{uniModelFit}}, 
#' \code{\link{uniModelPred}}, 
#' \code{\link{uniModelFilter}}
#'
#' @section Data:
#' \code{\link{AAPL_volume}}
#' \code{\link{GE_volume}}
#'
#' @section Help:
#' For a quick help see the README file:
#' \href{}{GitHub-README}.
#'
#'
#' @author Shengjie Xiu, Yifan Yu and Daniel P. Palomar
#'
#' @docType package
#' @name intradayModel-package
NULL