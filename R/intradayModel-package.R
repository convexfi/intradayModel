#' intradayModel: Univariate State-Space Modeling for Financial Intraday Signal
#'
#' Univariate state-space models fit to financial intraday signal, with a focus on intraday trading volume.  
#' Fitting is primarily via the Maximum-likelihood estimation and Expectation-Maximization (EM) algorithm. 
#' In addition, functions for prediction and filtering are provided.
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