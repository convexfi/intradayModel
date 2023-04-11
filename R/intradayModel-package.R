#' intradayModel: State-Space Modeling of Intraday Trading Volume
#'
#' Maximum-likelihood parameter estimation for state-space models fit to time-series data.  
#' Fitting is primarily via the Expectation-Maximization (EM) algorithm. Functions for 
#' prediction and filtering are also provided. The package is based on the papers: Chen, Feng, and Palomar (2016).
#' 
#' @section Functions:
#' \code{\link{uniModelSpec}},
#' \code{\link{uniModelFit}}, 
#' \code{\link{uniModelPred}}, 
#' \code{\link{uniModelFilter}}, 
#' \code{\link{plot_decomposition}}
#'
#' @section Data:
#' \code{\link{data_log_volume}}
#'
#' @section Help:
#' For a quick help see the README file:
#' \href{https://github.com/dppalomar/portfolioBacktest/blob/master/README.md}{GitHub-README}.
#'
#' For more details see the vignette:
#' \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html}{CRAN-vignette}.
#'
#' @author Shengjie Xiu, Yifan Yu and Daniel P. Palomar
#'
#' @docType package
#' @name intradayModel-package
NULL