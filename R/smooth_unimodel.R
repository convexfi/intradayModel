#' @title Smoothing Financial Intraday Signal via a Univariate State-Space Model 
#'
#' @description A model with all parameters fixed can be used to decompose the financial intraday signal into daily, seasonal, 
#'              and intraday dynamic components. The daily component and intraday dynamic component at time \eqn{\tau} are the smoothed state estimate 
#'              conditioned on all the data, and denoted by \eqn{E[\mathbf{x}_{\tau}|\{y_{\tau}\}_{\tau=1}^{N}]}{E[ x(\tau) | y(\tau), \tau = 1, ... , N ]}, 
#'              where \eqn{N} is the total number of bins in the dataset. The seasonal component has the value of 
#'              \eqn{\boldsymbol{\phi}}{\phi}.
#'
#' @param data A n_bin * n_day matrix or an xts object storing intraday signal.
#' @param unimodel A "\code{unimodel}" object from function \code{fit_unimodel}.
#'
#' @return A list containing the following elements:
#'        \item{\code{original_signal}}{A vector of original intraday signal;}
#'        \item{\code{smooth_signal}}{A vector of smoothed intraday signal;}
#'        \item{\code{components}}{A list of the three smoothed components:
#'              \itemize{ \item{\code{smooth_daily}}
#'                        \item{\code{smooth_seasonal}}
#'                        \item{\code{smooth_dynamic}}}}        
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A kalman filter approach. Available at SSRN 3101695.
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' data(aapl_volume)
#' unimodel_fit <- fit_unimodel(aapl_volume)
#' smooth_result <- smooth_unimodel(aapl_volume, unimodel_fit)
#' }
#' 
#' @export
smooth_unimodel <- function(data, unimodel) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  } 
  data <- clean_data(data)
  
  is_unimodel(unimodel, nrow(data))

  # if model isn't optimally fitted (no convergence), it cannot filter
  if (Reduce("+", unimodel$fit_request) != 0) {
    msg <- c("All parameters must be optimally fitted. ",
             "Parameters ", paste(names(unimodel$fit_request[unimodel$fit_request == TRUE]), collapse = ", "), " are not optimally fitted.")
    stop(msg)
  }

  # filter using UNISS (our own Kalman)
  args <- list(
    data = log(data),
    unimodel = unimodel
  )
  uniss_obj <- do.call(specify_uniss, args)
  Kf <- uniss_kalman(uniss_obj, "smoother")
  
  # tidy up components (scale change)
  components <- list(
    smooth_daily = exp(Kf$xtT[1,]),
    smooth_dynamic = exp(Kf$xtT[2,]),
    smooth_seasonal = exp(rep(uniss_obj$par$phi, uniss_obj$n_day))
  )
  smooth_signal <- components$smooth_daily * 
    components$smooth_dynamic * components$smooth_seasonal
  
  res <- list(
    original_signal = as.vector(data),
    smooth_signal = smooth_signal,
    components = components
  )
  
  return(res)
}