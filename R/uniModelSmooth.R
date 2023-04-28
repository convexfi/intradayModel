#' @title Smoothing Financial Intraday Signal via a Univariate State-Space Model 
#'
#' @description A model with all parameters fixed can be used to decompose the financial intraday signal into daily, seasonal, 
#'              and intraday dynamic components. The daily component and intraday dynamic component at time \eqn{\tau} are the smoothed state estimate 
#'              conditioned on all the data, and denoted by \eqn{\mathbb{E}[\mathbf{x}_{\tau}|\{y_{\tau}\}_{\tau=1}^{N}]}{E[ x(\tau) | y(\tau), \tau = 1, ... , N ]}, 
#'              where \eqn{N} is the total number of bins in the dataset. The seasonal component has the value of 
#'              \eqn{\boldsymbol{\phi}}{\phi}.
#'
#' @param data A n_bin * n_day matrix or an xts object storing intraday signal.
#' @param uniModel A univariate model list object from function \code{uniModelFit}.
#'
#' @return A list containing the following elements:
#'        \item{\code{real.signal}}{A vector of real intraday signal;}
#'        \item{\code{smooth.signal}}{A vector of smoothed intraday signal;}
#'        \item{\code{components}}{A list of the three smoothed components:
#'              \itemize{ \item{\code{smooth.daily}}
#'                        \item{\code{smooth.seasonal}}
#'                        \item{\code{smooth.dynamic}}}}        
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A kalman filter approach. Available at SSRN 3101695.
#' 
#' 
#' @examples
#' \dontrun{
#' data(GE_volume)
#' model_fitted <- uniModelFit(GE_volume, control = list(acceleration = TRUE))
#' smooth_result <- uniModelSmooth(GE_volume, model_fitted)
#' }
#' 
#' @export
uniModelSmooth <- function(data, uniModel) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  } 
  data <- clean_data(data)
  
  is_uniModel(uniModel, nrow(data))

  # if model isn't optimally fitted (no convergence), it cannot filter
  if (Reduce("+", uniModel$fit_request) != 0) {
    msg <- c("All parameters must be optimally fitted. ",
             "Parameters ", paste(names(uniModel$fit_request[uniModel$fit_request == TRUE]), collapse = ", "), " are not optimally fitted.")
    stop(msg)
  }

  # filter using UNISS (our own Kalman)
  args <- list(
    data = log(data),
    uniModel = uniModel
  )
  uniss_obj <- do.call(specify_uniss, args)
  Kf <- uniss_kalman(uniss_obj, "smoother")
  
  # tidy up components (scale change)
  components <- list(
    smooth.daily = exp(Kf$xtT[1,]),
    smooth.dynamic = exp(Kf$xtT[2,]),
    smooth.seasonal = exp(rep(uniss_obj$par$phi, uniss_obj$n_day))
  )
  smooth.signal <- components$smooth.daily * 
    components$smooth.dynamic * components$smooth.seasonal
  
  res <- list(
    real.signal = as.vector(data),
    smooth.signal = smooth.signal,
    components = components
  )
  
  return(res)
}