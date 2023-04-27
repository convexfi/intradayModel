#' @title Smoothing Financial Intraday Signal via a Univariate State-Space Model 
#'
#' @description A model with all parameters fixed can be used to decompose the financial intraday signal into daily, seasonal, 
#'              and intraday dynamic components. The daily component and intraday dynamic component at time \eqn{\tau} are the smoothed state estimate 
#'              conditioned on all the data, and denoted by \eqn{\mathbb{E}[\mathbf{x}_{\tau}|\{y_{\tau}\}_{\tau=1}^{N}]}{E[ x(\tau) | y(\tau), \tau = 1, ... , N ]}, 
#'              where \eqn{N} is the total number of bins in the dataset. The seasonal component has the value of 
#'              \eqn{\boldsymbol{\phi}}{\phi}.
#'              
#'              This function will produce the above three components with a plot.
#'
#' @param data Matrix of intraday signal of size n_bin * n_day without any missing values.
#' @param uniModel Univariate model list object with all parameters fixed.
#'
#' @return A list containing the following elements:
#'        \item{\code{components}}{A list of the three components:
#'              \itemize{ \item{\code{daily}: Daily component;}
#'                        \item{\code{seasonal}: Seasonal component;}
#'                        \item{\code{dynamic}}: Intraday dynamic component.}}
#'        \item{\code{plot}}{A plot of the original signal and the three components.}
#'        
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
  if (is.xts(data)) {
    data <- intraday_xts_to_matrix(data)
  }
  if (anyNA(data)) stop("data must have no NA.")
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