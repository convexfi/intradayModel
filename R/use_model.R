#' @title Use a Fitted Model for Analysis and Forecast
#'
#' @description If \code{purpose = analysis} (aka Kalman smoothing), the optimal components of the intraday signal conditioned on all the data are estimated.
#'              If \code{purpose = forecast} (aka forecast in state-space model), the default one-bin-ahead forecast signal is provided, mathematically denoted by \eqn{\hat{y}_{\tau+1} = E[y_{\tau+1}|\{y_{j}\}_{j=1}^{\tau}]}{y*(\tau+1) = E[y(\tau + 1) | y(j), j = 1, ... , \tau]}.
#'              Three measures are used to evaluate the performance:
#'              \itemize{\item{Mean absolute error (MAE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M|\hat{y}_{\tau} - y_{\tau}|}{\sum (|y*(\tau) - y(\tau)|) / M} ;}
#'                       \item{Mean absolute percent error (MAPE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M\frac{|\hat{y}_{\tau} - y_{\tau}|}{y_{\tau}}}{\sum (|y*(\tau) - y(\tau)| / y(\tau)) / M} ;}
#'                       \item{Root mean square error (RMSE):
#'                             \eqn{\sqrt{\sum_{\tau=1}^M\frac{\left(\hat{y}_{\tau} - y_{\tau}\right)^2}{M}}}{[\sum ((y*(\tau) - y(\tau))^2 / M)]^0.5} ,}
#'              }
#'              where \eqn{M} is the number of bins.
#'
#'
#' @param purpose String \code{analysis/forecast}. Indicates the purpose of using the provided model.
#' @param model A model object from fitting functions including \code{fit_volume}.
#' @param data A n_bin * n_day matrix or an xts object storing intraday signal.
#' @param burn_in_days  Number of initial days in the burn-in period for \code{forecast}. Samples from the first burn_in_days are used to warm up the model and then are discarded.
#'
#'
#' @return If \code{purpose = analysis}: A list containing the following elements:
#'        \itemize{
#'        \item{\code{original_signal}: }{A vector of original intraday signal;}
#'        \item{\code{smooth_signal}: }{A vector of smoothed intraday signal;}
#'        \item{\code{components}: }{A list of the three smoothed components:
#'              \itemize{ \item{\code{smooth_daily}}
#'                        \item{\code{smooth_seasonal}}
#'                        \item{\code{smooth_dynamic}}
#'                        \item{\code{residual}}}}
#'        \item{\code{error}: }{A list of three error measures:
#'              \itemize{ \item{\code{mae}}
#'                        \item{\code{mape}}
#'                        \item{\code{rmse}}}}}   
#'        If \code{purpose = forecast}: A list containing the following elements:
#'        \itemize{
#'         \item{\code{original_signal}: }{A vector of original intraday signal;}
#'         \item{\code{forecast_signal}: }{A vector of forecast intraday signal;}
#'         \item{\code{components}: }{A list of the three forecast components:
#'              \itemize{ \item{\code{smooth_daily}}
#'                        \item{\code{smooth_seasonal}}
#'                        \item{\code{smooth_dynamic}}
#'                        \item{\code{residual}}}} 
#'         \item{\code{error}: }{A list of three error measures:
#'              \itemize{ \item{\code{mae}}
#'                        \item{\code{mape}}
#'                        \item{\code{rmse}}}}}
#'         
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A Kalman filter approach. Available at SSRN 3101695.
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' data(aapl_volume)
#' aapl_volume_training <- aapl_volume[, 1:104]
#' aapl_volume_testing <- aapl_volume[, 105:124]
#' model_fit <- fit_volume(aapl_volume_training)
#' 
#' # analyze training volume
#' analysis_result <- use_model(purpose = "analysis", model_fit, aapl_volume_training)
#' 
#' # forecast testing volume
#' forecast_result <- use_model(purpose = "forecast", model_fit, aapl_volume_testing)
#' 
#' # forecast testing volume with burn-in 
#' forecast_result <- use_model(purpose = "forecast", model_fit, aapl_volume,
#'                              burn_in_days = 104)
#' 
#' }
#' 
#' @export
use_model <- function(purpose, model, data, burn_in_days = 0) {
  if (tolower(purpose) == "analysis") {
    res <- smooth_volume_model(data = data, volume_model = model)
  } else if (tolower(purpose) == "forecast") {
    res <- forecast_volume_model(data = data, volume_model = model, burn_in_days = burn_in_days)
  } else {
    warning("Wrong purpose for use_model function.\n")
  }
  
  return(res)
}

smooth_volume_model <- function(data, volume_model) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  } 
  data <- clean_data(data)
  
  is_volume_model(volume_model, nrow(data))

  # if model isn't optimally fitted (no convergence), it cannot filter
  if (Reduce("+", volume_model$fit_request) != 0) {
    msg <- c("All parameters must be optimally fitted. ",
             "Parameters ", paste(names(volume_model$fit_request[volume_model$fit_request == TRUE]), collapse = ", "), " are not optimally fitted.")
    stop(msg)
  }

  # filter using UNISS (our own Kalman)
  args <- list(
    data = log(data),
    volume_model = volume_model
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
  original_signal <- as.vector(data)
  components$residual <- original_signal / smooth_signal
  error <- list(
    mae = calculate_mae(original_signal, smooth_signal),
    mape = calculate_mape(original_signal, smooth_signal),
    rmse = calculate_rmse(original_signal, smooth_signal)
  )
  
  res <- list(
    original_signal = original_signal,
    smooth_signal = smooth_signal,
    components = components,
    error = error
  )
  
  return(res)
}

forecast_volume_model <- function(data, volume_model, burn_in_days = 0) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  } 
  data <- clean_data(data)
  if (burn_in_days > ncol(data)) stop("out_sample must be smaller than the number of columns in data matrix.")
  
  is_volume_model(volume_model, nrow(data))
  
  # check if fit is necessary
  if (Reduce("+", volume_model$fit_request) != 0) {
    msg <- c("All parameters must be fitted.\n ",
             "Parameter ", paste(names(volume_model$fit_request[volume_model$fit_request == TRUE]), collapse = ", "), " is not fitted.")
    stop(msg)
  }
  
  # one-step ahead prediction using UNISS (our own Kalman)
  args <- list(
    data = log(data),
    volume_model = volume_model
  )
  uniss_obj <- do.call(specify_uniss, args)
  Kf <- uniss_kalman(uniss_obj, "filter")
  
  # tidy up components (scale change)
  components <- list(
    forecast_daily = exp(Kf$xtt1[1,]),
    forecast_dynamic = exp(Kf$xtt1[2,]),
    forecast_seasonal = exp(rep(uniss_obj$par$phi, uniss_obj$n_day))
  )
  components_out <- lapply(components, function (c) tail(c, nrow(data) * (ncol(data) - burn_in_days)))
  forecast_signal <- components_out$forecast_daily * 
    components_out$forecast_dynamic * components_out$forecast_seasonal
  
  # error measures
  original_signal <- tail(as.vector(as.matrix(data)), nrow(data) * (ncol(data) - burn_in_days))
  components_out$residual <- original_signal / forecast_signal
  error <- list(
    mae = calculate_mae(original_signal, forecast_signal),
    mape = calculate_mape(original_signal, forecast_signal),
    rmse = calculate_rmse(original_signal, forecast_signal)
  )
  
  # result
  res <- list(
    original_signal = original_signal,
    forecast_signal = forecast_signal,
    components = components_out,
    error = error
  )
  
  return(res)
}