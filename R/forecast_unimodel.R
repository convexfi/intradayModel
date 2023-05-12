#' @title Forecast One-bin-ahead Financial Intraday Signal via a Univariate State-Space Model  
#'
#' @description The one-bin-ahead forecast is mathematically denoted by \eqn{\hat{y}_{\tau+1} = E[y_{\tau+1}|\{y_{j}\}_{j=1}^{\tau}]}{y*(\tau+1) = E[y(\tau + 1) | y(j), j = 1, ... , \tau]}.
#'              Given the dataset, you need to indicate how many days from the end of the dataset to keep for out-of-sample forecast.
#'              Three measures are used to evaluate the forecasting performance:
#'              \itemize{\item{Mean absolute error (MAE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M|\hat{y}_{\tau} - y_{\tau}|}{\sum (|y*(\tau) - y(\tau)|) / M} ;}
#'                       \item{Mean absolute percent error (MAPE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M\frac{|\hat{y}_{\tau} - y_{\tau}|}{y_{\tau}}}{\sum (|y*(\tau) - y(\tau)| / y(\tau)) / M} ;}
#'                       \item{Root mean square error (RMSE):
#'                             \eqn{\sqrt{\sum_{\tau=1}^M\frac{\left(\hat{y}_{\tau} - y_{\tau}\right)^2}{M}}}{[\sum ((y*(\tau) - y(\tau))^2 / M)]^0.5} ,}
#'              }
#'              where \eqn{M} is the total number of out-of-sample bins.
#'
#' @param data A n_bin * n_day matrix or an xts object storing intraday signal.
#' @param unimodel A "\code{unimodel}" object from function \code{fit_volume}.
#' @param out_sample  Number of days from the end of the dataset for out-of-sample forecast.
#'
#' @return A list containing the following elements:
#'         \item{\code{original_signal}}{A vector of original intraday signal;}
#'         \item{\code{forecast_signal}}{A vector of forecast intraday signal;}
#'         \item{\code{components}}{A list of the three forecast components:
#'              \itemize{ \item{\code{forecast_daily}}
#'                        \item{\code{forecast_seasonal}}
#'                        \item{\code{forecast_dynamic}}}}   
#'         \item{\code{error}}{A list of three error measures:
#'              \itemize{ \item{\code{mae}}
#'                        \item{\code{mape}}
#'                        \item{\code{rmse}}}}
#' 
#' @examples
#' \dontrun{
#' 
#' data(aapl_volume)
#' 
#' # Fit on the first 104 days
#' aapl_volume_training <- aapl_volume[, 1:104]
#' model_fit <- fit_volume(aapl_volume_training)
#' 
#' # forecast on last 20 days
#' forecast_result <- forecast_unimodel(aapl_volume, model_fit, out_sample = 20)
#' }
#' 
#' @importFrom utils tail
#' 
#' @export
forecast_unimodel <- function(data, unimodel, out_sample) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  } 
  data <- clean_data(data)
  if (out_sample > ncol(data)) stop("out_sample must be smaller than the number of columns in data matrix.")
  
  is_unimodel(unimodel, nrow(data))

  # check if fit is necessary
  if (Reduce("+", unimodel$fit_request) != 0) {
    msg <- c("All parameters must be fitted.\n ",
             "Parameter ", paste(names(unimodel$fit_request[unimodel$fit_request == TRUE]), collapse = ", "), " is not fitted.")
    stop(msg)
  }

  # one-step ahead prediction using UNISS (our own Kalman)
  args <- list(
    data = log(data),
    unimodel = unimodel
  )
  uniss_obj <- do.call(specify_uniss, args)
  Kf <- uniss_kalman(uniss_obj, "filter")
  
  # tidy up components (scale change)
  components <- list(
    forecast_daily = exp(Kf$xtt1[1,]),
    forecast_dynamic = exp(Kf$xtt1[2,]),
    forecast_seasonal = exp(rep(uniss_obj$par$phi, uniss_obj$n_day))
  )
  components_out <- lapply(components, function (c) tail(c, nrow(data) * out_sample))
  forecast_signal <- components_out$forecast_daily * 
    components_out$forecast_dynamic * components_out$forecast_seasonal

  # error measures
  signal_real <- tail(as.vector(as.matrix(data)), nrow(data) * out_sample)
  error <- list(
    mae = calculate_mae(signal_real, forecast_signal),
    mape = calculate_mape(signal_real, forecast_signal),
    rmse = calculate_rmse(signal_real, forecast_signal)
  )

  # result
  res <- list(
    original_signal = signal_real,
    forecast_signal = forecast_signal,
    components = components_out,
    error = error
  )
  
  return(res)
}