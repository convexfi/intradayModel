#' @title Forecast One-bin-ahead Financial Intraday Signal via a Univariate State-Space Model  
#'
#' @description The one-bin-ahead forecast is mathematically denoted by \eqn{\hat{y}_{\tau+1} = \mathbb{E}[y_{\tau+1}|\{y_{j}\}_{j=1}^{\tau}]}{y*(\tau+1) = E[y(\tau + 1) | y(j), j = 1, ... , \tau]}.
#'              Given the dataset, you need to indicate how many days from the end of the dataset to keep for out-of-sample forecast.
#'              Three measures are used to evaluate the forecasting performance:
#'              \itemize{\item{Mean absolute error (MAE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M\lvert\hat{y}_{\tau} - y_{\tau}\rvert}{\sum (|y*(\tau) - y(\tau)|) / M} ;}
#'                       \item{Mean absolute percent error (MAPE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M\frac{\lvert\hat{y}_{\tau} - y_{\tau}\rvert}{y_{\tau}},}{\sum (|y*(\tau) - y(\tau)| / y(\tau)) / M} ;}
#'                       \item{Root mean square error (RMSE):
#'                             \eqn{\sqrt{\sum_{\tau=1}^M\frac{\left(\hat{y}_{\tau} - y_{\tau}\right)^2}{M}}}{[\sum ((y*(\tau) - y(\tau))^2 / M)]^0.5} ,}
#'              }
#'              where \eqn{M} is the total number of out-of-sample bins.
#'
#' @param data A n_bin * n_day matrix or an xts object storing intraday signal.
#' @param uniModel A univariate model list object from function \code{uniModelFit}.
#' @param out.sample  Number of days from the end of the dataset for out-of-sample forecast.
#'
#' @return A list containing the following elements:
#'         \item{\code{real.signal}}{A vector of real intraday signal;}
#'         \item{\code{forecast.signal}}{A vector of forecast intraday signal;}
#'         \item{\code{components}}{A list of the three forecast components:
#'              \itemize{ \item{\code{forecast.daily}}
#'                        \item{\code{forecast.seasonal}}
#'                        \item{\code{forecast.dynamic}}}}   
#'         \item{\code{error}}{A list of three error measures:
#'              \itemize{ \item{\code{mae}}
#'                        \item{\code{mape}}
#'                        \item{\code{rmse}}}}
#' 
#' @examples
#' \dontrun{
#'  # One-bin-ahead prediction on the last 20 days of GE_volume
#' data(GE_volume)
#' data <- GE_volume
#' data_train <- GE_volume[, 1:104]
#' 
#' model_fitted <- uniModelFit(data_train, control = list(acceleration = TRUE))
#' predict_result <- uniModelForecast(data, model_fitted, out.sample = 20)
#' }
#' 
#' @importFrom utils tail
#' 
#' @export
uniModelForecast <- function(data, uniModel, out.sample) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  } 
  data <- clean_data(data)
  if (out.sample > ncol(data)) stop("out.sample must be smaller than the number of columns in data matrix.")
  
  is_uniModel(uniModel, nrow(data))

  # check if fit is necessary
  if (Reduce("+", uniModel$fit_request) != 0) {
    msg <- c("All parameters must be fitted.\n ",
             "Parameter ", paste(names(uniModel$fit_request[uniModel$fit_request == TRUE]), collapse = ", "), " is not fitted.")
    stop(msg)
  }

  # one-step ahead prediction using UNISS (our own Kalman)
  args <- list(
    data = log(data),
    uniModel = uniModel
  )
  uniss_obj <- do.call(specify_uniss, args)
  Kf <- uniss_kalman(uniss_obj, "filter")
  
  # tidy up components (scale change)
  components <- list(
    forecast.daily = exp(Kf$xtt1[1,]),
    forecast.dynamic = exp(Kf$xtt1[2,]),
    forecast.seasonal = exp(rep(uniss_obj$par$phi, uniss_obj$n_day))
  )
  components.out <- lapply(components, function (c) tail(c, nrow(data) * out.sample))
  forecast.signal <- components.out$forecast.daily * 
    components.out$forecast.dynamic * components.out$forecast.seasonal

  # error measures
  signal_real <- tail(as.vector(as.matrix(data)), nrow(data) * out.sample)
  error <- list(
    mae = calculate_mae(signal_real, forecast.signal),
    mape = calculate_mape(signal_real, forecast.signal),
    rmse = calculate_rmse(signal_real, forecast.signal)
  )

  # result
  res <- list(
    real.signal = signal_real,
    forecast.signal = forecast.signal,
    components = components.out,
    error = error
  )
  
  return(res)
}