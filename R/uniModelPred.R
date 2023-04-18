#' @title Predict One-bin-ahead Financial Intraday Signal via a Univariate State-Space Model  
#'
#' @description The one-bin-ahead prediction is mathematically denoted by \eqn{\hat{y}_{\tau+1} = \mathbb{E}[y_{\tau+1}|\{y_{j}\}_{j=1}^{\tau}]}{y*(\tau+1) = E[y(\tau + 1) | y(j), j = 1, ... , \tau]}.
#'              Given the dataset, you need to indicate how many days from the end of the dataset to keep for out-of-sample prediction.
#'              Three measures are used to evaluate the prediction performance:
#'              \itemize{\item{Mean absolute error (MAE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M\lvert\hat{y}_{\tau} - y_{\tau}\rvert}{\sum (|y*(\tau) - y(\tau)|) / M} ;}
#'                       \item{Mean absolute percent error (MAPE):
#'                             \eqn{\frac{1}{M}\sum_{\tau=1}^M\frac{\lvert\hat{y}_{\tau} - y_{\tau}\rvert}{y_{\tau}},}{\sum (|y*(\tau) - y(\tau)| / y(\tau)) / M} ;}
#'                       \item{Root mean square error (RMSE):
#'                             \eqn{\sqrt{\sum_{\tau=1}^M\frac{\left(\hat{y}_{\tau} - y_{\tau}\right)^2}{M}}}{[\sum ((y*(\tau) - y(\tau))^2 / M)]^0.5} ,}
#'              }
#'              where \eqn{M} is the total number of out-of-sample bins.
#'
#' @param data Matrix of intraday signal of size n_bin * n_day without any missing values.
#' @param uniModel Univariate model list object with all parameters fixed.
#' @param out.sample  Number of days from the end of the dataset for out-of-sample prediction.
#'
#' @return A list containing the following elements:
#'         \item{\code{signal_pred}}{One-bin-ahead prediction of intraday signal.}
#'         \item{\code{signal_real}}{Real out-of-sample intraday signal.}
#'         \item{\code{measure}}{MAE, MAPE, RMSE of out-of-sample prediction performance.}
#'         \item{\code{plot}}{Plot of the prediction and real values.}
#' 
#' @seealso \code{\link{uniModelSpec}}
#' 
#' @examples
#' # One-bin-ahead prediction on the last 20 days of AAPL_volume
#' data("AAPL_volume")
#' data <- AAPL_volume
#' data_train <- AAPL_volume[, 1:104]
#' 
#' model <- uniModelSpec(fit = TRUE)
#' model_fitted <- uniModelFit(data_train, model, acceleration = TRUE)
#' predict_result <- uniModelPred(data, model_fitted, out.sample = 20)
#' 
#' @export
uniModelPred <- function(data, uniModel, out.sample) {
  # error control
  if (!is.matrix(data)) stop("data must be a matrix.")
  if (anyNA(data)) stop("data must have no NA.")
  if (out.sample > ncol(data)) stop("out.sample must be smaller than the number of columns in data matrix.")
  is_uniModel(uniModel, nrow(data))

  # check if fit is necessary
  if (Reduce("+", uniModel$fit_request) != 0) {
    msg <- c("All parameters must be fitted.\n ",
             "Parameter ", paste(names(uniModel$fit_request[uniModel$fit_request == TRUE]), collapse = ", "), " is not fitted.")
    stop(msg)
  }

  # one-step ahead prediction using MARSS
  signal_pred <- exp(marss_predict(log(data), uniModel, out.sample))
  
  # error measures
  signal_real <- tail(as.vector(as.matrix(data)), nrow(data) * out.sample)
  measure <- data.frame(mae = calculate_mae(signal_real, signal_pred),
                        mape = calculate_mape(signal_real, signal_pred),
                        rmse = calculate_rmse(signal_real, signal_pred))
  
  # plot
  plot <- plot_prediction(signal_real, signal_pred)

  res <- list(signal_pred = signal_pred,
              signal_real = signal_real,
              measure = measure,
              plot = plot)
  
  return(res)
}

marss_predict <- function(data, uniModel, out.sample) {
  data <- as.matrix(data)
  args <- list(
    data = data,
    uniModel = uniModel
  )

  marss_obj <- do.call(specify_marss, args = args)
  Kf <- MARSS::MARSSkfas(marss_obj)
  x_pred <- Kf[["xtt1"]]
  seasonal <- uniModel$par$phi[, 1]
  names(seasonal) <- NULL
  y_pred <- x_pred[1, ] + x_pred[2, ] + rep(seasonal, ncol(data))
  y_pred <- utils::tail(y_pred, nrow(data) * out.sample)
  
  return(y_pred)
}
