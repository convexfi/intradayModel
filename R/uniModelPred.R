#' @title Predict one-bin-ahead trading volume via kalman filter
#'
#' @description The one-bin-ahead prediction is mathematically denoted by \eqn{\hat{y}_{\tau+1} = \mathbb{E}[y_{\tau+1}|\{y_{j}\}_{j=1}^{\tau}]}.
#'              Given the dataset, you need to indicate how many days from the end to keep for out-of-sample forecast.
#'              Three performance measures are used to evaluate the forecast results:
#'              \itemize{\item{Mean absolute error (MAE):
#'                             \eqn{\frac{1}{M}\sum_{i=1}^M\lvert\hat{y}_{\tau} - y_{\tau}\rvert,}}
#'                       \item{Mean absolute percent error (MAPE):
#'                             \eqn{\frac{1}{M}\sum_{i=1}^M\frac{\lvert\hat{y}_{\tau} - y_{\tau}\rvert}{y_{\tau}},}}
#'                       \item{Root Mean Square Error (RMSE):
#'                             \eqn{\sqrt{\sum_{i=1}^M\frac{\left(\hat{y}_{\tau} - y_{\tau}\right)^2}{M}},}}
#'              }
#'              where \eqn{M} is the number of bins.
#'
#' @param data n_bin * n_day trading volume data matrix with no NA.
#' @param uniModel uniModel object with all parameters fixed.
#' @param out.sample  Number of days before the last for out of sample forecast.
#'
#' @return A list containing the following elements:
#'         \item{\code{signal_pred}}{One-bin-ahead trading volume forecast.}
#'         \item{\code{signal_real}}{Real values of trading volume.}
#'         \item{\code{measure}}{Prediction performance measured by mae, mape and rmse.}
#'         \item{\code{plot}}{Plot of prediction and real values.}
#' 
#' @seealso \code{\link{uniModelSpec}}
#' 
#' @examples
#' library(intradayModel)
#' # load the data
#' data("AAPL_volume")
#' 
#' # define the uniModel
#' modelSpec <- uniModelSpec(fit = TRUE)
#' 
#' # fit the model
#' data <- AAPL_volume
#' data_train <- AAPL_volume[, 1:104]
#' modelSpec_fitted <- uniModelFit(data_train, modelSpec, acceleration = TRUE)
#' 
#' # predict
#' predict_result <- uniModelPred(data, modelSpec_fitted, out.sample = 20)
#' 
#' # predict performance measures
#' predict_result$measure
#' 
#' # predict result plot
#' predict_result$plot
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
  y_pred <- tail(y_pred, nrow(data) * out.sample)
  
  return(y_pred)
}
