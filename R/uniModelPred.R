#' @title Predict one-step ahead trading volume via kalman filter
#'
#' @description This function will return the modeled value of \eqn{y_t} conditioned on the previous data 
#' (the data during the time \eqn{0} to \eqn{t-1}).
#'
#' @param data n_bin * n_day trading volume data matrix or data.frame with no NA.
#' @param uniModel uniModel object from function \code{uniModelSpec}.
#' @param out.sample  Number of days before the last for out of sample prediction.
#'
#' @return A numeric vector containing the prediction result.
#' @author Shengjie Xiu and Yifan Yu
#' @seealso \code{\link{uniModelSpec}}
#' @examples
#' library(intradayModel)
#' @export
uniModelPred <- function(data, uniModel, out.sample) {
  # error control
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data.frame.")
  if (anyNA(data)) stop("data must have no NA.")
  is_uniModel(uniModel, nrow(data))

  # check if fit is necessary
  if (Reduce("+", uniModel$fit_request) != 0) {
    stop("All parameters must be fixed.\n")
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
