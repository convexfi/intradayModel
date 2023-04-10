#' Title
#'
#' @param data.pre
#' @param model
#' @param out.sample  the number of days before the last to keep for out of sample forecasting
#'
#' @return
#' @export
#'
#' @examples
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
  y.pred <- marss_predict(data, uniModel)
  y.pred.out.sample <- tail(y.pred, nrow(data) * out.sample)

  return(y.pred.out.sample)
}

marss_predict <- function(data, uniModel) {
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
  y.pred <- x_pred[1, ] + x_pred[2, ] + rep(seasonal, ncol(data))

  return(y.pred)
}
