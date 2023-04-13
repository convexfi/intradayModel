#' @title Decompose the intraday trading volume signal
#'
#' @description Decompose the intraday trading volume into daily average, intraday dynamic and intraday periodic parts (i.e., the seasonality).
#'              Daily average and intraday dynamic are the smoothed state estimate at time \eqn{t} conditioned on all the data, 
#'              i.e., \eqn{\mathbf{x}_t^T}. The seasonality is the value of \eqn{\phi} fitted by the \code{uniModeFit}.
#'
#' @param data n_bin * n_day trading volume data matrix with no NA.
#' @param uniModel uniModel object from function \code{uniModelSpec} with all parameters fixed.
#'
#' @return A list containing the following elements:
#'        \item{\code{daily}}{Daily average part.}
#'        \item{\code{dynamic}}{Intraday dynamic part.}
#'        \item{\code{seasonal}}{Intraday periodic component (the seasonality).}
#'        
#' @author Shengjie Xiu and Yifan Yu
#' 
#' @references
#' R. Chen, Y. Feng, and D. Palomar, “Forecasting intraday trading volume: a kalman filter approach,” Available at SSRN 3101695, 2016.
#' 
#' @seealso \code{\link{uniModelSpec}}
#' 
#' @examples
#' library(intradayModel)
#' # load the data
#' data("AAPL_volume")
#' #' 
#' # define the uniModel
#' modelSpec <- uniModelSpec(fit = TRUE)
#' 
#' # fit the model
#' modelSpec.fit <- uniModelFit(data_log_volume, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
#' 
#' # filter
#' components <- uniModelFilter(data_log_volume, uniModel.fit)
#' 
#' @export
uniModelFilter <- function(data, uniModel) {
  # error control
  if (!is.matrix(data)) stop("data must be a matrix.")
  if (anyNA(data)) stop("data must have no NA.")
  is_uniModel(uniModel, nrow(data))

  # check if fit is necessary
  if (Reduce("+", uniModel$fit_request) != 0) {
    stop("All parameters must be fixed.\n")
  }

  # filter using MARSS
  components <- marss_filter(log(data), uniModel)
  
  # add decomposition plot
  plot <- plot_decomposition(data, components)
  res <- list(components = components,
              plot = plot)
  
  return(res)
}

marss_filter <- function(data, uniModel) {
  data <- as.matrix(data)
  args <- list(
    data = data,
    uniModel = uniModel
  )
  marss_obj <- do.call(specify_marss, args)
  Kf <- MARSS::MARSSkfas(marss_obj)
  result <- list(
    "daily" = Kf$xtT[1, ],
    "dynamic" = Kf$xtT[2, ],
    "seasonal" = as.vector(marss_obj$model$fixed[["A"]])
  )

  return(result)
}
