#' @title Decompose the intraday trading volume signal
#'
#' @description A model with all parameters fixed can be used to decompose the intraday trading signal into daily, seasonal, 
#'              and intraday dynamic components. The daily component and intraday dynamic component at time \eqn{t} are the 
#'              smoothed state estimate conditioned on all the data. They are mathematically denoted by 
#'              \eqn{\mathbb{E}[\mathbf{x}_{\tau}|\{y_{\tau}\}_{\tau=1}^{N}]}, where \eqn{N} is the total number of bins in the train set. 
#'              The seasonal component has the value of \eqn{\boldsymbol{\phi}}.
#'              
#'              This function will produce the three components along with a plot.
#'
#' @param data n_bin * n_day trading volume data matrix with no NA.
#' @param uniModel uniModel object from function with all parameters fixed.
#'
#' @return A list containing the following elements:
#'        \item{\code{components}}{A list containing three components:
#'              \itemize{ \item{\code{daily}: Daily average part;}
#'                        \item{\code{dynamic}: Intraday dynamic part;}
#'                        \item{\code{seasonal}}: Intraday periodic component (the seasonality).}}
#'        \item{\code{plot}}{The plot of original signal and three components.}
#'        
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A kalman filter approach. Available at SSRN 3101695.
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
