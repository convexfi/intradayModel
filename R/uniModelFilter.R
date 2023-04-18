#' @title Decompose Financial Intraday Signal via a Univariate State-Space Model 
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
#' @seealso \code{\link{uniModelSpec}}
#' 
#' @examples
#' # filter AAPL_volume
#' data(AAPL_volume)
#' model <- uniModelSpec(fit = TRUE)
#' model_fitted <- uniModelFit(AAPL_volume, model, acceleration = TRUE, 
#'                   maxit = 1000, abstol = 1e-4, log.switch = TRUE)
#' filter_result <- uniModelFilter(AAPL_volume, model_fitted)
#' 
#' @export
uniModelFilter <- function(data, uniModel) {
  # error control
  if (!is.matrix(data)) stop("data must be a matrix.")
  if (anyNA(data)) stop("data must have no NA.")
  is_uniModel(uniModel, nrow(data))

  # if model isn't optimally fitted (no convergence), it cannot filter
  if (Reduce("+", uniModel$fit_request) != 0) {
    msg <- c("All parameters must be optimally fitted. ",
             "Parameters ", paste(names(uniModel$fit_request[uniModel$fit_request == TRUE]), collapse = ", "), " are not optimally fitted.")
    stop(msg)
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
