#' @title Define a uniModel object
#' 
#' @description Define the state space model for intraday log trading volume. We denote the day with index \eqn{t\in\{1,\dots,T\}} 
#'              and divide each day into \eqn{I} intervals (referred to as bins) indexed by \eqn{i\in\{1,\dots,I\}}. 
#'              The intraday observations of market signal is labeled with the single subscript \eqn{\tau = I \times (t-1) + i} 
#'              to represent the index of time series. 
#'              
#'              The observed market signal can be written in the following multiplicative form:
#'              \deqn{\text{intraday signal} = \text{daily} \times \text{seasonal} \times \text{intraday dynamic} \times \text{noise}. }
#'              By taking the logarithm transform, we rewrite (1) as an addictive formula:
#'              \deqn{ y_{\tau} = \eta_{\tau} + \phi_i + \mu_{t,i} + v_{t,i},}
#'              where each log-component can be extracted by the state-space model.
#'              The model is formulated as:
#'              \deqn{\mathbf{x}_{\tau+1} = \mathbf{A}_{\tau}\mathbf{x}_{\tau} + \mathbf{w}_{\tau},}
#'              \deqn{y_{\tau} = \mathbf{C}\mathbf{x}_{\tau} + \phi_{\tau} + v_\tau,}
#'              where:
#'              \itemize{\item{\eqn{\mathbf{x}_{\tau} = [\eta_{\tau}, \mu_{\tau}]^\top} is the hidden state vector containing the daily average part and the intraday dynamic part;} 
#'                       \item{\eqn{\mathbf{A}_{\tau} = \left[\begin{array}{l}a_{\tau}^{\eta}&0\\0&a^{\mu}\end{array} \right]} 
#'                             is the state transition matrix with \eqn{a_{\tau}^{\eta} = \begin{cases}a^{\eta}&t = kI, k = 1,2,\dots\\0&\text{otherwise}\end{cases}};}
#'                       \item{\eqn{\mathbf{C} = [1, 1]} is the observation matrix;}
#'                       \item{\eqn{\boldsymbol{\phi} = [\phi_1,\dots, \phi_I]^\top} is the seasonal component;}
#'                       \item{\eqn{\mathbf{w}_{\tau} = [\epsilon_{\tau}^{\eta},\epsilon_{\tau}^{\mu}]^\top \sim \mathcal{N}(\mathbf{0}, \mathbf{Q}_{\tau})} 
#'                             represents the i.i.d. Gaussian noise in the state transition, with diagonal covariance matrix 
#'                             \eqn{\mathbf{Q}_{\tau} = \left[\begin{array}{l}(\sigma_{\tau}^{\eta})^2&0\\0&(\sigma_{\tau}^{\mu})\end{array} \right]} 
#'                             and \eqn{\sigma_\tau^{\eta} = \begin{cases}\sigma^{\eta}&t = kI, k = 1,2,\dots\\0&\text{otherwise}\end{cases}};}
#'                        \item{\eqn{v_\tau \sim \mathcal{N}(0, r)} is the i.i.d. Gaussian noise in the observation;}
#'                        \item{\eqn{\mathbf{x}_1} is the initial state, and it is assumed to follow \eqn{\mathcal{N}(\mathbf{x}_0, \mathbf{V}_0).}}}
#'             In the proposed model, \eqn{\boldsymbol{\theta} = \left\{\mathbf{x}_0, \mathbf{V}_0, \mathbf{A}_{\tau},\mathbf{Q}_{\tau},r,\boldsymbol{\phi}\right\}} 
#'             are treated as the unknown parameters.
#' 
#' @param fit Logical value indicating whether the model need to be fitted (default is \code{FALSE}). 
#'            If \code{FLASE}, all unknown parameters should be assigned a fixed value in \code{fixed.pars}.
#' @param fixed.pars List of values of fixed parameters. The elements in the list specify the values for the unknown parameters. 
#'                   The list elements should be a subset of following ones: 
#'                  \itemize{\item{\code{"a_eta"}: \eqn{a^{\eta}};}
#'                           \item{\code{"a_mu"}: \eqn{a^{\mu}};}
#'                           \item{\code{"var_eta"}: \eqn{\sigma^{\eta}};}
#'                           \item{\code{"var_mu"}: \eqn{\sigma^{\mu}};}
#'                           \item{\code{"r"}: \eqn{r};}
#'                           \item{\code{"phi"}: \eqn{\phi = [\phi_1,\dots, \phi_I]^\top};}
#'                           \item{\code{"x0"}: \eqn{\mathbf{x}_0};}
#'                           \item{\code{"V0"}: \eqn{\mathbf{V}_0}, contains three doubles, corresponding to the 
#'                                              \eqn{\mathbf{V}_0(1,1),\mathbf{V}_0(1,2),\mathbf{V}_0(2,2).}}}
#'                   The parameters without a fixed value in \code{uniModelSpec} should be fitted with \code{uniModelFit}.
#' @param init.pars List of initial values of unfixed parameters. The elements are the same with \code{fixed.pars}. 
#'                  The unfixed parameters without specified initial values will be given default initial values in \code{uniModelFit}.
#'
#' @return A list containing the following elements:
#'         \item{\code{par}}{Values of fixed parameters.}
#'         \item{\code{init}}{Initial values of unfixed parameters.}
#'         \item{\code{fit_request}}{List of logical values indicating whether the parameters are fixed or not.}
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A kalman filter approach. Available at SSRN 3101695.
#' 
#' Brownlees, C. T., Cipollini, F., and Gallo, G. M. (2011). Intra-daily volume modeling and prediction for algorithmic trading. 
#' Journal of Financial Econometrics, 9(3), 489–518.
#' 
#' @seealso \code{\link{uniModelFit}}, \code{\link{uniModelFilter}}, \code{\link{uniModelPred}}
#'
#' @examples
#' library(intradayModel)
#' # set initial value 
#' init.pars <- list()
#' init.pars$"a_eta" <- 1
#'
#' # set fixed value
#' fixed.pars <- list()
#' fixed.pars$"var_eta" <- 4
#' fixed.pars$"x0" <- matrix(c(10, 0), 2)
#'
#' # define the uniModel
#' modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars, init.pars = init.pars)
#' @export
uniModelSpec <- function(fit = FALSE, fixed.pars = NULL, init.pars = NULL) {
  uniModel <- list()

  # error control
  if (!is.null(init.pars) && !is.list(init.pars)) stop("init.pars must be a list.")
  if (!is.null(fixed.pars) && !is.list(fixed.pars)) stop("fixed.pars must be a list.")

  # uniModel class properties
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  uniModel$par$"a_eta" <- NA
  uniModel$par$"a_mu" <- NA
  uniModel$par$"var_eta" <- NA
  uniModel$par$"var_mu" <- NA
  uniModel$par$"r" <- NA
  uniModel$par$"phi" <- NA
  uniModel$par$"x0" <- matrix(NA, 2)
  uniModel$par$"V0" <- matrix(NA, 3)
  uniModel$init <- list()

  # read in input parameters
  fixed.pars <- clean_pars_list(fixed.pars)
  init.pars <- clean_pars_list(init.pars)
  for (name in all_pars_name) {
    if (name %in% names(fixed.pars)) {
      uniModel$par[[name]] <- fixed.pars[[name]]
    } else if (name %in% names(init.pars)) {
      uniModel$init[[name]] <- init.pars[[name]]
    }
  }

  # decide if each variable requires fitting
  if (fit == FALSE) {
    if (anyNA(unlist(uniModel$par))) {
      na_check <- lapply(uniModel$par, anyNA)
      na_par <- names(na_check[na_check == TRUE])
      msg <- c("If fit = FALSE, ", paste(na_par, collapse = ", "), " must have no NAs.")
      stop(msg)
      break
    }
  }
  uniModel$fit_request <- list()
  for (name in all_pars_name) {
    if (anyNA(uniModel$par[[name]])) {
      uniModel$fit_request[[name]] <- TRUE
    } else {
      uniModel$fit_request[[name]] <- FALSE
    }
  }

  # unify the uniModel parameters format
  uniModel <- format_unimodel(uniModel)

  return(uniModel)
}
