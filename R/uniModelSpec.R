#' @title Define a uniModel object
#' 
#' @description Define the state space model for intraday log trading volume. The model is formulated as:
#' \deqn{\mathbf{x}_{t+1} = \mathbf{A}_{t}\mathbf{x}_{t} + \mathbf{w}_{t},}
#' \deqn{y_{t} = \mathbf{C}\mathbf{x}_{t} + \phi_{t} + v_t,}
#' where:
#' * \eqn{\mathbf{x}_{t} = [\eta_{t}, \mu_{t}]^\top} is the hidden state vector containing the daily average part and the intraday dynamic part;
#' * \eqn{\mathbf{A}_{t} = \left[\begin{array}{l}a_t^{\eta}&0\\0&a^{\mu}\end{array} \right]} 
#' is the state transition matrix with \eqn{a_t^{\eta} = \begin{cases}a^{\eta}&t = kI, k = 1,2,\dots\\0&\text{otherwise}\end{cases}}, 
#' \eqn{I} is the number of intervals (referred to as bins) within a day;
#' * \eqn{\mathbf{C} = [1, 1]} is the observation matrix;
#' * \eqn{\mathbf{w}_{t} = [\epsilon_t^{\eta},\epsilon_t^{\mu}]^\top \sim \mathcal{N}(\mathbf{0}, \mathbf{Q}_{t})} 
#' represents the i.i.d. Gaussian noise in the state transition, with diagonal covariance matrix 
#' \eqn{\mathbf{Q}_{t} = \left[\begin{array}{l}(\sigma_t^{\eta})^2&0\\0&(\sigma_t^{\mu})\end{array} \right]} 
#' and \eqn{\sigma_t^{\eta} = \begin{cases}\sigma^{\eta}&t = kI, k = 1,2,\dots\\0&\text{otherwise}\end{cases}};
#' * \eqn{v_t \sim \mathcal{N}(0, r)} is the i.i.d. Gaussian noise in the observation;
#' * \eqn{\mathbf{x}_0} is the initial state, and it is assumed to follow \eqn{\mathcal{N}(\overline{\mathbf{x}}_0, \mathbf{V}_0)}.
#' In the proposed model, \eqn{\overline{\mathbf{x}}_0, \mathbf{V}_0, \mathbf{A}_{t},\mathbf{Q}_{t},r} 
#' and the seasonality \eqn{\phi = [\phi_1,\dots, \phi_I]^\top} are treated as the unknown parameters.
#' @md
#'
#' @param fit Logical value indicating whether the model need to be fitted (default is \code{FALSE}). 
#'            If \code{FLASE}, all unknown parameters should be assigned a fixed value in \code{fixed.pars}.
#' @param fixed.pars List of values of fixed parameters. The elements in the list specify the values for the unknown parameters. 
#'                   The list elements should be a subset of following ones: 
#'                  \itemize{\item{\code{"a_eta"}: \eqn{a^{\eta}}, contains a double;}
#'                           \item{\code{"a_mu"}: \eqn{a^{\mu}}, contains a double;}
#'                           \item{\code{"var_eta"}: \eqn{\sigma^{\eta}}, contains a double;}
#'                           \item{\code{"var_mu"}: \eqn{\sigma^{\mu}}, contains a double;}
#'                           \item{\code{"r"}: \eqn{r}, contains a double;}
#'                           \item{\code{"phi"}: \eqn{\phi = [\phi_1,\dots, \phi_I]^\top}, contains I doubles;}
#'                           \item{\code{"x0"}: \eqn{\overline{\mathbf{x}}_0}, contains two doubles;}
#'                           \item{\code{"V0"}: \eqn{\mathbf{V}_0}, contains three doubles, corresponding to the 
#'                                              \eqn{\mathbf{V}_0(1,1),\mathbf{V}_0(1,2),\mathbf{V}_0(2,2).}}
#'                   The parameters without a fixed value in \code{uniModelSpec} should be fitted with \code{uniModelFit}.
#' @param init.pars List of initial values of unfixed parameters. The elements are the same with \code{fixed.pars}. 
#'                  The unfixed parameters without specified initial values will be given default initial values in \code{uniModelFit}.
#'
#' @return A list containing the following elements:
#'         \item{\code{par}}{Values of fixed parameters.}
#'         \item{\code{init}}{Initial values of unfixed parameters.}
#'         \item{\code{fit_request}}{List of logical values indicating whether the parameters are fixed or not.}
#' 
#' @author Shengjie Xiu and Yifan Yu
#' 
#' @references
#' R. Chen, Y. Feng, and D. Palomar, “Forecasting intraday trading volume: a kalman filter approach,” Available at SSRN 3101695, 2016.
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
  fixed.pars <- cleanParsList(fixed.pars)
  init.pars <- cleanParsList(init.pars)
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
      stop("Wrong input: unfitted model contains unknown parameters \n")
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
