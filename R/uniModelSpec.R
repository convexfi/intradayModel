#' @title Define a Univariate State-Space Model
#' 
#' @description This function defines a univariate state-space model proposed by (Chen et al., 2016). The model has the formulation:
#'              \deqn{\mathbf{x}_{\tau+1} = \mathbf{A}_{\tau}\mathbf{x}_{\tau} + \mathbf{w}_{\tau},}{x(\tau+1) = A(\tau) x(\tau) + w(\tau),}
#'              \deqn{y_{\tau} = \mathbf{C}\mathbf{x}_{\tau} + \phi_{\tau} + v_\tau,}{y(\tau) = C x(\tau) + \phi(\tau) + v(\tau),}
#'              where
#'              \itemize{\item{\eqn{\mathbf{x}_{\tau} = [\eta_{\tau}, \mu_{\tau}]^\top}{x(\tau) = [\eta(\tau); \mu(\tau)]} is the hidden state vector containing the log daily component and the log intraday dynamic component;} 
#'                       \item{\eqn{\mathbf{A}_{\tau} = \left[\begin{array}{l}a_{\tau}^{\eta}&0\\0&a^{\mu}\end{array} \right]}{A(\tau) = [a.\eta(\tau), 0; 0, a.\mu]} 
#'                             is the state transition matrix with \eqn{a_{\tau}^{\eta} = \begin{cases}a^{\eta}&t = kI, k = 1,2,\dots\\0&\text{otherwise};\end{cases}}{a.\eta(\tau) = a.\eta, when \tau = kI, k = 1, 2, ... , and zero otherwise;}}
#'                       \item{\eqn{\mathbf{C} = [1, 1]}{C = [1, 1]} is the observation matrix;}
#'                       \item{\eqn{\phi_{\tau}}{\phi(\tau)} is the corresponding element from \eqn{\boldsymbol{\phi} = [\phi_1,\dots, \phi_I]^\top}{\phi = [\phi(1); ... ; \phi(I)]}, which is the log seasonal component;}
#'                       \item{\eqn{\mathbf{w}_{\tau} = [\epsilon_{\tau}^{\eta},\epsilon_{\tau}^{\mu}]^\top \sim \mathcal{N}(\mathbf{0}, \mathbf{Q}_{\tau})}{w(\tau) = [\epsilon.\eta(\tau); \epsilon.\mu(\tau)] ~ N(0, Q(\tau))} 
#'                             represents the i.i.d. Gaussian noise in the state transition, with a time-varying covariance matrix 
#'                             \eqn{\mathbf{Q}_{\tau} = \left[\begin{array}{l}(\sigma_{\tau}^{\eta})^2&0\\0&(\sigma_{\tau}^{\mu})\end{array} \right]}{Q(\tau) = [(\sigma.\eta(\tau))^2, 0; 0, (\sigma.\mu)^2]} 
#'                             and \eqn{\sigma_\tau^{\eta} = \begin{cases}\sigma^{\eta}&t = kI, k = 1,2,\dots\\0&\text{otherwise};\end{cases}}{\sigma.\eta(\tau) = \sigma.\eta, when \tau = kI, k = 1, 2, ... , and zero otherwise;}}
#'                        \item{\eqn{v_\tau \sim \mathcal{N}(0, r)}{v(\tau) ~ N(0, r)} is the i.i.d. Gaussian noise in the observation;}
#'                        \item{\eqn{\mathbf{x}_1}{x(1)} is the initial state at \eqn{\tau = 1}{\tau = 1}, and it follows \eqn{\mathcal{N}(\mathbf{x}_0, \mathbf{V}_0).}{N(x(0), V(0))}}.}
#'             In the proposed model, \eqn{\boldsymbol{\theta} = \left\{\mathbf{A}_{\tau},\mathbf{Q}_{\tau},r,\boldsymbol{\phi}, \mathbf{x}_0, \mathbf{V}_0\right\}}{\Theta = {a.\eta, a.\mu, (\sigma.\eta)^2, (\sigma.\mu)^2, r, \phi, x(0), V(0)}} 
#'             are treated as parameters.
#' 
#' @param fit Logical value indicating whether the model needs to be fitted (default is \code{FALSE}). 
#'            If \code{FLASE}, all parameters should be assigned values via \code{fixed.pars}.
#' @param fixed.pars List of parameters' fixed values. The allowed parameters are listed below,
#'                  \itemize{\item{\code{"a_eta"}: \eqn{a^{\eta}}{a.\eta}} of size 1 ;
#'                           \item{\code{"a_mu"}: \eqn{a^{\mu}}{a.\mu}} of size 1 ;
#'                           \item{\code{"var_eta"}: \eqn{\sigma^{\eta}}{(\sigma.\eta)^2}} of size 1 ;
#'                           \item{\code{"var_mu"}: \eqn{\sigma^{\mu}}{(\sigma.\mu)^2}} of size 1 ;
#'                           \item{\code{"r"}: \eqn{r}{r} of size 1 ;}
#'                           \item{\code{"phi"}: \eqn{\phi = [\phi_1,\dots, \phi_I]^\top}{\phi = [\phi(1); ... ; \phi(I)]} of size \eqn{I} ;}
#'                           \item{\code{"x0"}: \eqn{\mathbf{x}_0}{x(0)} of size 2 ;}
#'                           \item{\code{"V0"}: \eqn{\mathbf{V}_0}{V(0)} of size 2 * 2 .}}
#' @param init.pars List of unfitted parameters' initial values. The parameters are the same as \code{fixed.pars}. 
#'                  If the user does not assign initial values for the unfitted parameters, default ones will be used in \code{uniModelFit}.
#'
#' @return A univaraite model list object which contains the following elements:
#'         \item{\code{par}}{List of parameters' values.}
#'         \item{\code{init}}{List of unfitted parameters' initial values defined by users.}
#'         \item{\code{fit_request}}{List of logical values indicating whether each parameters requires fitting.}
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A kalman filter approach. Available at SSRN 3101695.
#' 
#' 
#' @seealso \code{\link{uniModelFit}}, \code{\link{uniModelFilter}}, \code{\link{uniModelPred}}
#'
#' @examples
#' # set fixed value
#' fixed.pars <- list()
#' fixed.pars$"var_eta" <- 4
#' fixed.pars$"x0" <- c(10, 0)
#' 
#' # set initial value 
#' init.pars <- list()
#' init.pars$"a_eta" <- 1
#'
#' # define the univariate model
#' model <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars, init.pars = init.pars)
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
  fixed_clean_result <- clean_pars_list(fixed.pars)
  fixed.pars <- fixed_clean_result$input_list
  
  unecessary_init <- intersect(names(init.pars), names(fixed.pars))
  init.pars <- init.pars[setdiff(names(init.pars), names(fixed.pars))]
  init_clean_result <- clean_pars_list(init.pars)
  init.pars <- init_clean_result$input_list
  
  # generate warning message
  if (length(fixed_clean_result$msg) > 0) {
    cat("Warnings in fixed.pars:\n")
    for (m in fixed_clean_result$msg) {
      cat("  ", m, "\n", sep = "")
    }
  }
  if (length(init_clean_result$msg) > 0 | length(unecessary_init) > 0) {
    cat("Warnings in init.pars:\n")
    if (!is.null(init_clean_result$msg)) {
      for (m in init_clean_result$msg) {
        cat("  ", m, "\n", sep = "")
      }
    }
    if (length(unecessary_init) > 0) {
      cat("  Elements ", paste(unecessary_init, collapse = ", "),
          " have already been fixed.\n", sep = "")
    }
  }

  # store inputs in univariate model object
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
