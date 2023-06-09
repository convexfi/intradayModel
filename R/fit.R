#' @title Fit a Univariate State-Space Model on Intraday Trading Volume
#'
#' @description The main function for defining and fitting a univaraite state-space model on intraday trading volume. The model is proposed in (Chen et al., 2016) as
#'  \deqn{\mathbf{x}_{\tau+1} = \mathbf{A}_{\tau}\mathbf{x}_{\tau} + \mathbf{w}_{\tau},}{x(\tau+1) = A(\tau) x(\tau) + w(\tau),}
#'              \deqn{y_{\tau} = \mathbf{C}\mathbf{x}_{\tau} + \phi_{\tau} + v_\tau,}{y(\tau) = C x(\tau) + \phi(\tau) + v(\tau),}
#'              where
#'              \itemize{\item{\eqn{\mathbf{x}_{\tau} = [\eta_{\tau}, \mu_{\tau}]^\top}{x(\tau) = [\eta(\tau); \mu(\tau)]} is the hidden state vector containing the log daily component and the log intraday dynamic component;}
#'                       \item{\eqn{\mathbf{A}_{\tau} = \left[\begin{array}{cc}a_{\tau}^{\eta}&0\\0&a^{\mu}\end{array} \right]}{A(\tau) = [a.\eta(\tau), 0; 0, a.\mu]}
#'                             is the state transition matrix with \eqn{a_{\tau}^{\eta} = \left\{\begin{array}{cl}a^{\eta} & t = kI, k = 1,2,\dots\\0 & \textrm{otherwise};\end{array}\right.}{a.\eta(\tau) = a.\eta, when \tau = kI, k = 1, 2, ... , and zero otherwise;}}
#'                       \item{\eqn{\mathbf{C} = [1, 1]}{C = [1, 1]} is the observation matrix;}
#'                       \item{\eqn{\phi_{\tau}}{\phi(\tau)} is the corresponding element from \eqn{\boldsymbol{\phi} = [\phi_1,\dots, \phi_I]^\top}{\phi = [\phi(1); ... ; \phi(I)]}, which is the log seasonal component;}
#'                       \item{\eqn{\mathbf{w}_{\tau} = [\epsilon_{\tau}^{\eta},\epsilon_{\tau}^{\mu}]^\top \sim \mathcal{N}(\mathbf{0}, \mathbf{Q}_{\tau})}{w(\tau) = [\epsilon.\eta(\tau); \epsilon.\mu(\tau)] ~ N(0, Q(\tau))}
#'                             represents the i.i.d. Gaussian noise in the state transition, with a time-varying covariance matrix
#'                             \eqn{\mathbf{Q}_{\tau} = \left[\begin{array}{cc}(\sigma_{\tau}^{\eta})^2&0\\ 0&(\sigma^{\mu})^2\end{array} \right]}{Q(\tau) = [(\sigma.\eta(\tau))^2, 0; 0, (\sigma.\mu)^2]}
#'                             and \eqn{\sigma_\tau^{\eta} = \left\{\begin{array}{cl} \sigma^{\eta} & t = kI, k = 1,2,\dots\\0 & \textrm{otherwise};\end{array}\right.}{\sigma.\eta(\tau) = \sigma.\eta, when \tau = kI, k = 1, 2, ... , and zero otherwise;}}
#'                        \item{\eqn{v_\tau \sim \mathcal{N}(0, r)}{v(\tau) ~ N(0, r)} is the i.i.d. Gaussian noise in the observation;}
#'                        \item{\eqn{\mathbf{x}_1}{x(1)} is the initial state at \eqn{\tau = 1}{\tau = 1}, and it follows \eqn{\mathcal{N}(\mathbf{x}_0, \mathbf{V}_0)}{N(x(0), V(0))}}.}
#'             In the model, \eqn{\boldsymbol{\Theta} = \left\{a^{\eta},a^{\mu},\sigma^{\eta},\sigma^{\mu},r,\boldsymbol{\phi}, \mathbf{x}_0, \mathbf{V}_0\right\}}{\Theta = {a.\eta, a.\mu, (\sigma.\eta)^2, (\sigma.\mu)^2, r, \phi, x(0), V(0)}}
#'             are treated as parameters.
#' The model is fitted by expectation-maximization (EM) algorithms. The implementation follows (Chen et al., 2016), and the accelerated scheme is provided in (Varadhan and Roland, 2008).
#' The algorithm terminates when \code{maxit} is reached or the condition \eqn{\|\Delta \boldsymbol{\Theta}_i\| \le \textrm{abstol}}{||\Delta \Theta(i)|| <= abstol} is satisfied.
#'
#' @author Shengjie Xiu, Yifan Yu and Daniel P. Palomar
#'
#' @param data An n_bin * n_day matrix or an \code{xts} object storing intraday trading volume.
#' @param fixed_pars A list of parameters' fixed values. The allowed parameters are listed below,
#'                  \itemize{\item{\code{"a_eta"}: \eqn{a^{\eta}}{a.\eta}} of size 1 ;
#'                           \item{\code{"a_mu"}: \eqn{a^{\mu}}{a.\mu}} of size 1 ;
#'                           \item{\code{"var_eta"}: \eqn{\sigma^{\eta}}{(\sigma.\eta)^2}} of size 1 ;
#'                           \item{\code{"var_mu"}: \eqn{\sigma^{\mu}}{(\sigma.\mu)^2}} of size 1 ;
#'                           \item{\code{"r"}: \eqn{r}{r} of size 1 ;}
#'                           \item{\code{"phi"}: \eqn{\boldsymbol{\phi} = [\phi_1,\dots, \phi_I]^\top}{\phi = [\phi(1); ... ; \phi(I)]} of size \eqn{I} ;}
#'                           \item{\code{"x0"}: \eqn{\mathbf{x}_0}{x(0)} of size 2 ;}
#'                           \item{\code{"V0"}: \eqn{\mathbf{V}_0}{V(0)} of size 2 * 2 .}}
#' @param init_pars A list of unfitted parameters' initial values. The parameters are the same as \code{fixed_pars}.
#'                  If the user does not assign initial values for the unfitted parameters, default ones will be used.
#' @param verbose An integer specifying the print level of information during the algorithm (default \code{1}). Possible numbers:
#'                \itemize{\item{\code{"0"}: no output;}
#'                    \item{\code{"1"}: show the iteration number and \eqn{\|\Delta \boldsymbol{\Theta}_i\|}{||\Delta \Theta(i)||};}
#'                    \item{\code{"2"}: 1 + show the obtained parameters.}}
#' @param control A list of control values of EM algorithm:
#'                 \itemize{\item{\code{acceleration}: TRUE/FALSE indicating whether to use the accelerated EM algorithm (default TRUE);}
#'                    \item{\code{maxit}: Maximum number of iterations (default \code{3000});}
#'                    \item{\code{abstol}: Absolute tolerance for parameters' change \eqn{\|\Delta \boldsymbol{\Theta}_i\|}{||\Delta \Theta(i)||} as the stopping criteria (default \code{1e-4})}
#'                    \item{\code{log_switch}: TRUE/FALSE indicating whether to record the history of convergence progress (defalut TRUE).}}
#'
#' @return A list of class "\code{volume_model}" with the following elements (if the algorithm converges):
#'         \itemize{\item{\code{par}: }{A list of parameters' fitted values.}
#'         \item{\code{init}: }{A list of valid initial values from users.}
#'         \item{\code{par_log}: }{A list of intermediate parameters' values if \code{log_switch = TRUE}.}
#'         \item{\code{converged}: }{A list of logical values indicating whether each parameter is fitted.}
#'         }
#'
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A Kalman filter approach. Available at SSRN 3101695.
#'
#' Varadhan, R., and Roland, C. (2008). Simple and globally convergent methods for accelerating the convergence of any EM algorithm.
#' Scandinavian Journal of Statistics, 35(2), 335–353.
#'
#' @examples
#' library(intradayModel)
#' data(volume_aapl)
#' volume_aapl_training <- volume_aapl[, 1:20]
#' \donttest{
#' # fit model with no prior knowledge
#' model_fit <- fit_volume(volume_aapl_training)
#' }
#' # fit model with fixed_pars and init_pars
#' model_fit <- fit_volume(volume_aapl_training, fixed_pars = list(a_mu = 0.5, var_mu = 0.05),
#'                         init_pars = list(a_eta = 0.5))
#' \donttest{
#' # fit model with other control options
#' model_fit <- fit_volume(volume_aapl_training, verbose = 2, 
#'   control = list(acceleration = FALSE, maxit = 1000, abstol = 1e-4, log_switch = FALSE))
#' }
#'
#' @importFrom magrittr %>%
#' @import xts
#'
#' @export
fit_volume <- function(data, fixed_pars = NULL, init_pars = NULL, verbose = 0, control = NULL) {
  # error control of data
  if (!is.xts(data) & !is.matrix(data)) {
    stop("data must be matrix or xts.")
  }
  data <- clean_data(data)

  # Define a Univariate State-Space Model
  volume_model <- spec_volume_model(fixed_pars, init_pars)
  is_volume_model(volume_model, nrow(data))

  # check if fit is required
  if (Reduce("+", volume_model$converged) == 8) {
    if (verbose > 0) {
      cat("All parameters have already been fixed.\n")
    }
    return(volume_model)
  }

  # control list check
  ## initial control values
  control_final <- list(
    acceleration = TRUE, maxit = 3000, abstol = 1e-4,
    log_switch = TRUE
  )
  if (is.list(control)) {
    for (prop in c("acceleration", "maxit", "abstol", "log_switch")) {
      if (prop %in% names(control)) {
        control_final[[prop]] <- control[[prop]]
      }
    }
  }

  # specify uniss-format model (volume_model is outer interface, uniss is inner obj)
  args <- list(
    data = log(data),
    volume_model = volume_model
  )
  uniss_obj <- do.call(specify_uniss, args)

  # fit parameters with EM algorithm
  args <- append(args, list(
    uniss_obj = uniss_obj,
    verbose = verbose,
    control = control_final
  ))
  if (control_final$acceleration == FALSE) {
    em_result <- do.call(uniss_em_alg, args)
  } else {
    em_result <- do.call(uniss_em_alg_acc, args)
  }
  volume_model$par_log <- em_result$par_log

  if (length(em_result$warning_msg) > 0) {
    warning(em_result$warning_msg)
  }

  # update volume_model list object
  volume_model$par <- em_result$uniss_obj$par
  if (em_result$convergence) {
    volume_model$converged[] <- TRUE
    volume_model$init <- list()
  }

  # verbose
  if (verbose >= 2) {
    cat("--- obtained parameters ---\n")
    par_visual <- lapply(volume_model$par, as.numeric)
    par_visual$V0 <- matrix(c(
      par_visual$V0[1], par_visual$V0[2],
      par_visual$V0[2], par_visual$V0[3]
    ), 2)
    utils::str(par_visual)
    cat("---------------------------\n")
  }

  return(volume_model)
}

uniss_em_alg <- function(...) {
  # read input information
  args <- list(...)
  uniss_obj <- args$uniss_obj
  verbose <- args$verbose
  control <- args$control

  # required settings
  convergence <- FALSE
  par_log <- list(uniss_obj$par)

  # EM loops
  for (i in 1:control$maxit) {
    ## one update
    new_par <- uniss_kalman(uniss_obj, "em_update")$new_par

    ## logging
    if (control$log_switch == TRUE) {
      par_log <- append(par_log, list(new_par))
    }

    ## verbose & stopping criteria
    diff <- norm(as.numeric(unlist(uniss_obj$par)) -
      as.numeric(unlist(new_par)), type = "2")
    if (verbose >= 1 & i %% 25 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    if (diff < control$abstol) {
      convergence <- TRUE
      break
    }

    uniss_obj$par <- new_par
  }

  # verbose
  warning_msg <- NULL
  if (!convergence) {
    warning_msg <- c(warning_msg, paste("Warning! Reached maxit before parameters converged. Maxit was ", control$maxit, ".\n", sep = ""))
  } else if (verbose > 0) {
    cat("Success! abstol test passed at", i, "iterations.\n")
  }

  result <- list(
    "uniss_obj" = uniss_obj, "convergence" = convergence,
    "par_log" = par_log, "warning_msg" = warning_msg
  )
  return(result)
}

uniss_em_alg_acc <- function(...) {
  # read input information
  args <- list(...)
  uniss_obj <- args$uniss_obj
  verbose <- args$verbose
  control <- args$control

  # required settings
  convergence <- FALSE
  par_log <- list(uniss_obj$par)

  # EM loops
  for (i in 1:control$maxit) {
    ## one update
    curr_par <- uniss_obj$par
    new_par_1 <- uniss_kalman(uniss_obj, "em_update")$new_par
    uniss_obj$par <- new_par_1
    new_par_2 <- uniss_kalman(uniss_obj, "em_update")$new_par
    uniss_obj$par <- new_par_2

    new_par <- curr_par # copy the structure
    ## vector-wise acceleration for intraday periodic
    if (!uniss_obj$converged$phi) {
      r <- new_par_1$phi - curr_par$phi
      v <- new_par_2$phi - new_par_1$phi - r
      r_norm <- norm(r, "2")
      v_norm <- norm(v, "2")
      step_vec <- -r_norm / v_norm
      new_par$phi <- curr_par$phi -
        2 * step_vec * r + step_vec^2 * v
    }

    ## element-wise acceleration for other parameters
    for (name in names(curr_par)) {
      if (name != "phi" & length(curr_par[[name]]) > 0) {
        r <- new_par_1[[name]] - curr_par[[name]]
        v <- new_par_2[[name]] - new_par_1[[name]] - r
        step_len <- -abs(r) / abs(v)
        for (n in 1:length(step_len)) {
          if (abs(v[n]) > 1e-8) {
            new_par[[name]][n] <- curr_par[[name]][n] - step_len[n] * r[n]
          } else {
            new_par[[name]][n] <- new_par_2[[name]][n]
          }
        }
      }
    }

    ## acceleration error check
    if (new_par$r < 0) {
      new_par$r <- new_par_2$r
    }
    if (new_par$var_eta < 0) {
      new_par$var_eta <- new_par_2$var_eta
    }
    if (new_par$var_mu < 0) {
      new_par$var_mu <- new_par_2$var_mu
    }

    ## logging
    if (control$log_switch == TRUE) {
      par_log <- append(par_log, list(new_par))
    }

    ## verbose & stopping criteria
    diff <- norm(as.numeric(unlist(new_par_1)) -
      as.numeric(unlist(new_par_2)), type = "2")
    if (verbose >= 1 & i %% 5 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    if (diff < control$abstol) {
      convergence <- TRUE
      break
    }

    uniss_obj$par <- new_par
  }

  # verbose
  warning_msg <- NULL
  if (!convergence) {
    warning_msg <- c(warning_msg, paste("Warning! Reached maxit before parameters converged. Maxit was ", control$maxit, ".\n", sep = ""))
  } else if (verbose > 0) {
    cat("Success! abstol test passed at", i, "iterations.\n")
  }

  result <- list(
    "uniss_obj" = uniss_obj, "convergence" = convergence,
    "par_log" = par_log, "warning_msg" = warning_msg
  )
  return(result)
}
