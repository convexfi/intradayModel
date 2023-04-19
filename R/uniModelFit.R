#' @title Fit a Univaraite State-Space Model via Expectation-Maximization Algorithm
#' 
#' @description The main function for fitting a univaraite state-space model by using expectation-maximization (EM) algorithms. 
#' The implementation follows (Chen et al., 2016), and the accelerated scheme is provided in (Varadhan and Roland, 2008).
#' The algorithm terminates when \code{maxit} is reached or the condition \eqn{\|\Delta \boldsymbol{\Theta}_i\| \le \text{abstol}}{||\Delta \Theta(i)|| <= abstol} is satisfied.
#'
#' @param data Matrix of intraday signal of size n_bin * n_day without any missing values.
#' @param uniModel Univariate model list object from function \code{uniModelSpec}.
#' @param acceleration Logical value indicating whether to use the accelerated EM algorithm. If \code{TRUE}, the accelerated one will be used (default is \code{FALSE}).
#' @param maxit Maximum number of iterations (default is \code{3000}).
#' @param abstol Absolute tolerance for parameters' change \eqn{\|\Delta \boldsymbol{\Theta}_i\|}{||\Delta \Theta(i)||} as the stopping criteria (default is \code{1e-4}).
#' @param log.switch Logical value indicating whether to record the history of convergence progress. 
#'                   If \code{TRUE}, the intermediate parameters are recorded during the algorithm (default is \code{TRUE}).
#'                       
#' @param verbose An integer specifying the print level of information during the algorithm (default is \code{1}). Possible numbers:
#'                \itemize{\item{\code{"0"}: no output;}
#'                    \item{\code{"1"}: show the iteration number and \eqn{\|\Delta \boldsymbol{\Theta}_i\|}{||\Delta \Theta(i)||};}
#'                    \item{\code{"2"}: 1 + show the final parameters.}}
#' @return A list containing the following elements (if the algorithm converges):
#'         \item{\code{par}}{List of parameters' fitted values.}
#'         \item{\code{par_log}}{List of intermediate parameters' values if \code{log.switch = TRUE}.} 
#'         \item{\code{fit_request}}{List of logical values indicating whether the parameters require further fitting.}
#'                                
#' 
#' @references
#' Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday trading volume: A kalman filter approach. Available at SSRN 3101695.
#' 
#' Varadhan, R., and Roland, C. (2008). Simple and globally convergent methods for accelerating the convergence of any EM algorithm. 
#' Scandinavian Journal of Statistics, 35(2), 335–353.
#' 
#' @seealso \code{\link{uniModelSpec}}
#' 
#' @examples 
#' # fit the model to AAPL_volume
#' data(AAPL_volume)
#' model <- uniModelSpec(fit = TRUE)
#' model_fitted <- uniModelFit(AAPL_volume, model, acceleration = TRUE, 
#'                   maxit = 1000, abstol = 1e-4, log.switch = TRUE)
#' 
#' @importFrom magrittr %>%
#' 
#' @export
uniModelFit <- function(data, uniModel, acceleration = FALSE,
                        maxit = 3000, abstol = 1e-4, log.switch = TRUE,
                        verbose = 1) {
  
  # error control
  if (!is.matrix(data)) stop("data must be a matrix.")
  if (anyNA(data)) stop("data must have no NA.")
  is_uniModel(uniModel, nrow(data))
  
  # check if fit is required
  if (Reduce("+", uniModel$fit_request) == 0) {
    if (verbose > 0) {
      cat("All parameters have already been fixed.\n")
    }
    return(uniModel)
  }
  
  # specify uniss-format model (uniModel is outer interface, uniss is inner obj)
  args <- list(
    data = log(data),
    uniModel = uniModel
  )
  uniss_obj <- do.call(specify_uniss, args)
  
  # fit parameters with EM algorithm
  args <- append(args, list(
    uniss_obj = uniss_obj,
    control = list(maxit = maxit, abstol = abstol, 
                   log.switch = log.switch, acceleration = acceleration,
                   verbose = verbose)
  ))
  if (acceleration == FALSE) {
    em_result <- do.call(uniss_em_alg, args)
  } else {
    em_result <- do.call(uniss_em_alg_acc, args)
  }
  uniModel$par_log <- em_result$par_log
  
  if (length(em_result$warning_msg) > 0) {
    warning(em_result$warning_msg)
  }
  
  # update uniModel list object
  uniModel$par <- em_result$uniss_obj$par
  if (em_result$convergence) {
    uniModel$fit_request[] <- FALSE
    uniModel$init <- list()
  }
  
  # verbose
  if (verbose >= 2) {
    cat("--- obtained parameters ---\n")
    par_visual <- lapply(uniModel$par, as.numeric)
    par_visual$V0 <- matrix(c(par_visual$V0[1], par_visual$V0[2],
                              par_visual$V0[2], par_visual$V0[3]), 2)
    utils::str(par_visual)
    cat("---------------------------\n")
  }
  
  return(uniModel)
}

uniss_em_alg <- function(...) {
  # read input information
  args <- list(...)
  uniss_obj <- args$uniss_obj
  control <- args$control
  
  # required settings
  convergence <- FALSE
  par_log <- list(uniss_obj$par)
  
  # EM loops
  for (i in 1:control$maxit) {
    ## one update
    new_par <- uniss_kalman(uniss_obj, "em_update")$new_par
    
    ## logging
    if (control$log.switch == TRUE) {
      par_log <- rlist::list.append(par_log, new_par)
    }
    
    ## verbose & stopping criteria
    diff <- norm(as.numeric(unlist(uniss_obj$par)) -
                   as.numeric(unlist(new_par)), type = "2")
    if (control$verbose >= 1 & i %% 25 == 0) {
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
  if (convergence) {
    cat("Success! abstol test passed at", i, "iterations.\n")
  } else {
    warning_msg <- c(warning_msg, paste("Warning! Reached maxit before parameters converged. Maxit was ", control$maxit, ".\n", sep = ""))
  }
  result <- list("uniss_obj" = uniss_obj, "convergence" = convergence, 
                 "par_log" = par_log, "warning_msg" = warning_msg)
  return(result)
}

uniss_em_alg_acc <- function(...) {
  # read input information
  args <- list(...)
  uniss_obj <- args$uniss_obj
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
    if (uniss_obj$fit_request$phi) {
      r <- new_par_1$phi - curr_par$phi
      v <- new_par_2$phi - new_par_1$phi - r
      r_norm <- norm(r, "2")
      v_norm <- norm(v, "2")
      step_vec <- - r_norm / v_norm
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
    if (control$log.switch == TRUE) {
      par_log <- rlist::list.append(par_log, new_par)
    }
    
    ## verbose & stopping criteria
    diff <- norm(as.numeric(unlist(new_par_1)) -
                   as.numeric(unlist(new_par_2)), type = "2")
    if (control$verbose >= 1 & i %% 5 == 0) {
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
  if (convergence) {
    cat("Success! abstol test passed at", i, "iterations.\n")
  } else {
    warning_msg <- c(warning_msg, paste("Warning! Reached maxit before parameters converged. Maxit was ", control$maxit, ".\n", sep = ""))
  }
  result <- list("uniss_obj" = uniss_obj, "convergence" = convergence, 
                 "par_log" = par_log, "warning_msg" = warning_msg)
  return(result)
}