uniModelFit2 <- function(data, uniModel, acceleration = FALSE,
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
    em_result <- do.call(em_update2, args)
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
  
  browser()
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
    warning_msg <- c(warning_msg, paste("Warning! Reached maxit before parameters converged. Maxit was ", maxit, ".\n", sep = ""))
  }
  result <- list("uniss_obj" = uniss_obj, "convergence" = convergence, 
                 "par_log" = par_log, "warning_msg" = warning_msg)
  return(result)
}

