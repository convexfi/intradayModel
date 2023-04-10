#' Title
#'
#' @param data.train log volume data matrix of size n_bin * n_day with no NA for fitting
#' @param uniModel uniModel object from function uniModelSpec
#' @param control List of control variables, e.g., maxit, reltol
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
uniModelFit <- function(data, uniModel,
                        maxit = 3000, abstol = 1e-4, log.switch = TRUE, acceleration = FALSE) {

  # error control
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data.frame.")
  if (anyNA(data)) stop("data must have no NA.")
  # isIntraModel(uniModel, data)

  # check if fit is required
  if (Reduce("+", uniModel$fit_request) == 0) {
    cat("All parameters are fixed. No need to fit.\n")
    return(uniModel)
  }

  # specify MARSS-format model
  args <- list(
    data = data,
    uniModel = uniModel
  )
  marss_obj <- do.call(specify_marss, args)
  marss_obj$par <- marss_obj$start

  # fit parameters with EM algorithm
  args <- append(args, list(
    marss_obj = marss_obj,
    control = list(maxit = maxit, abstol = abstol, log.switch = log.switch, acceleration = acceleration)
  ))
  if (acceleration == FALSE) {
    em_result <- do.call(em_update, args)
  } else {
    em_result <- do.call(em_update_acc, args)
  }
  uniModel$par_log <- em_result$par_log

  # change parameters in MARSS format to uniModel format
  uniModel$par <- marss_to_unimodel(em_result$marss_obj$par, uniModel$par)
  if (em_result$convergence) {
    uniModel$fit_request[] <- FALSE
  }

  return(uniModel)
}

em_update <- function(...) {
  # read input information
  args <- list(...)
  data <- args$data
  uniModel <- args$uniModel
  marss_obj <- args$marss_obj
  control <- args$control
  maxit <- control$maxit
  abstol <- control$abstol
  log.switch <- control$log.switch

  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day

  # required settings
  convergence <- FALSE
  Z_matrix <- matrix(marss_obj[["model"]][["fixed"]][["Z"]][, , 1], nrow = 1)
  jump_interval <- seq(n_bin + 1, n_bin_total, n_bin)
  data_reform <- unlist(as.list(data))
  y_daily_matrix <- matrix(data_reform, n_bin)
  par_log <- list(marss_obj$par)

  # decide unfitted parameters
  unfitted_pars <- names(uniModel$fit_request[uniModel$fit_request == TRUE])

  # EM algorithm
  environment(em_one_loop) <- environment() # pass current environment to nested fcn
  curr_par <- marss_obj$par
  for (i in 1:maxit) {
    # EM parameter updates
    new_par <- em_one_loop(curr_par)
    
    # logging
    if (log.switch == TRUE) {
      par_log <- rlist::list.append(par_log, new_par)
    }
    
    # verbose & stopping criteria
    diff <- norm(as.numeric(unlist(curr_par)) -
      as.numeric(unlist(new_par)), type = "2")
    if (i %% 25 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    if (diff < abstol) {
      convergence <- TRUE
      break
    }
    
    curr_par <- new_par
  }
  marss_obj$par <- new_par

  # reshape phi and add name for phi
  phi_names <- c()
  for (i in 1:n_bin) {
    phi_names <- append(phi_names, paste("phi", i, sep = ""))
  }
  marss_obj$par$A <- array(marss_obj$par$A, dim = c(n_bin, 1), dimnames = list(phi_names, NULL))

  # add name for R and x0
  marss_obj$par$R <- array(marss_obj$par$R, dim = c(1, 1), dimnames = list("r", NULL))
  # marss_obj$par$x0 <- array(marss_obj$par$x0, dim = c(2,1), dimnames = list(c("x01","x02"),NULL))

  if (!convergence) warning("No convergence")
  result <- list("marss_obj" = marss_obj, "convergence" = convergence, "par_log" = par_log)
  return(result)
}

em_update_acc <- function(...) {
  # read input information
  args <- list(...)
  data <- args$data
  uniModel <- args$uniModel
  marss_obj <- args$marss_obj
  control <- args$control
  maxit <- control$maxit
  abstol <- control$abstol
  log.switch <- control$log.switch
  
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  # required settings
  convergence <- FALSE
  Z_matrix <- matrix(marss_obj[["model"]][["fixed"]][["Z"]][, , 1], nrow = 1)
  jump_interval <- seq(n_bin + 1, n_bin_total, n_bin)
  data_reform <- unlist(as.list(data))
  y_daily_matrix <- matrix(data_reform, n_bin)
  par_log <- list(marss_obj$par)
  
  # decide unfitted parameters
  unfitted_pars <- names(uniModel$fit_request[uniModel$fit_request == TRUE])
  
  # EM algorithm
  environment(em_one_loop) <- environment() # pass current environment to nested fcn
  curr_par <- marss_obj$par
  for (i in 1:maxit) {
    # EM parameter updates
    new_par_1 <- em_one_loop(input_par = curr_par)
    new_par_2 <- em_one_loop(input_par = new_par_1)
    new_par <- curr_par
    
    # vector-wise acceleration for intraday periodic
    if ("phi" %in% unfitted_pars) {
      r <- new_par_1$A - curr_par$A
      v <- new_par_2$A - new_par_1$A - r
      r_norm <- norm(r, "2")
      v_norm <- norm(v, "2")
      step_vec <- - r_norm / v_norm
      new_par$A <- curr_par$A - 
        2 * step_vec * r + step_vec^2 * v
    }
    
    # element-wise acceleration for other parameters
    for (name in names(curr_par)) {
      if (name != "A" & length(curr_par[[name]]) > 0) {
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
    
    # acceleration error check
    if (new_par$R < 0) {
      new_par$R <- new_par_2$R
    }
    if (new_par$Q[1] < 0) {
      new_par$Q[1] <- new_par_2$Q[1]
    }
    if (new_par$Q[2] < 0) {
      new_par$Q[2] <- new_par_2$Q[2]
    }

    # logging
    if (log.switch == TRUE) {
      par_log <- rlist::list.append(par_log, new_par)
    }
    
    # verbose & stopping criteria
    diff <- norm(as.numeric(unlist(new_par_1)) -
                   as.numeric(unlist(new_par_2)), type = "2")
    if (i %% 1 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    if (diff < abstol) {
      convergence <- TRUE
      break
    }
    
    curr_par <- new_par
  }
  marss_obj$par <- new_par
  
  # reshape phi and add name for phi
  phi_names <- c()
  for (i in 1:n_bin) {
    phi_names <- append(phi_names, paste("phi", i, sep = ""))
  }
  marss_obj$par$A <- array(marss_obj$par$A, dim = c(n_bin, 1), dimnames = list(phi_names, NULL))
  
  # add name for R and x0
  marss_obj$par$R <- array(marss_obj$par$R, dim = c(1, 1), dimnames = list("r", NULL))
  # marss_obj$par$x0 <- array(marss_obj$par$x0, dim = c(2,1), dimnames = list(c("x01","x02"),NULL))
  
  if (!convergence) warning("No convergence")
  result <- list("marss_obj" = marss_obj, "convergence" = convergence, "par_log" = par_log)
  return(result)
}

# A single em iteration
em_one_loop <- function(input_par) {
  marss_obj$par <- input_par
  
  # Kalman filter & smoother
  Kf <- MARSS::MARSSkfas(marss_obj)
  new_par <- marss_obj$par
  
  # update parameter estimation
  Pt <- Ptt1 <- array(NA, c(2, 2, n_bin_total))
  for (n in 1:n_bin_total) {
    Pt[, , n] <- Kf$VtT[, , n] + Kf$xtT[, n] %*% t(Kf$xtT[, n])
  }
  for (n in 2:n_bin_total) {
    Ptt1[, , n] <- Kf$Vtt1T[, , n] + Kf$xtT[, n] %*% t(Kf$xtT[, n - 1])
  }
  for (name in unfitted_pars) {
    switch(name,
           "x0" = {
             new_par$x0 <- Kf$x0T
           },
           "V0" = {
             new_par$V0[1] <- Kf$V0T[1, 1]
             new_par$V0[2] <- Kf$V0T[2, 1]
             new_par$V0[3] <- Kf$V0T[2, 2]
           },
           "phi" = {
             new_par$A <- rowMeans(y_daily_matrix - matrix(Z_matrix %*% Kf$xtT, nrow = n_bin))
             new_par$A <- new_par$A - mean(new_par$A)
           },
           "r" = {
             if ("phi" %in% unfitted_pars) {
               phi.matrix <- rep(matrix(new_par$A, nrow = 1), n_day)
             } else {
               phi.matrix <- unlist(uniModel$par[["phi"]])
             } # need input
             new_par$R <- mean(data_reform^2 + apply(Pt, 3, function(p) Z_matrix %*% p %*% t(Z_matrix)) -
                                  2 * data_reform * as.numeric(Z_matrix %*% Kf$xtT) +
                                  phi.matrix^2 -
                                  2 * data_reform * phi.matrix +
                                  2 * phi.matrix * as.numeric(Z_matrix %*% Kf$xtT))
           },
           "a_eta" = {
             new_par$B["a_eta", 1] <- sum(Ptt1[1, 1, jump_interval]) /
               sum(Pt[1, 1, jump_interval - 1])
           },
           "a_mu" = {
             new_par$B["a_mu", 1] <- sum(Ptt1[2, 2, 2:n_bin_total]) /
               sum(Pt[2, 2, 1:(n_bin_total - 1)])
           },
           "var_eta" = {
             if ("a_eta" %in% unfitted_pars) {
               curr_a_eta <- new_par$B["a_eta", 1]
             } else {
               curr_a_eta <- uniModel$par[["a_eta"]]
             } # need input
             new_par$Q["var_eta", 1] <- mean(Pt[1, 1, jump_interval] +
                                                curr_a_eta^2 * Pt[1, 1, jump_interval - 1] -
                                                2 * curr_a_eta * Ptt1[1, 1, jump_interval])
           },
           "var_mu" = {
             if ("a_mu" %in% unfitted_pars) {
               curr_a_mu <- new_par$B["a_mu", 1]
             } else {
               curr_a_mu <- uniModel$par[["a_mu"]]
             } # need input
             new_par$Q["var_mu", 1] <- mean(Pt[2, 2, 2:n_bin_total] +
                                               curr_a_mu^2 * Pt[2, 2, 1:(n_bin_total - 1)] -
                                               2 * curr_a_mu * Ptt1[2, 2, 2:n_bin_total])
           }
    )
  }
  
  return(new_par)
}



