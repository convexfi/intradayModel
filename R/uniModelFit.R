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
                        maxit = 3000, abstol = 1e-4, log.switch = TRUE) {

  # error control
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data.frame.")
  if (anyNA(data)) stop("data must have no NA.")
  isIntraModel(uniModel, nrow(data))

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
    control = list(maxit = maxit, abstol = abstol, log.switch = log.switch)
  ))
  em_result <- do.call(em_update, args)
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

  # EM iteration
  for (i in 1:maxit) {
    # Kalman filter & smoother
    Kf <- MARSS::MARSSkfas(marss_obj)
    curr_par <- marss_obj$par

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
          curr_par$x0 <- Kf$x0T
        },
        "V0" = {
          curr_par$V0[1] <- Kf$V0T[1, 1]
          curr_par$V0[2] <- Kf$V0T[2, 1]
          curr_par$V0[3] <- Kf$V0T[2, 2]
        },
        "phi" = {
          curr_par$A <- rowMeans(y_daily_matrix - matrix(Z_matrix %*% Kf$xtT, nrow = n_bin))
          curr_par$A <- curr_par$A - mean(curr_par$A)
        },
        "r" = {
          if ("phi" %in% unfitted_pars) {
            phi.matrix <- rep(matrix(curr_par$A, nrow = 1), n_day)
          } else {
            phi.matrix <- unlist(uniModel$par[["phi"]])
          } # need input
          curr_par$R <- mean(data_reform^2 + apply(Pt, 3, function(p) Z_matrix %*% p %*% t(Z_matrix)) -
            2 * data_reform * as.numeric(Z_matrix %*% Kf$xtT) +
            phi.matrix^2 -
            2 * data_reform * phi.matrix +
            2 * phi.matrix * as.numeric(Z_matrix %*% Kf$xtT))
        },
        "a_eta" = {
          curr_par$B["a_eta", 1] <- sum(Ptt1[1, 1, jump_interval]) /
            sum(Pt[1, 1, jump_interval - 1])
        },
        "a_mu" = {
          curr_par$B["a_mu", 1] <- sum(Ptt1[2, 2, 2:n_bin_total]) /
            sum(Pt[2, 2, 1:(n_bin_total - 1)])
        },
        "var_eta" = {
          if ("a_eta" %in% unfitted_pars) {
            curr_a_eta <- curr_par$B["a_eta", 1]
          } else {
            curr_a_eta <- uniModel$par[["a_eta"]]
          } # need input
          curr_par$Q["var_eta", 1] <- mean(Pt[1, 1, jump_interval] +
            curr_a_eta^2 * Pt[1, 1, jump_interval - 1] -
            2 * curr_a_eta * Ptt1[1, 1, jump_interval])
        },
        "var_mu" = {
          if ("a_mu" %in% unfitted_pars) {
            curr_a_mu <- curr_par$B["a_mu", 1]
          } else {
            curr_a_mu <- uniModel$par[["a_mu"]]
          } # need input
          curr_par$Q["var_mu", 1] <- mean(Pt[2, 2, 2:n_bin_total] +
            curr_a_mu^2 * Pt[2, 2, 1:(n_bin_total - 1)] -
            2 * curr_a_mu * Ptt1[2, 2, 2:n_bin_total])
        }
      )
    }

    # verbose and logging
    if (i %% 25 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    if (log.switch == TRUE) {
      par_log <- rlist::list.append(par_log, curr_par)
    }

    # stopping criteria
    diff <- norm(as.numeric(unlist(marss_obj$par)) -
      as.numeric(unlist(curr_par)), type = "2")
    if (diff < abstol) {
      convergence <- TRUE
      break
    }
    marss_obj$par <- curr_par
  }

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