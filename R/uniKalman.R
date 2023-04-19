# define the uniss model (prepare all required information) in log form
specify_uniss <- function(...) {
  # read input information
  args <- list(...)
  data <- args$data # log intraday signal
  uniModel <- args$uniModel

  data.reform <- unlist(as.list(data))
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day

  # define uniss_obj list
  uniss_obj <- list()

  ## uniss parameters
  uniss_obj$par <- list()
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  init.default <- list(
    "x0" = matrix(c(mean(data.reform), 0), 2, 1),
    "a_eta" = 1, "a_mu" = 0,
    "r" = 1e-4,
    "var_eta" = 1e-4, "var_mu" = 1e-4,
    "V0" = matrix(c(1e-3, 1e-7, 1e-5), 3, 1),
    "phi" = rowMeans(matrix(data.reform, nrow = n_bin)) - mean(data.reform)
  )
  for (name in all.pars.name) {
    ### specify EM initial values
    if (uniModel$fit_request[[name]]) {
      if (name %in% names(uniModel$init)) {
        uniss_obj$par[[name]] <- uniModel$init[[name]]
      } else {
        uniss_obj$par[[name]] <- init.default[[name]]
      }
    } else { ### set to fixed values
      uniss_obj$par[[name]] <- uniModel$par[[name]]
    }
  }

  ## uniss other properties
  uniss_obj$y <- data.reform
  uniss_obj$n_bin <- n_bin
  uniss_obj$n_day <- n_day
  uniss_obj$n_bin_total <- n_bin_total
  uniss_obj$fit_request <- uniModel$fit_request

  return(uniss_obj)
}


# one-time Kalman filter, smoother, and em update on uniss
# type = c("filter", "smoother", "em_update")
uniss_kalman <- function(uniss_obj, type = "em_update") {
  result <- list()

  # filter -----------------------------------

  ## declare containers for filter results
  xtt1 <- matrix(NA, 2, uniss_obj$n_bin_total)
  Vtt1 <- array(NA, dim = c(2, 2, uniss_obj$n_bin_total))
  xtt <- matrix(NA, 2, uniss_obj$n_bin_total)
  Vtt <- array(NA, dim = c(2, 2, uniss_obj$n_bin_total))
  Kt <- matrix(NA, 2, uniss_obj$n_bin_total)

  ## fixed-variables to use
  A_jump <- matrix(c(uniss_obj$par$a_eta, 0, 0, uniss_obj$par$a_mu), 2, 2)
  A_intra <- matrix(c(1, 0, 0, uniss_obj$par$a_mu), 2, 2)
  Q_jump <- matrix(c(uniss_obj$par$var_eta, 0, 0, uniss_obj$par$var_mu), 2, 2)
  Q_intra <- matrix(c(0, 0, 0, uniss_obj$par$var_mu), 2, 2)
  r <- uniss_obj$par$r
  phi <- rep(uniss_obj$par$phi, uniss_obj$n_day)
  C <- matrix(1, nrow = 1, ncol = 2)

  ## initialize according to MARSS tinitx = 1 rule
  xtt1[, 1] <- uniss_obj$par$x0
  Vtt1[, , 1] <- matrix(c(
    uniss_obj$par$V0[1], uniss_obj$par$V0[2],
    uniss_obj$par$V0[2], uniss_obj$par$V0[3]
  ), 2)
  Kt[, 1] <- Vtt1[, , 1] %*% t(C) %*% solve(C %*% Vtt1[, , 1] %*% t(C) + r)
  xtt[, 1] <- xtt1[, 1] + Kt[, 1] %*% (uniss_obj$y[1] - phi[1] - C %*% xtt1[, 1])
  Vtt[, , 1] <- Vtt1[, , 1] - Kt[, 1] %*% C %*% Vtt1[, , 1]

  ## Kalman filter recursion
  for (i in 1:(uniss_obj$n_bin_total - 1)) {
    ### prediction
    if ((i %% uniss_obj$n_bin) == 0) {
      xtt1[, i + 1] <- A_jump %*% xtt[, i]
      Vtt1[, , i + 1] <- A_jump %*% Vtt[, , i] %*% t(A_jump) + Q_jump
    } else {
      xtt1[, i + 1] <- A_intra %*% xtt[, i]
      Vtt1[, , i + 1] <- A_intra %*% Vtt[, , i] %*% t(A_intra) + Q_intra
    }

    ### kalman gain
    Kt[, i + 1] <- Vtt1[, , i + 1] %*% t(C) %*% solve(C %*% Vtt1[, , i + 1] %*% t(C) + r)

    ### measurement update
    xtt[, i + 1] <- xtt1[, i + 1] + Kt[, i + 1] %*%
      (uniss_obj$y[i + 1] - phi[i + 1] - C %*% xtt1[, i + 1])
    Vtt[, , i + 1] <- Vtt1[, , i + 1] - Kt[, i + 1] %*% C %*% Vtt1[, , i + 1]
  }

  result$xtt1 <- xtt1
  result$Vtt1 <- Vtt1
  result$Kt <- Kt
  result$xtt <- xtt
  result$Vtt <- Vtt
  if (type == "filter") {
    return(result)
  }

  # smoother -----------------------------------

  ## declare containers for smoother results
  xtT <- matrix(NA, 2, uniss_obj$n_bin_total)
  VtT <- array(NA, dim = c(2, 2, uniss_obj$n_bin_total))
  Lt <- array(NA, dim = c(2, 2, (uniss_obj$n_bin_total - 1)))

  ## initialize
  xtT[, uniss_obj$n_bin_total] <- xtt[, uniss_obj$n_bin_total]
  VtT[, , uniss_obj$n_bin_total] <- Vtt[, , uniss_obj$n_bin_total]

  # Kalman smoother recursion
  for (i in (uniss_obj$n_bin_total - 1):1) {
    if ((i %% uniss_obj$n_bin) == 0) {
      Lt[, , i] <- Vtt[, , i] %*% t(A_jump) %*% solve(Vtt1[, , (i + 1)])
    } else {
      Lt[, , i] <- Vtt[, , i] %*% t(A_intra) %*% solve(Vtt1[, , (i + 1)])
    }
    xtT[, i] <- xtt[, i] + Lt[, , i] %*% (xtT[, i + 1] - xtt1[, i + 1])
    VtT[, , i] <- Vtt[, , i] + Lt[, , i] %*% (VtT[, , i + 1] - Vtt1[, , i + 1]) %*% t(Lt[, , i])
  }

  result$xtT <- xtT
  result$VtT <- VtT
  result$Lt <- Lt
  if (type == "smoother") {
    return(result)
  }
  
  # em update -----------------------------------

  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  
  
  browser()
}
