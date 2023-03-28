#' Title
#'
#' @param data.train log volume data matrix of size n_bin * n_day with no NA for fitting
#' @param modelSpec modelSpec object from function uniModelSpec
#' @param control List of control variables, e.g., maxit, reltol
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
uniModelFit <- function(data, modelSpec,
                        control = list(maxit = 3000, abstol = 1e-4, log.switch = TRUE)) {
  
  # check if fit is necessary
  if (!is.list(modelSpec)) stop("tbd.")
  if (Reduce("+", modelSpec$fitFlag) == 0) {
    cat("All parameters are fixed. No need to fit.\n")
    break
  }
  
  # error control
  if (!is.matrix(data) && !is.data.frame(data)) stop("Input data must be a matrix or data.frame.")
  if (anyNA(data)) stop("Input data must have no NA.")
  
  
  data <- as.matrix(data)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  # data.train_reform <- data %>%
  #                      as.list() %>%
  #                      unlist()
  
  args <- list(data = data, n_bin = n_bin,
               n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec,
               control = control)
  
  
  result <- do.call(MARSS_spec, args = args)
  kalman <- result$kalman
  At <- result$At
  
  kalman$par <- kalman$start
  args <- append(args, list(kalman = kalman, At = At))
  
  EM_result <- do.call(EM_param, args = args)
  
  # EM_result <- EM_param(kalman, modelSpec,
  #                         data.train_reform, n_bin,
  #                         n_bin_total,
  #                         n_day,At,
  #                         control)
  
  modelSpec$par <- trans_MARSStoIntra(EM_result$model$par, modelSpec$par)
  if (EM_result$convergence) {
    modelSpec$fitFlag[] <- TRUE
  }
  
  
  return (modelSpec)
  
}

EM_param <- function(...){
  args <- list(...)
  data <- args$data
  n_bin <- args$n_bin
  n_bin_total <- args$n_bin_total
  n_day <- args$n_day
  modelSpec <- args$modelSpec
  kalman <- args$kalman
  control <- args$control
  At <- args$At
  
  data.reform <- data %>%
    as.list() %>%
    unlist()
  
  convergence <- FALSE
  Z.matrix <- matrix(kalman[["model"]][["fixed"]][["Z"]][,,1], nrow = 1) # matrix(unlist(Z), nrow = 1)
  jump_interval <- seq(n_bin + 1, n_bin_total, n_bin)
  y.daily.matrix <- matrix(data.reform, n_bin)
  
  
  maxit <- control$maxit
  abstol <- control$abstol
  log.switch <- control$log.switch
  
  # fixed params
  fix <- modelSpec$fitFlag
  unfixed.names <- names(fix[fix == TRUE])
  
  par_log <- list(kalman.ours$par)
  for (i in 1: maxit) {
    # Kalman filter & smoother
    Kf <- MARSS::MARSSkfas(kalman)
    curr_par <- kalman$par
    
    # update parameter estimation
    Pt <- Ptt1 <- array(NA, c(2, 2, n_bin_total))
    for (n in 1: n_bin_total) {
      Pt[, , n] <- Kf$VtT[, , n] + Kf$xtT[, n] %*% t(Kf$xtT[, n])
    }
    for (n in 2: n_bin_total) {
      Ptt1[, , n] <- Kf$Vtt1T[, , n] + Kf$xtT[, n] %*% t(Kf$xtT[, n - 1])
    }
    
    update_r <- function(){
      if("phi" %in% unfixed.names) {phi.matrix <- rep(matrix(curr_par$A, nrow = 1), n_day) }
      else {phi.matrix <-unlist(At)} # need input
      curr_par$R <<- mean(data.reform^2 + apply(Pt, 3, function(p) Z.matrix %*% p %*% t(Z.matrix)) -
                            2 * data.reform * as.numeric(Z.matrix %*% Kf$xtT) +
                            phi.matrix^2 -
                            2 * data.reform * phi.matrix +
                            2 * phi.matrix * as.numeric(Z.matrix %*% Kf$xtT))
    }
    update_var_eta <- function(){
      if("a_eta" %in% unfixed.names) {curr_a_eta <- curr_par$B["a_eta",1] }
      else {curr_a_eta <- modelSpec$par[["a_eta"]]} # need input
      curr_par$Q["var_eta",1] <<- mean(Pt[1, 1, jump_interval] +
                                         curr_a_eta^2 * Pt[1, 1, jump_interval - 1] -
                                         2 * curr_a_eta * Ptt1[1, 1, jump_interval])
    }
    update_var_mu <- function(){
      if("a_mu" %in% unfixed.names) {curr_a_mu <- curr_par$B["a_mu",1] }
      else {curr_a_mu <- modelSpec$par[["a_mu"]]} # need input
      curr_par$Q["var_mu",1] <<- mean(Pt[2, 2, 2: n_bin_total] +
                                        curr_a_mu^2 * Pt[2, 2, 1: (n_bin_total - 1)] -
                                        2 * curr_a_mu * Ptt1[2, 2, 2: n_bin_total])
    }
    
    
    for (name in unfixed.names){
      switch(name,
             "x0" = {curr_par$x0 <- Kf$x0T},
             "V0" = {curr_par$V0[1] <- Kf$V0T[1, 1]
             curr_par$V0[2] <- Kf$V0T[2, 1]
             curr_par$V0[3] <- Kf$V0T[2, 2]
             },
             "phi" = {curr_par$A <- rowMeans(y.daily.matrix - matrix(Z.matrix %*% Kf$xtT, nrow = n_bin))},
             "r" = {update_r()},
             "a_eta" = {curr_par$B["a_eta",1] <- sum(Ptt1[1, 1, jump_interval]) /
               sum(Pt[1, 1, jump_interval - 1])},
             "a_mu" = {curr_par$B["a_mu",1] <- sum(Ptt1[2, 2, 2: n_bin_total]) /
               sum(Pt[2, 2, 1: (n_bin_total - 1)])},
             "var_eta" = {update_var_eta()},
             "var_mu" = {update_var_mu()}
             
      )
    }
    
    # stopping criteria
    diff <- norm(as.numeric(unlist(kalman$par)) -
                   as.numeric(unlist(curr_par)), type = "2")
    if (diff < abstol) {
      convergence <- TRUE
      break
    }
    
    if (i %% 25 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    
    if (log.switch == TRUE) {
      par_log <- list.append(par_log, curr_par)
    }
    kalman$par <- curr_par
    
  }
  # reshape phi
  phi_names <- c()
  for (i in 1:n_bin){
    phi_names <- append(phi_names, paste(paste("phi", i, sep = "")))
  }
  kalman$par$A <- array(kalman$par$A, dim = c(n_bin,1), dimnames = list(phi_names,NULL))
  if (!convergence) warning("No convergence")
  result <- list("model" = kalman, "convergence" = convergence, par_log = par_log)
  return (result)
}

fetch_par_log <- function(par_log, index) {
  par_list <- list()
  for (i in 1:length(par_log)) {
    par_list <- list.append(par_list, par_log[[i]][[index]])
  }
  return(do.call(cbind, par_list))
}
