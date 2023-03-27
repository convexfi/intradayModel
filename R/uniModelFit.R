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
uniModelFit <- function(data.train, modelSpec, control = list(maxit = 3000, abstol = 1e-4)) {
  
  # check if fit is necessary
  if (!is.list(modelSpec)) stop("tbd.")
  if (Reduce("+", modelSpec$fitFlag) == 0) {
    cat("All parameters are fixed. No need to fit.\n")
    break
  }
  
  # error control
  if (!is.matrix(data.train) && !is.data.frame(data.train)) stop("data.train must be a matrix or data.frame.")
  if (anyNA(data.train)) stop("data.train must have no NA.")
  
  
  data.train <- as.matrix(data.train)
  n_bin <- nrow(data.train)
  n_day <- ncol(data.train)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data.train_reform <- data.train %>%
    as.list() %>%
    unlist()
  
  ## MARSS parameters
  MARSS_model <- list()
  MARSS_model$model.gen <- list()
  MARSS_model$init.gen <- list()
  
  ## State Equation
  Bt <- array(list(0), c(2, 2, n_bin_total))
  b1 <- matrix(list(1), n_bin)
  b1[1] <- extract_value("a_eta", modelSpec)
  Bt[1, 1, ] <- rep(b1, n_day)
  Bt[2, 2, ] <- extract_value("a_mu", modelSpec)
  
  Qt <- array(list(0), c(2, 2, n_bin_total))
  q1 <- matrix(list(1e-10), n_bin)
  q1[1] <- extract_value("var_eta", modelSpec)
  Qt[1, 1, ] <- rep(q1, n_day)
  Qt[2, 2, ] <- extract_value("var_mu", modelSpec)
  
  U <- "zero"
  
  ## Measurement Equation
  Z <- array(list(1, 1), c(1, 2))
  
  At = array(list(0), dim = c(1, 1, n_bin_total))
  a_vec = extract_value("phi", modelSpec)
  if (a_vec == "phi" || length(a_vec) != n_bin){
    if (a_vec != "phi" && length(a_vec) != n_bin) warning("Dimensions of input data and pre-fixed phi aren't compatible.\n
                                       The values of fixed phi are ignored.")
    for (n in 1:n_bin) {
      a_vec[n] <- paste("a", n, sep = "")
    }
    
  }
  
  At[1, 1, ] = rep(a_vec, n_day)
  
  R <- extract_value("r", modelSpec) %>%
    list() %>%
    matrix(1,1)
  
  ## Initial State
  x0 <- extract_value("x0", modelSpec) %>%
    matrix(2, 1)
  V0 <- extract_value("V0", modelSpec)
  
  ## predefined init value
  init.default <- list("x0" = matrix(c(10, 0), 2, 1),
                       "a_eta" = 1, "a_mu" = 0.7,
                       "r" = 0.08,
                       "var_eta" = 0.07, "var_mu" = 0.06,
                       "V0" = matrix(c(1e-10, 0, 1e-10), 3, 1),
                       "phi" = rowMeans(matrix(data.train_reform, nrow = n_bin)) - mean(data.train_reform)
  )
  ## Init param
  MARSS_model$init.gen <- extract_init(init.default, modelSpec$init, modelSpec$fitFlag)
  
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  kalman.ours <- MARSS::MARSS(data.train_reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE)
  kalman.ours$par <- kalman.ours$start
  
  # args <- list(y_train = data_log_volume,
  #              n_bin = n_bin,
  #              n_bin_total = n_bin_total,
  #              n_day = n_day,
  #              control = control
  #              )
  kalman.ours <- EM_param(kalman.ours, modelSpec,
                          data.train_reform, n_bin,
                          n_bin_total,
                          n_day,
                          control)
  
  # modelSpec$par <- kalman.ours$par
  # modelSpec$fitFlag[] <- TRUE
  
  # return (modelSpec)
  
}

extract_value <- function(name, modelSpec) {
  if (modelSpec$fitFlag[[name]]) {
    name <- switch(name,
                   "x0"= list("x01","x02"),
                   "V0" = "unconstrained",
                   name)
    return(name)
  } else {
    return(modelSpec$par[[name]])
  }
}

extract_init <- function(init.default, init.pars, fitFlag){
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  for (name in all.pars.name){
    if (!fitFlag[[name]]) {
      init.default[[name]] <- NULL
    }
    if (name %in% names(init.pars)){
      init.default[[name]] <- init.pars[[name]]
    }
  }
  init.marss <- list()
  var.init <- c()
  a.init <- c()
  for (name in names(init.default)){
    tmp <- switch(name,
                  "phi" = {init.marss$A <- init.default$phi},
                  "V0" = {init.marss$V0 <- init.default$V0},
                  "x0" = {init.marss$x0 <- init.default$x0},
                  "r" = {init.marss$R <- init.default$r},
                  "a_eta" = {a.init <- append(a.init, init.default$a_eta)},
                  "a_mu" = {a.init <- append(a.init, init.default$a_mu)},
                  "var_eta" = {var.init <- append(var.init, init.default$var_eta)},
                  "var_mu" = {var.init <- append(var.init, init.default$var_mu)}
    )
  }
  if (length(a.init) > 0){
    init.marss$B <- matrix(a.init, length(a.init), 1)
  }
  if (length(var.init) > 0){
    init.marss$Q <- matrix(var.init, length(var.init), 1)
  }
  return (init.marss)
}

EM_param <- function(kalman.ours,modelSpec,
                     y_train, n_bin,
                     n_bin_total,
                     n_day,
                     control){
  Z.matrix <- matrix(kalman.ours[["model"]][["fixed"]][["Z"]][,,1], nrow = 1) # matrix(unlist(Z), nrow = 1)
  jump_interval <- seq(n_bin + 1, n_bin_total, n_bin)
  y.daily.matrix <- matrix(y_train, n_bin)
  
  
  maxit <- 1 #control$maxit
  abstol <- control$abstol
  
  # fixed params
  fix <- modelSpec$fitFlag
  fixed.names <- names(fix[fix == FALSE])
  unfixed.names <- names(fix[fix == TRUE])
  
  for (i in 1: maxit) {
    # Kalman filter & smoother
    Kf <- MARSS::MARSSkfas(kalman.ours)
    curr_par <- kalman.ours$par
    
    
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
      curr_par$R <<- mean(y_train^2 + apply(Pt, 3, function(p) Z.matrix %*% p %*% t(Z.matrix)) -
                            2 * y_train * as.numeric(Z.matrix %*% Kf$xtT) +
                            phi.matrix^2 -
                            2 * y_train * phi.matrix +
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
    diff <- norm(as.numeric(unlist(kalman.ours$par)) -
                   as.numeric(unlist(curr_par)), type = "2")
    if (diff < abstol) {
      break
    }
    
    if (i %% 25 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    kalman.ours$par <- curr_par
  }
  return (kalman.ours)
}

