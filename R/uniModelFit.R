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
  init_predefined <- list(x0 = matrix(c(10, 0), 2, 1),
                          B = matrix(c(1,0.7), 2, 1),
                          R = 0.08,
                          Q = matrix(c(0.07, 0.06), 2, 1),
                          V0 = matrix(c(1e-10, 0, 1e-10), 3, 1),
                          A = rowMeans(matrix(data.train_reform, nrow = n_bin)) - mean(data.train_reform)
  )
  
  ## Init param
  MARSS_model$init.gen <- init_predefined
  
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  kalman.ours <- MARSS::MARSS(data.train_reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE)
  kalman.ours$par <- kalman.ours$start
  
  Z.matrix <- matrix(unlist(Z), nrow = 1)
  y.daily.matrix <- matrix(data.train_reform, nrow = n_bin)
  jump_interval <- seq(n_bin + 1, n_bin_total, n_bin)
  kalman.ours <- EM_param(kalman.ours,data.train_reform, Z.matrix, y.daily.matrix,n_bin, n_bin_total,n_day, jump_interval, control)
  
  modelSpec$par <- kalman.ours$par
  
  return (modelSpec)
  
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

EM_param <- function(kalman.ours,y_train, Z.matrix, y.daily.matrix,n_bin, nBin_train,nDay_train, jump_interval,control){
  maxit <- control$maxit
  abstol <- control$abstol
  for (i in 1: maxit) {
    # Kalman filter & smoother
    Kf <- MARSS::MARSSkfas(kalman.ours)
    curr_par <- kalman.ours$par
    
    # update parameter estimation
    Pt <- Ptt1 <- array(NA, c(2, 2, nBin_train))
    for (n in 1: nBin_train) {
      Pt[, , n] <- Kf$VtT[, , n] + Kf$xtT[, n] %*% t(Kf$xtT[, n])
    }
    for (n in 2: nBin_train) {
      Ptt1[, , n] <- Kf$Vtt1T[, , n] + Kf$xtT[, n] %*% t(Kf$xtT[, n - 1])
    }
    
    curr_par$x0 <- Kf$x0T
    curr_par$V0[1] <- Kf$V0T[1, 1]
    curr_par$V0[2] <- Kf$V0T[2, 1]
    curr_par$V0[3] <- Kf$V0T[2, 2]
    
    curr_par$B[1] <- sum(Ptt1[1, 1, jump_interval]) /
      sum(Pt[1, 1, jump_interval - 1])
    curr_par$B[2] <- sum(Ptt1[2, 2, 2: nBin_train]) /
      sum(Pt[2, 2, 1: (nBin_train - 1)])
    
    curr_par$Q[1] <- mean(Pt[1, 1, jump_interval] +
                            curr_par$B[1]^2 * Pt[1, 1, jump_interval - 1] -
                            2 * curr_par$B[1] * Ptt1[1, 1, jump_interval])
    curr_par$Q[2] <- mean(Pt[2, 2, 2: nBin_train] +
                            curr_par$B[2]^2 * Pt[2, 2, 1: (nBin_train - 1)] -
                            2 * curr_par$B[2] * Ptt1[2, 2, 2: nBin_train])
    
    phi.matrix <- rep(matrix(curr_par$A, nrow = 1), nDay_train)
    curr_par$R <- mean(y_train^2 +
                         apply(Pt, 3, function(p) Z.matrix %*% p %*% t(Z.matrix)) -
                         2 * y_train * as.numeric(Z.matrix %*% Kf$xtT) +
                         phi.matrix^2 -
                         2 * y_train * phi.matrix +
                         2 * phi.matrix * as.numeric(Z.matrix %*% Kf$xtT))
    curr_par$A <-
      rowMeans(y.daily.matrix - matrix(Z.matrix %*% Kf$xtT, nrow = n_bin))
    
    # stopping criteria
    diff <- norm(as.numeric(unlist(kalman.ours$par)) -
                   as.numeric(unlist(curr_par)), type = "2")
    if (diff < abstol) {
      break
    }
    
    if (i %% 25 == 0) {
      cat("iter:", i, " diff:", diff, "\n", sep = "")
    }
    
    # if (log.switch == TRUE) {
    #   par_log <- list.append(par_log, curr_par)
    # }
    kalman.ours$par <- curr_par
  }
  return (kalman.ours)
}
