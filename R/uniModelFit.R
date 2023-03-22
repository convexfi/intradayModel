#' Title
#'
#' @param data Volume data matrix of size n_bin * n_day with no NA for fitting
#' @param modelSpec modelSpec object from function uniModelSpec
#' @param control List of control variables, e.g., maxit, reltol
#'
#' @return
#' @export
#'
#' @examples
uniModelFit <- function(data, modelSpec, control = list(maxit = 1000)) {
  
  # check if fit is necessary
  if (sum(modelSpec$fitFlag) == 0) {
    cat("All parameters are fixed. No need to fit.\n")
    break
  }
  
  data.train <- log(as.matrix(data))
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## MARSS parameters
  MARSS_model <- list()
  MARSS_model$model.gen <- list()
  MARSS_mdoel$init.gen <- list()
  
  ## State Equation
  Bt = array(list(0), c(2, 2, nBin_train))
  b1 = matrix(list(1), n_bin)
  b1[1] = extract_value("a_eta", modelSpec)
  Bt[1, 1, ] = rep(b1, nDay_train)
  Bt[2, 2, ] = extract_value("a_mu", modelSpec)

  Qt = array(list(0), c(2, 2, nBin_train))
  q1 = matrix(list(1e-10), n_bin)
  q1[1] = extract_value("var_eta", modelSpec)
  Qt[1, 1, ] = rep(q1, nDay_train)
  Qt[2, 2, ] = extract_value("var_mu", modelSpec)
  
  U = "zero"
  
  ## Measurement Equation
  Z = array(list(1, 1), c(1, 2))
  
  At = array(list(0), dim = c(1,1,nBin_train))
  a_vec = rep(0, n_bin)
  for (n in 1:n_bin) {
    a_vec[n] <- paste("a", n, sep = "")
  }
  At[1, 1, ] = rep(a_vec, nDay_train)
  
  R = matrix(list('r1'), 1, 1)
  
  ## Initial State
  x0 = matrix(list("x01", "x02"), 2, 1)
  V0 = "unconstrained"
}

extract_value <- function(name, modelSpec) {
  len <- length(modelSpec$par[[name]])
  if (modelSpec$fitFlag[[name]]) {
    a_vec <- rep(NA, len)
    for (n in 1:n_bin) {
      a_vec[n] <- paste(name, ".", n, sep = "")
    }
    return(name)
  } else {
    return(modelSpec$par[[name]])
  }
}

