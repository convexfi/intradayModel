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
uniModelFit <- function(data.train, modelSpec, control = list(maxit = 1000, tol)) {
  
  # check if fit is necessary
  if (!is.list(modelSpec)) stop("tbd.")
  if (sum(modelSpec$fitFlag) == 0) {
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
  
  ## MARSS parameters
  MARSS_model <- list()
  MARSS_model$model.gen <- list()
  MARSS_mdoel$init.gen <- list()
  
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
  
  At = array(list(0), dim = c(1,1,n_bin_total))
  a_vec = extract_value("phi", modelSpec)
  if (a_vec == "phi" || length(a_vec) != nbin){
    if (length(a_vec) != nbin) warning("Dimensions of input data and pre-fixed phi aren't compatible.\n
                                       The values of fixed phi are ignored.")
    for (n in 1:n_bin) {
      a_vec[n] <- paste("a", n, sep = "")
    }
    
  }
  At[1, 1, ] = rep(a_vec, n_day)
  
  R <- extract_value("r1", modelSpec) %>%
    list() %>%
    matrix(1,1)
  
  ## Initial State
  x0 <- extract_value("r1", modelSpec) %>%
    matrix(2, 1)
  V0 <- extract_value("V0", modelSpec)
}

extract_value <- function(name, modelSpec) {
  len <- length(modelSpec$par[[name]])
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