#' Title
#'
#' @param data
#' @param modelSpec
#' @param control
#'
#' @return
#' @export
#'
#' @examples
uniModelFilter <- function(data.filter, modelSpec) {
  # check if fit is necessary
  if (!is.list(modelSpec)) stop("tbd.")
  if (Reduce("+", modelSpec$fitFlag) != 0) {
    stop("All parameters must be fixed.\n")
  }
  
  # error control
  if (!is.matrix(data.filter) && !is.data.frame(data.filter)) stop("data must be a matrix or data.frame.")
  if (anyNA(data.filter)) stop("data must have no NA.")
  
  data.filter <- as.matrix(data.filter)
  n_bin <- nrow(data.filter)
  n_day <- ncol(data.filter)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data.reform <- data.filter %>%
    as.list() %>%
    unlist()
  
  ## MARSS parameters
  MARSS_model <- list()
  MARSS_model$model.gen <- list()
  MARSS_model$init.gen <- list()
  
  ## State Equation
  Bt <- array(list(0), c(2, 2, n_bin_total))
  b1 <- matrix(list(1), n_bin)
  b1[1] <- modelSpec$par[["a_eta"]]
  Bt[1, 1, ] <- rep(b1, n_day)
  Bt[2, 2, ] <- modelSpec$par[["a_mu"]]
  
  Qt <- array(list(0), c(2, 2, n_bin_total))
  q1 <- matrix(list(1e-10), n_bin)
  q1[1] <- modelSpec$par[["var_eta"]]
  Qt[1, 1, ] <- rep(q1, n_day)
  Qt[2, 2, ] <- modelSpec$par[["var_mu"]]
  
  U <- "zero"
  
  ## Measurement Equation
  Z <- array(list(1, 1), c(1, 2))
  
  At = array(list(0), dim = c(1, 1, n_bin_total))
  a_vec = modelSpec$par[["phi"]]
  At[1, 1, ] = rep(a_vec, n_day)
  
  R <- extract_value("r", modelSpec) %>%
    list() %>%
    matrix(1,1)
  
  ## Initial State
  x0 <- extract_value("x0", modelSpec) %>%
    matrix(2, 1)
  V0 <- extract_value("V0", modelSpec)
  
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  kalman.ours <- MARSS::MARSS(data.train_reform, model=MARSS_model$model.gen, fit=FALSE)
  KfList.all <- MARSS::MARSSkfas(kalman.ours)
  
  return (KfList.all)
  
}
