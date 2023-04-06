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
uniModelFilter <- function(data, modelSpec) {
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  # check if fit is necessary
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data.frame.")
  if (anyNA(data)) stop("data must have no NA.")
  isIntraModel(modelSpec, data)
  # if (!is.list(modelSpec)) stop("tbd.")
  # todo
  if (Reduce("+", modelSpec$fitFlag) != 0) {
    stop("All parameters must be fixed.\n")
  }
  
  # if (modelSpec$fitFlag[["x0"]] || modelSpec$fitFlag[["V0"]]){
  #   modelSpec <- uniModelFit(data.p1, modelSpec)
  # }
  Filter.result <- Filter(data, modelSpec)
  
  
  
  return (Filter.result)
  
}

Filter <- function(data, modelSpec){
  data <- as.matrix(data)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  args <- list(data = data, n_bin = n_bin,
               n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec)
  result <- do.call(MARSS_spec, args = args)
  kalman <- result$kalman
  KfList.all <- MARSS::MARSSkfas(kalman)
  result <- list("daily" = KfList.all$xtT[1,],
                 "dynamic" = KfList.all$xtT[2,],
                 "seasonal" = as.vector(kalman$model$fixed[["A"]]))
  
  return (result)
}
# Filter <- function(data, modelSpec){
#   data <- as.matrix(data)
#   n_bin <- nrow(data)
#   n_day <- ncol(data)
#   n_bin_total <- n_bin * n_day
#
#   ## reform data
#   data.reform <- data %>%
#     as.list() %>%
#     unlist()
#
#   ## MARSS parameters
#   MARSS_model <- list()
#   MARSS_model$model.gen <- list()
#   MARSS_model$init.gen <- list()
#
#   ## State Equation
#   Bt <- array(list(0), c(2, 2, n_bin_total))
#   b1 <- matrix(list(1), n_bin)
#   b1[1] <- modelSpec$par[["a_eta"]]
#   Bt[1, 1, ] <- rep(b1, n_day)
#   Bt[2, 2, ] <- modelSpec$par[["a_mu"]]
#
#   Qt <- array(list(0), c(2, 2, n_bin_total))
#   q1 <- matrix(list(1e-10), n_bin)
#   q1[1] <- modelSpec$par[["var_eta"]]
#   Qt[1, 1, ] <- rep(q1, n_day)
#   Qt[2, 2, ] <- modelSpec$par[["var_mu"]]
#
#   U <- "zero"
#
#   ## Measurement Equation
#   Z <- array(list(1, 1), c(1, 2))
#
#   At = array(list(0), dim = c(1, 1, n_bin_total))
#   a_vec = modelSpec$par[["phi"]]
#   At[1, 1, ] = rep(a_vec, n_day)
#
#   R <- extract_value("r", modelSpec) %>%
#     list() %>%
#     matrix(1,1)
#
#   ## Initial State
#   x0 <- extract_value("x0", modelSpec) %>%
#     matrix(2, 1)
#   V0 <- extract_value("V0", modelSpec)
#
#   MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
#   kalman.ours <- MARSS::MARSS(data.reform, model=MARSS_model$model.gen, fit=FALSE)
#   KfList.all <- MARSS::MARSSkfas(kalman.ours)
#
#   return (KfList.all)
#
# }

