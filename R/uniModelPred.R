#' Title
#'
#' @param data.pre
#' @param model
#' @param n.ahead
#' @param out_of_sample
#'
#' @return
#' @export
#'
#' @examples
uniModelPred <- function(data, modelSpec, out_of_sample, init_state = list()) {
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  # check if fit is necessary
  if (!is.list(modelSpec)) stop("tbd.")
  # todo
  # if (Reduce("+", modelSpec$fitFlag) != 0) {
  #   stop("All parameters must be fixed.\n")
  # }
  
  # error control
  # if (!is.matrix(data.filter) && !is.data.frame(data.filter)) stop("data must be a matrix or data.frame.")
  # if (anyNA(data.filter) ||) stop("data must have no NA.")
  #
  
  
  if (modelSpec$fitFlag[["x0"]] || modelSpec$fitFlag[["V0"]]){
    for (name in c("x0", "V0")){
      if (modelSpec$fitFlag[[name]] && name %in% names(init_state)) {
        modelSpec$par[[name]] <- init_state[[name]]
        modelSpec$fitFlag[[name]] <- FALSE
      }
    }
  }
  if (modelSpec$fitFlag[["x0"]] || modelSpec$fitFlag[["V0"]]){
    if(out_of_sample == ncol(data)) stop("Need data for estimating the initial state.")
    modelSpec <- uniModelFit(data[,1:(ncol(data) - out_of_sample)], modelSpec)
  }
  
  y.pred <- Pred(data, modelSpec)
  
  return (y.pred)
  
}

Pred <- function(data, modelSpec){
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
  x.pred <- KfList.all[["xtt1"]]
  seasonal <- modelSpec$par$phi[,1]
  names(seasonal) <- NULL
  y.pred <- x.pred[1,] + x.pred[2,] + rep(seasonal, ncol(data))
  
  return (y.pred)
}
