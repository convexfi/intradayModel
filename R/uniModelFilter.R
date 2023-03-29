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
  if (!is.list(modelSpec)) stop("tbd.")
  # todo
  # if (Reduce("+", modelSpec$fitFlag) != 0) {
  #   stop("All parameters must be fixed.\n")
  # }
  
  # error control
  # if (!is.matrix(data.filter) && !is.data.frame(data.filter)) stop("data must be a matrix or data.frame.")
  # if (anyNA(data.filter) ||) stop("data must have no NA.")
  #
  
  
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