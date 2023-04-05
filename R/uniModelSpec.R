#' Title
#'
#' @param fit
#' @param init.pars
#' @param fixed.pars
#'
#' @return
#' @export
#'
#' @examples
uniModelSpec <- function(fit = FALSE, init.pars = NULL, fixed.pars = NULL) {
  modelSpec <- list()
  
  # error control
  if (!is.null(init.pars) && !is.list(init.pars)) stop("init.pars must be a list.")
  if (!is.null(fixed.pars) && !is.list(fixed.pars)) stop("fixed.pars must be a list.")
  
  # C-step: check the validity of inputs
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  
  modelSpec$par$"a_eta" <- NA
  modelSpec$par$"a_mu" <- NA
  modelSpec$par$"var_eta" <- NA
  modelSpec$par$"var_mu" <- NA
  modelSpec$par$"r" <- NA
  modelSpec$par$"phi" <- NA
  modelSpec$par$"x0" <- matrix(NA, 2)
  modelSpec$par$"V0" <- matrix(NA, 3)
  
  modelSpec$init <- list()# modelSpec$par
  
  fixed.pars <- transList(fixed.pars)
  # print(fixed.pars)
  init.pars <- transList(init.pars)
  for (name in all.pars.name) {
    if (name %in% names(fixed.pars)) {
      ## <requires dimension check>
      ## <requires no NA check>
      modelSpec$par[[name]] <- fixed.pars[[name]]
    }
    else if (name %in% names(init.pars)){
      modelSpec$init[[name]] <- init.pars[[name]]
    }
    
    # if (name %in% names(init.pars)) {
    
    #}
  }
  
  ## C-step: check fit = FALSE case
  if (fit == FALSE) {
    if (anyNA(unlist(modelSpec$par))) {
      stop("Wrong input: unfitted model contains unknown parameters \n")
      break
    }
  }
  
  ## Output
  modelSpec$fitFlag <- list()
  cat("fit = ", fit, "\n", sep = "")
  for (name in all.pars.name) {
    if (anyNA(modelSpec$par[[name]])) {
      modelSpec$fitFlag[[name]] <- TRUE
      # if (anyNA(modelSpec$init[[name]])) {
      #   cat(name, " is unfitted without initial value\n", sep = "")
      # } else{
      #   cat(name, " is unfitted with initial value: ", modelSpec$init[[name]], "\n", sep = "")
      # }
    } else {
      modelSpec$fitFlag[[name]] <- FALSE
      # cat(name, " is fixed at ", modelSpec$par[[name]], "\n", sep = "")
    }
  }
  
  
  modelSpec <- IntraFormat(modelSpec)
  return(modelSpec)
  
}
