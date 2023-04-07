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

  # modelSpec class properties
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  modelSpec$par$"a_eta" <- NA
  modelSpec$par$"a_mu" <- NA
  modelSpec$par$"var_eta" <- NA
  modelSpec$par$"var_mu" <- NA
  modelSpec$par$"r" <- NA
  modelSpec$par$"phi" <- NA
  modelSpec$par$"x0" <- matrix(NA, 2)
  modelSpec$par$"V0" <- matrix(NA, 3)
  modelSpec$init <- list()

  # read in input parameters
  fixed.pars <- cleanParsList(fixed.pars)
  init.pars <- cleanParsList(init.pars)
  for (name in all_pars_name) {
    if (name %in% names(fixed.pars)) {
      modelSpec$par[[name]] <- fixed.pars[[name]]
    } else if (name %in% names(init.pars)) {
      modelSpec$init[[name]] <- init.pars[[name]]
    }
  }

  # decide if each variable requires fitting
  if (fit == FALSE) {
    if (anyNA(unlist(modelSpec$par))) {
      stop("Wrong input: unfitted model contains unknown parameters \n")
      break
    }
  }
  modelSpec$fit_request <- list()
  for (name in all_pars_name) {
    if (anyNA(modelSpec$par[[name]])) {
      modelSpec$fit_request[[name]] <- TRUE
    } else {
      modelSpec$fit_request[[name]] <- FALSE
    }
  }

  # unify the modelSpec parameters format
  modelSpec <- unifyModelFormat(modelSpec)

  return(modelSpec)
}
