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
  uniModel <- list()

  # error control
  if (!is.null(init.pars) && !is.list(init.pars)) stop("init.pars must be a list.")
  if (!is.null(fixed.pars) && !is.list(fixed.pars)) stop("fixed.pars must be a list.")

  # uniModel class properties
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  uniModel$par$"a_eta" <- NA
  uniModel$par$"a_mu" <- NA
  uniModel$par$"var_eta" <- NA
  uniModel$par$"var_mu" <- NA
  uniModel$par$"r" <- NA
  uniModel$par$"phi" <- NA
  uniModel$par$"x0" <- matrix(NA, 2)
  uniModel$par$"V0" <- matrix(NA, 3)
  uniModel$init <- list()

  # read in input parameters
  fixed.pars <- cleanParsList(fixed.pars)
  init.pars <- cleanParsList(init.pars)
  for (name in all_pars_name) {
    if (name %in% names(fixed.pars)) {
      uniModel$par[[name]] <- fixed.pars[[name]]
    } else if (name %in% names(init.pars)) {
      uniModel$init[[name]] <- init.pars[[name]]
    }
  }

  # decide if each variable requires fitting
  if (fit == FALSE) {
    if (anyNA(unlist(uniModel$par))) {
      stop("Wrong input: unfitted model contains unknown parameters \n")
      break
    }
  }
  uniModel$fit_request <- list()
  for (name in all_pars_name) {
    if (anyNA(uniModel$par[[name]])) {
      uniModel$fit_request[[name]] <- TRUE
    } else {
      uniModel$fit_request[[name]] <- FALSE
    }
  }

  # unify the uniModel parameters format
  uniModel <- format_unimodel(uniModel)

  return(uniModel)
}
