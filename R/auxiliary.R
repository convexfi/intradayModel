# This is a library of auxiliary functions, which might be useful for other exported functions of this package.
# They should be invisible to package users

# transform the parameter from the format of MARSS to IntradayModel
trans_MARSStoIntra <- function(MARSS.par, intra.par = NULL) {
  all.pars.name.MARSS <- c("A", "R", "B", "Q", "x0", "V0")
  all.pars.name.intra <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  MARSS.par <- MARSS.par[all.pars.name.MARSS]
  for (name in c("B", "Q", "R")) {
    if (length(MARSS.par[[name]]) > 0) {
      intra.name <- dimnames(MARSS.par[[name]])[[1]]
      fix_par <- as.list(MARSS.par[[name]])
      names(fix_par) <- intra.name
      MARSS.par[[name]] <- NULL
      MARSS.par <- append(MARSS.par, fix_par)
    } else {
      MARSS.par[[name]] <- NULL
    }
  }

  if (length(MARSS.par[["A"]]) > 0) {
    MARSS.par[["phi"]] <- MARSS.par[["A"]]
  }
  # if (is.null(intra.par)){
  #   return(MARSS.par[all.pars.name.intra])
  # }
  # else{
  for (name in names(intra.par)) {
    if (anyNA(intra.par[[name]])) {
      intra.par[[name]] <- MARSS.par[[name]]
    }
  }
  dimnames(intra.par[["x0"]]) <- list(c("x01", "x02"), NULL)
  return(intra.par)
  # }
}

# define the MARSS model
specify_marss <- function(...) {
  # read input information
  args <- list(...)
  data <- args$data
  modelSpec <- args$modelSpec
  data.reform <- data %>%
    as.list() %>%
    unlist()
  
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  # MARSS parameters
  marss_model <- list()
  marss_model$model.gen <- list()
  marss_model$init.gen <- list()

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

  At <- array(list(0), dim = c(1, 1, n_bin_total))
  a_vec <- extract_value("phi", modelSpec)

  # need check
  if (identical(a_vec, "phi") || (length(a_vec) != n_bin)) {
    if (!identical(a_vec, "phi") && length(a_vec) != n_bin) warning("Dimensions of input data and pre-fixed phi aren't compatible.\n
                                       The values of fixed phi are ignored.")
    for (n in 1:n_bin) {
      a_vec[n] <- paste("phi", n, sep = "")
    }
  }
  At[1, 1, ] <- rep(a_vec, n_day)

  R <- extract_value("r", modelSpec) %>%
    list() %>%
    matrix(1, 1)

  ## Initial State
  x0 <- extract_value("x0", modelSpec) %>%
    as.list() %>%
    matrix(2, 1)

  # need check
  V0 <- extract_value("V0", modelSpec)
  if (!identical(V0, "unconstrained")) {
    V0 <- matrix(c(as.numeric(V0[1, 1]), as.numeric(V0[2, 1]), as.numeric(V0[2, 1]), as.numeric(V0[3, 1])), nrow = 2)
  }


  ## predefined init value
  init.default <- list(
    "x0" = matrix(c(10, 0), 2, 1),
    "a_eta" = 1, "a_mu" = 0.7,
    "r" = 0.08,
    "var_eta" = 0.07, "var_mu" = 0.06,
    "V0" = matrix(c(1e-3, 1e-7, 1e-5), 3, 1),
    "phi" = rowMeans(matrix(data.reform, nrow = n_bin)) - mean(data.reform)
  )
  ## Init param
  marss_model$init.gen <- extract_init(init.default, modelSpec$init, modelSpec$fit_request)

  ## MARSS model
  marss_model$model.gen <- list(Z = Z, R = R, A = At, B = Bt, Q = Qt, U = U, x0 = x0, V0 = V0, tinitx = 1)
  marss_obj <- MARSS::MARSS(data.reform, model = marss_model$model.gen, inits = marss_model$init.gen, fit = FALSE)

  return(marss_obj)
}

# extract fixed pars value from modelSpec for MARSS
extract_value <- function(name, modelSpec) {
  if (modelSpec$fit_request[[name]]) {
    name <- switch(name,
      "x0" = list("x01", "x02"),
      "V0" = "unconstrained",
      name
    )
    return(name)
  } else {
    return(modelSpec$par[[name]])
  }
}

# extract init pars value from modelSpec for MARSS
extract_init <- function(init.default, init.pars, fit_request) {
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  for (name in all.pars.name) {
    if (!fit_request[[name]]) {
      init.default[[name]] <- NULL
    }
    if (name %in% names(init.pars)) {
      init.default[[name]] <- init.pars[[name]]
    }
  }
  init.marss <- list()
  var.init <- c()
  a.init <- c()
  for (name in names(init.default)) {
    tmp <- switch(name,
      "phi" = {
        init.marss$A <- init.default$phi
      },
      "V0" = {
        init.marss$V0 <- init.default$V0
      },
      "x0" = {
        init.marss$x0 <- init.default$x0
      },
      "r" = {
        init.marss$R <- init.default$r
      },
      "a_eta" = {
        a.init <- append(a.init, init.default$a_eta)
      },
      "a_mu" = {
        a.init <- append(a.init, init.default$a_mu)
      },
      "var_eta" = {
        var.init <- append(var.init, init.default$var_eta)
      },
      "var_mu" = {
        var.init <- append(var.init, init.default$var_mu)
      }
    )
  }
  if (length(a.init) > 0) {
    init.marss$B <- matrix(a.init, length(a.init), 1)
  }
  if (length(var.init) > 0) {
    init.marss$Q <- matrix(var.init, length(var.init), 1)
  }
  return(init.marss)
}

# unify the output of uniModelSpec().
# modify the parameter dimension and add names to parameter
unifyModelFormat <- function(modelSpec) {
  # prepare phi's names
  phi_names <- c()
  for (i in 1:length(modelSpec$par[["phi"]])) {
    phi_names <- append(phi_names, paste(paste("phi", i, sep = "")))
  }

  # assign names to parameters whose dimension > 1
  for (name in c("x0", "V0", "phi")) {
    var_dim <- var_name <- NULL
    switch(name,
      "x0" = {
        var_dim <- c(2, 1)
        var_name <- list(c("x01", "x02"), NULL)
      },
      "V0" = {
        var_dim <- c(3, 1)
        var_name <- list(c("(1,1)", "(2,1)", "(2,2)"), NULL)
      },
      "phi" = {
        var_dim <- c(length(modelSpec$par[["phi"]]), 1)
        var_name <- list(phi_names, NULL)
      }
    )
    if (!modelSpec$fit_request[[name]]) {
      modelSpec$par[[name]] <- array(modelSpec$par[[name]], dim = var_dim, dimnames = var_name)
    } else if (name %in% names(modelSpec$init)) {
      modelSpec$init[[name]] <- array(modelSpec$init[[name]], dim = var_dim, dimnames = var_name)
    }
  }

  return(modelSpec)
}

# clean the uniModelSpec()'s input args (init.pars/fixed.pars)
# remove any variable containing NA/inf/non-numeric
# remove any variable that won't appear in model
# flatten the variable if user input a high dimension one
cleanParsList <- function(input_list) {
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "x0", "V0", "phi")
  expected_pars_len <- list(
    "a_eta" = 1, "a_mu" = 1,
    "var_eta" = 1, "var_mu" = 1, "r" = 1,
    "x0" = 2, "V0" = 3
  )

  # check if parameters are valid
  for (name in names(input_list)) {
    if (!(name %in% all_pars_name)) {
      input_list[[name]] <- NULL
      next
    }
    input_list[[name]] <- unlist(as.list(input_list[[name]]))

    if (mode(input_list[[name]]) != "numeric" ||
      any(is.na(input_list[[name]])) ||
      any(is.infinite(input_list[[name]]))) {
      input_list[[name]] <- NULL
    }

    if (name == "phi") next

    if (expected_pars_len[[name]] != length(input_list[[name]])) {
      input_list[[name]] <- NULL
    }
  }
  return(input_list)
}


# part of error check for init.pars/fixed.pars
checkList <- function(check.list, type) {
  pars.name.1 <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "x0", "V0")
  len.expect <- list("a_eta" = 1, "a_mu" = 1, "var_eta" = 1, "var_mu" = 1, "r" = 1, "x0" = 2, "V0" = 3)
  # no more
  for (name in pars.name.1) {
    if (!(name %in% names(check.list))) next
    # maybe no need
    if (!(is.list(check.list$name) || is.numeric(check.list$name))) {
      stop("no")
    }
    if (mode(check.list[[name]]) != "numeric" || any(is.na(check.list[[name]])) || any(is.infinite(check.list[[name]]))) {
      stop("no")
    }
    if (!identical(len.expect$name, length(check.list$name))) {
      stop("no")
    }
  }

  if ("phi" %in% names(check.list)) {
    if (!is.list(check.list$phi)) {
      stop("no")
    }
    if (mode(check.list[["phi"]]) != "numeric" || any(is.na(check.list[["phi"]])) || any(is.infinite(check.list[["phi"]]))) {
      stop("no")
    }
  }
}

# check whether the modelSpec is correct
isIntraModel <- function(modelSpec, data = NULL) {
  # msg <- NULL
  ## Check for required components
  el <- c("fit_request", "par", "init")
  if (!all(el %in% names(modelSpec))) {
    stop("Element ", paste(el[!(el %in% names(modelSpec))], collapse = " & "), " is missing from the model object.\n")
  }

  msg <- NULL
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  if (!all(all.pars.name %in% names(modelSpec$par))) {
    msg <- c(msg, "Element ", paste(all.pars.name[!(all.pars.name %in% names(modelSpec$par))], collapse = " & "), " is missing from the model$par.\n")
  }
  if (!all(all.pars.name %in% names(modelSpec$fit_request))) {
    msg <- c(msg, "Element ", paste(all.pars.name[!(all.pars.name %in% names(modelSpec$fit_request))], collapse = " & "), " is missing from the model$fit_request.\n")
  }
  if (!is.null(msg)) { # rest of the tests won't work so stop now
    stop(msg)
  }

  # Check no additional names in fit_request, par, init
  for (mat in el) {
    if (!all(names(modelSpec[[mat]]) %in% all.pars.name)) {
      msg <- c(msg, "Element\n")
    }
  }

  # Check no NA inf and dimension check
  checkList(modelSpec$par)
  checkList(modelSpec$init)

  unifxed <- names(modelSpec$fit_request[modelSpec$fit_request == TRUE])
  fixed <- names(modelSpec$fit_request[modelSpec$fit_request == FALSE])
  for (name in fixed) {
    if (any(is.na(modelSpec[["par"]][[name]]))) {
      stop("no")
    }
  }

  for (name in unfixed) {
    if (!all(is.na(modelSpec[["par"]][[name]]))) {
      stop("no")
    }
  }

  if (!is.null(data)) {
    n_bin <- nrow(data)
    # dim
    if (!modelSpec$fit_request[["phi"]] && !identical(length(modelSpec$par[["phi"]]), n_bin)) stop("no")
    # if(modelSpec$fit_request[["phi"]] && (!identical(length(modelSpec$init[["phi"]]), n_bin))) stop("no")
  }
}

fetch_par_log <- function(par_log, index) {
  par_list <- list()
  for (i in 1:length(par_log)) {
    par_list <- rlist::list.append(par_list, par_log[[i]][[index]])
  }
  return(do.call(cbind, par_list))
}

calculate_mape <- function(referenced_data, predicted_data) {
  referenced_data <- as.vector(referenced_data)
  predicted_data <- as.vector(predicted_data)
  return(mean(abs(predicted_data - referenced_data) / referenced_data))
}

calculate_mae <- function(referenced_data, predicted_data) {
  referenced_data <- as.vector(referenced_data)
  predicted_data <- as.vector(predicted_data)
  return(mean(abs(predicted_data - referenced_data)))
}

calculate_rmse <- function(referenced_data, predicted_data) {
  referenced_data <- as.vector(referenced_data)
  predicted_data <- as.vector(predicted_data)
  return(sqrt(mean((predicted_data - referenced_data)^2)))
}
