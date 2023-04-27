# This is a library of auxiliary functions, which might be useful for other exported functions of this package.
# They should be invisible to package users

# Define a Univariate State-Space Model
spec_unimodel <- function(fixed.pars = NULL, init.pars = NULL) {
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
  uniModel$par$"x0" <- rep(NA, 2)
  uniModel$par$"V0" <- rep(NA, 3)
  uniModel$init <- list()
  
  # read in input parameters
  fixed_clean_result <- clean_pars_list(fixed.pars)
  fixed.pars <- fixed_clean_result$input_list
  
  unecessary_init <- intersect(names(init.pars), names(fixed.pars))
  init.pars <- init.pars[setdiff(names(init.pars), names(fixed.pars))]
  init_clean_result <- clean_pars_list(init.pars)
  init.pars <- init_clean_result$input_list
  
  # generate warning message
  if (length(fixed_clean_result$msg) > 0) {
    cat("Warnings in fixed.pars:\n")
    for (m in fixed_clean_result$msg) {
      cat("  ", m, "\n", sep = "")
    }
  }
  if (length(init_clean_result$msg) > 0 | length(unecessary_init) > 0) {
    cat("Warnings in init.pars:\n")
    if (!is.null(init_clean_result$msg)) {
      for (m in init_clean_result$msg) {
        cat("  ", m, "\n", sep = "")
      }
    }
    if (length(unecessary_init) > 0) {
      cat("  Elements ", paste(unecessary_init, collapse = ", "),
          " have already been fixed.\n", sep = "")
    }
  }
  
  # store inputs in univariate model object
  for (name in all_pars_name) {
    if (name %in% names(fixed.pars)) {
      uniModel$par[[name]] <- fixed.pars[[name]]
    } else if (name %in% names(init.pars)) {
      uniModel$init[[name]] <- init.pars[[name]]
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
  
  return(uniModel)
}

# Remove anyday containing NA/missing bins
# unify the data to matrix
clean_data <- function(data){
  if (is.xts(data)) {
    data <- intraday_xts_to_matrix(data)
  }
  else {
    index_NA_bin <- colnames(data)[apply(data, 2, anyNA)]
    data <- data[,!apply(data, 2, anyNA)]
    if (length(index_NA_bin) > 0) {
      cat("Warning in input matrix:\n")
      cat(" Remove trading days with missing bins: ", format(index_NA_bin), "\n")
    }
  }
  return (data)
}

# library(xts)
# Process xts data
# remove any day containing missing bins/NA
# convert the data to matrix
intraday_xts_to_matrix <- function(data.xts) {
  contain_NA <- apply.daily(data.xts, function(x) as.integer(any(is.na(x))))
  index_no_NA_bin <- zoo::index(to.daily(contain_NA[contain_NA == 0]))
  data.xts <- data.xts[format(index_no_NA_bin, format="%Y-%m-%d")]
  
  bins_count <- apply.daily(data.xts, nrow)
  n_bin <- max(bins_count)
  index_full_bin <- zoo::index(to.daily(bins_count[bins_count == n_bin]))
  
  data.xts <- data.xts[format(index_no_NA_bin, format="%Y-%m-%d")]
  data.xts <- data.xts[format(index_full_bin, format="%Y-%m-%d")]
  data.mat <- matrix(data.xts, nrow = n_bin)
  
  index_NA_bin <- c()
  if (sum(contain_NA != 0) > 0) {
    index_NA_bin <- zoo::index(to.daily(contain_NA[contain_NA != 0]))
  }
  index_notfull_bin <- c()
  if (sum(bins_count != n_bin) > 0) {
    index_notfull_bin <- zoo::index(to.daily(bins_count[bins_count != n_bin]))
  }
  wrong_index <- c(index_NA_bin, index_notfull_bin)
  if (length(wrong_index) > 0) {
    cat("Warning in input xts:\n")
    cat(" Remove trading days with missing bins: ", sort(format(wrong_index)), "\n")
  }
  
  return(data.mat)
}


# clean the uniuniModel()'s input args (init.pars/fixed.pars)
# remove any variable containing NA/inf/non-numeric
# remove any variable that won't appear in model
# flatten the variable if user input a high dimension one
clean_pars_list <- function(input_list) {
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "x0", "V0", "phi")
  expected_pars_len <- list(
    "a_eta" = 1, "a_mu" = 1,
    "var_eta" = 1, "var_mu" = 1, "r" = 1,
    "x0" = 2, "V0" = 4
  )

  invalid_param <- c()
  incorrect_param <- c()
  msg <- NULL
  
  # check if parameters are valid
  for (name in names(input_list)) {
    if (!(name %in% all_pars_name)) {
      input_list[[name]] <- NULL
      invalid_param <- c(invalid_param, name)
      next
    }
    input_list[[name]] <- unlist(as.list(input_list[[name]]))

    if (mode(input_list[[name]]) != "numeric" ||
      any(is.na(input_list[[name]])) ||
      any(is.infinite(input_list[[name]]))) {
      input_list[[name]] <- NULL
      incorrect_param <- c(incorrect_param, name)
      next
    }

    if (name == "phi") next

    if (expected_pars_len[[name]] != length(input_list[[name]])) {
      input_list[[name]] <- NULL
      incorrect_param <- c(incorrect_param, name)
      next
    }
    
    switch (name,
      "V0" = { V_matrix <-matrix(input_list[["V0"]],2)
               eigen_value <- eigen(matrix(input_list[["V0"]],2), only.values = TRUE)$values >= 0  
              if (!isSymmetric(V_matrix)|| !eigen_value[1]||!eigen_value[2]){
               input_list[[name]] <- NULL
               incorrect_param <- c(incorrect_param, name)}
              else{
               input_list[["V0"]] <- input_list[["V0"]][c(1,2,4)]}
                },
      "r" = {if(input_list[["r"]] < 0) {
             input_list[["r"]] <- NULL
             incorrect_param <- c(incorrect_param, "r")
      }}
    )
  }

  if (length(invalid_param) > 0){
    msg <- c(msg, paste("Elements ", paste(invalid_param, collapse = ", "),
                   " are not allowed in parameter list.", sep = ""))
  }
  if (length(incorrect_param) > 0){
    msg <- c(msg, paste("Elements ", paste(unique(incorrect_param), collapse = ", "),
                   " are invalid (check number/dimension/PSD).", sep = ""))
  }
  clean_result <- list("input_list" = input_list,
                       "msg" = msg)

  return(clean_result)
}

# part of error check for init.pars/fixed.pars
check_pars_list <- function(uniModel, n_bin = NULL) {
  fit_request_list <- uniModel$fit_request
  par_list <- uniModel$par
  init_list <- uniModel$init
  
  all_par_list <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "x0", "V0")
  scalar_par_list <- c("a_eta", "a_mu", "var_eta", "var_mu", "r")
  len_expect <- list("a_eta" = 1L, "a_mu" = 1L, "var_eta" = 1L, "var_mu" = 1L, "r" = 1L, "x0" = 2L, "V0" = 3L)
  if (!identical(n_bin, NULL)){
    all_par_list <- append(all_par_list, "phi")
    len_expect <- rlist::list.append(len_expect, "phi" = as.integer(n_bin))
  }
  unfixed <- intersect(names(fit_request_list[fit_request_list == TRUE]), all_par_list)
  fixed <- intersect(names(fit_request_list[fit_request_list == FALSE]), all_par_list)
  

  msg <- NULL
  for (name in fixed){
    if (mode(par_list[[name]]) != "numeric" || any(is.na(par_list[[name]])) || any(is.infinite(par_list[[name]]))) {
        msg <- c(msg, paste(name, "must be numeric, have no NAs, and no Infs.\n"))
      }
      if (name %in% scalar_par_list && !identical(len_expect[[name]], length(par_list[[name]]))) {
        msg <- c(msg, paste("Length of uniModel$par$", name, " is wrong.\n", sep = ""))
      }
    }
    for (name in unfixed){
      if (!all(is.na(par_list[[name]]))) {
        msg <- c(msg, paste("uniModel$par$", name, " and uniModel$fit_request$", name, " are conflicted.\n", sep = ""))
      }
    }
  
 
    for (name in fixed){
      if (name %in% names(init_list)){
        msg <- c(msg, paste(name, "is fixed. No need for init.\n"))
      }
    }
    unfixed_init <- intersect(unfixed, names(init_list))
    for (name in unfixed_init){
      if (mode(init_list[[name]]) != "numeric" || any(is.na(init_list[[name]])) || any(is.infinite(init_list[[name]]))) {
        msg <- c(msg, paste(name, "must be numeric, have no NAs, and no Infs.\n"))
      }
      if (name %in% scalar_par_list && !identical(len_expect[[name]], length(init_list[[name]]))) {
        msg <- c(msg, paste("Lenght of uniModel$init$", name, " is wrong.\n", sep = ""))
      }
    }
  return (msg)
  
}

# check whether the uniModel is correct
is_uniModel <- function(uniModel, n_bin = NULL) {
  ## Check for required components 
  el <- c("fit_request", "par", "init")
  # if some components are missing from the uniModel, rest of the tests won't work so stop now
  if (!all(el %in% names(uniModel))) {
    stop("Elements ", paste(el[!(el %in% names(uniModel))], collapse = ", "), " are missing from the model.\n")
  }
  
  # if some args are missing from the uniModel's components, the code will stop when all missing parts are found.
  msg <- NULL
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  if (!all(all_pars_name %in% names(uniModel$par))) {
    msg <- c(msg, "Elements ", paste(all_pars_name[!(all_pars_name %in% names(uniModel$par))], collapse = ", "), " are missing from uniModel$par.\n")
  }
  if (!all(all_pars_name %in% names(uniModel$fit_request))) {
    msg <- c(msg, "Elements ", paste(all_pars_name[!(all_pars_name %in% names(uniModel$fit_request))], collapse = ", "), " are missing from uniModel$fit_request.\n")
  }
  if (!is.null(msg)) { # rest of the tests won't work so stop now
    stop(msg)
  }

  # Check no additional names in fit_request, par, init
  # for (mat in el) {
  #   if (!all(names(uniModel[[mat]]) %in% all_pars_name)) {
  #     msg <- c(msg, "Element\n")
  #   }
  # }
  
  # check fit_request
  logical_check <- lapply(uniModel$fit_request, function (f) isTRUE(f) | identical(f, FALSE))
  if (any(logical_check == FALSE)) {
    msg <- c("Elements in uniModel$fit_request must be TRUE/FALSE.\n")
    stop(msg)
  }
  
  # Check no NA inf and dimension
  msg <- check_pars_list(uniModel, n_bin)
  if (!is.null(msg)) {
    stop(msg)
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
