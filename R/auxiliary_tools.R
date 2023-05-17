# This is a library of auxiliary functions, which might be useful for other exported functions of this package.
# They should be invisible to package users

# Define a Univariate State-Space Model
spec_volume_model <- function(fixed_pars = NULL, init_pars = NULL) {
  volume_model <- list()
  class(volume_model) <- "volume_model"

  # error control
  if (!is.null(init_pars) && !is.list(init_pars)) stop("init_pars must be a list.")
  if (!is.null(fixed_pars) && !is.list(fixed_pars)) stop("fixed_pars must be a list.")

  # volume_model class properties
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  volume_model$par$"a_eta" <- NA
  volume_model$par$"a_mu" <- NA
  volume_model$par$"var_eta" <- NA
  volume_model$par$"var_mu" <- NA
  volume_model$par$"r" <- NA
  volume_model$par$"phi" <- NA
  volume_model$par$"x0" <- rep(NA, 2)
  volume_model$par$"V0" <- rep(NA, 3)
  volume_model$init <- list()

  # read in input parameters
  fixed_clean_result <- clean_pars_list(fixed_pars)
  fixed_pars <- fixed_clean_result$input_list

  unecessary_init <- intersect(names(init_pars), names(fixed_pars))
  init_pars <- init_pars[setdiff(names(init_pars), names(fixed_pars))]
  init_clean_result <- clean_pars_list(init_pars)
  init_pars <- init_clean_result$input_list

  # generate warning message
  msg <- c()
  if (length(fixed_clean_result$msg) > 0) {
    msg <- append(msg, "Warnings in fixed_pars:\n")
    # cat("Warnings in fixed_pars:\n")
    for (m in fixed_clean_result$msg) {
      msg <- append(msg, paste("  ", m, "\n", sep = ""))
      # cat("  ", m, "\n", sep = "")
    }
  }
  if (length(init_clean_result$msg) > 0 | length(unecessary_init) > 0) {
    msg <- append(msg, "Warnings in init_pars:\n")
    # cat("Warnings in init_pars:\n")
    if (!is.null(init_clean_result$msg)) {
      for (m in init_clean_result$msg) {
        msg <- append(msg, paste("  ", m, "\n", sep = ""))
        # cat("  ", m, "\n", sep = "")
      }
    }
    if (length(unecessary_init) > 0) {
      msg <- append(msg, paste("  Elements ", paste(unecessary_init, collapse = ", "),
        " have already been fixed.\n",
        sep = ""
      ))
      # cat("  Elements ", paste(unecessary_init, collapse = ", "),
      #     " have already been fixed.\n", sep = "")
    }
  }
  if (length(msg) > 0) {
    warning(msg)
  }
  # if (length(fixed_clean_result$msg) > 0) {
  #   cat("Warnings in fixed_pars:\n")
  #   for (m in fixed_clean_result$msg) {
  #     cat("  ", m, "\n", sep = "")
  #   }
  # }
  # if (length(init_clean_result$msg) > 0 | length(unecessary_init) > 0) {
  #   cat("Warnings in init_pars:\n")
  #   if (!is.null(init_clean_result$msg)) {
  #     for (m in init_clean_result$msg) {
  #       cat("  ", m, "\n", sep = "")
  #     }
  #   }
  #   if (length(unecessary_init) > 0) {
  #     cat("  Elements ", paste(unecessary_init, collapse = ", "),
  #         " have already been fixed.\n", sep = "")
  #   }
  # }

  # store inputs in univariate model object
  for (name in all_pars_name) {
    if (name %in% names(fixed_pars)) {
      volume_model$par[[name]] <- fixed_pars[[name]]
    } else if (name %in% names(init_pars)) {
      volume_model$init[[name]] <- init_pars[[name]]
    }
  }

  volume_model$converged <- list()
  for (name in all_pars_name) {
    if (anyNA(volume_model$par[[name]])) {
      volume_model$converged[[name]] <- FALSE
    } else {
      volume_model$converged[[name]] <- TRUE
    }
  }

  return(volume_model)
}

# Remove anyday containing NA/missing bins
# unify the data to matrix
clean_data <- function(data) {
  if (is.xts(data)) {
    data <- intraday_xts_to_matrix(data)
  } else {
    index_NA_bin <- colnames(data)[apply(data, 2, anyNA)]
    data <- data[, !apply(data, 2, anyNA)]
    if (length(index_NA_bin) > 0) {
      msg <- paste("For input matrix:\n", " Remove trading days with missing bins: ", toString(index_NA_bin), ".\n", sep = "")
      warning(msg)
      # cat("Warning in input matrix:\n")
      # cat(" Remove trading days with missing bins: ", format(index_NA_bin), "\n")
    }
  }
  return(data)
}

# Process xts data
# remove any day containing missing bins/NA
# convert the data to matrix
intraday_xts_to_matrix <- function(data_xts) {
  contain_NA <- xts::apply.daily(data_xts, function(x) as.integer(any(is.na(x))))
  index_no_NA_bin <- zoo::index(to.daily(contain_NA[contain_NA == 0]))
  data_xts <- data_xts[format(index_no_NA_bin, format = "%Y-%m-%d")]

  bins_count <- apply.daily(data_xts, nrow)
  n_bin <- max(bins_count)
  index_full_bin <- zoo::index(to.daily(bins_count[bins_count == n_bin]))

  data_xts <- data_xts[format(index_no_NA_bin, format = "%Y-%m-%d")]
  data_xts <- data_xts[format(index_full_bin, format = "%Y-%m-%d")]
  data.mat <- matrix(data_xts, nrow = n_bin)

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
    msg <- paste("For input xts:\n", " Remove trading days with missing bins: ", toString(sort(format(wrong_index))), ".\n", sep = "")
    warning(msg)
    # cat("Warning in input xts:\n")
    # cat(" Remove trading days with missing bins: ", sort(format(wrong_index)), "\n")
  }

  return(data.mat)
}


# clean the univolume_model()'s input args (init_pars/fixed_pars)
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

    switch(name,
      "V0" = {
        V_matrix <- matrix(input_list[["V0"]], 2)
        eigen_value <- eigen(matrix(input_list[["V0"]], 2), only.values = TRUE)$values >= 0
        if (!isSymmetric(V_matrix) || !eigen_value[1] || !eigen_value[2]) {
          input_list[[name]] <- NULL
          incorrect_param <- c(incorrect_param, name)
        } else {
          input_list[["V0"]] <- input_list[["V0"]][c(1, 2, 4)]
        }
      },
      "r" = {
        if (input_list[["r"]] < 0) {
          input_list[["r"]] <- NULL
          incorrect_param <- c(incorrect_param, "r")
        }
      }
    )
  }

  if (length(invalid_param) > 0) {
    msg <- c(msg, paste("Elements ", paste(invalid_param, collapse = ", "),
      " are not allowed in parameter list.",
      sep = ""
    ))
  }
  if (length(incorrect_param) > 0) {
    msg <- c(msg, paste("Elements ", paste(unique(incorrect_param), collapse = ", "),
      " are invalid (check number/dimension/PSD).",
      sep = ""
    ))
  }
  clean_result <- list(
    "input_list" = input_list,
    "msg" = msg
  )

  return(clean_result)
}

# part of error check for init_pars/fixed_pars
check_pars_list <- function(volume_model, n_bin = NULL) {
  converged_list <- volume_model$converged
  par_list <- volume_model$par
  init_list <- volume_model$init

  all_par_list <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "x0", "V0")
  scalar_par_list <- c("a_eta", "a_mu", "var_eta", "var_mu", "r")
  len_expect <- list("a_eta" = 1L, "a_mu" = 1L, "var_eta" = 1L, "var_mu" = 1L, "r" = 1L, "x0" = 2L, "V0" = 3L)
  if (!identical(n_bin, NULL)) {
    all_par_list <- append(all_par_list, "phi")
    len_expect <- append(len_expect, list("phi" = as.integer(n_bin)))
  }
  unfixed <- intersect(names(converged_list[converged_list == FALSE]), all_par_list)
  fixed <- intersect(names(converged_list[converged_list == TRUE]), all_par_list)


  msg <- NULL
  for (name in fixed) {
    if (mode(par_list[[name]]) != "numeric" || any(is.na(par_list[[name]])) || any(is.infinite(par_list[[name]]))) {
      msg <- c(msg, paste(name, "must be numeric, have no NAs, and no Infs.\n"))
    }
    if (name %in% scalar_par_list && !identical(len_expect[[name]], length(par_list[[name]]))) {
      msg <- c(msg, paste("Length of volume_model$par$", name, " is wrong.\n", sep = ""))
    }
  }
  for (name in unfixed) {
    if (!all(is.na(par_list[[name]]))) {
      msg <- c(msg, paste("volume_model$par$", name, " and volume_model$converged$", name, " are conflicted.\n", sep = ""))
    }
  }


  for (name in fixed) {
    if (name %in% names(init_list)) {
      msg <- c(msg, paste(name, "is fixed. No need for init.\n"))
    }
  }
  unfixed_init <- intersect(unfixed, names(init_list))
  for (name in unfixed_init) {
    if (mode(init_list[[name]]) != "numeric" || any(is.na(init_list[[name]])) || any(is.infinite(init_list[[name]]))) {
      msg <- c(msg, paste(name, "must be numeric, have no NAs, and no Infs.\n"))
    }
    if (name %in% scalar_par_list && !identical(len_expect[[name]], length(init_list[[name]]))) {
      msg <- c(msg, paste("Lenght of volume_model$init$", name, " is wrong.\n", sep = ""))
    }
  }
  return(msg)
}

# check whether the volume_model is correct
is_volume_model <- function(volume_model, n_bin = NULL) {
  ## Check for required components
  el <- c("converged", "par", "init")
  # if some components are missing from the volume_model, rest of the tests won't work so stop now
  if (!all(el %in% names(volume_model))) {
    stop("Elements ", paste(el[!(el %in% names(volume_model))], collapse = ", "), " are missing from the model.\n")
  }

  # if some args are missing from the volume_model's components, the code will stop when all missing parts are found.
  msg <- NULL
  all_pars_name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  if (!all(all_pars_name %in% names(volume_model$par))) {
    msg <- c(msg, "Elements ", paste(all_pars_name[!(all_pars_name %in% names(volume_model$par))], collapse = ", "), " are missing from volume_model$par.\n")
  }
  if (!all(all_pars_name %in% names(volume_model$converged))) {
    msg <- c(msg, "Elements ", paste(all_pars_name[!(all_pars_name %in% names(volume_model$converged))], collapse = ", "), " are missing from volume_model$converged.\n")
  }
  if (!is.null(msg)) { # rest of the tests won't work so stop now
    stop(msg)
  }

  # check converged
  logical_check <- lapply(volume_model$converged, function(f) isTRUE(f) | identical(f, FALSE))
  if (any(logical_check == FALSE)) {
    msg <- c("Elements in volume_model$converged must be TRUE/FALSE.\n")
    stop(msg)
  }

  # Check no NA inf and dimension
  msg <- check_pars_list(volume_model, n_bin)
  if (!is.null(msg)) {
    stop(msg)
  }
}

fetch_par_log <- function(par_log, index) {
  par_list <- list()
  for (i in 1:length(par_log)) {
    par_list <- append(par_list, par_log[[i]][[index]])
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
