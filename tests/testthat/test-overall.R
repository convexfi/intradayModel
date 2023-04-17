test_that("package, stock = GE", {
  data(GE_volume)
  modelSpec <- uniModelSpec(fit = TRUE)

  data <- GE_volume
  data_train <- GE_volume[, 1:104]
  modelSpec.fit <- uniModelFit(data_train, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  modelSpec.fit_acc <- uniModelFit(data_train, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)

  # Fitting
  expected_par <- readRDS("./tests/testthat/GE_expected_par")
  expected_modelSpec <- list()
  expected_modelSpec$par$a_eta <- expected_par$B[1]
  expected_modelSpec$par$a_mu <- expected_par$B[2]
  expected_modelSpec$par$var_eta <- expected_par$Q[1]
  expected_modelSpec$par$var_mu <- expected_par$Q[2]
  expected_modelSpec$par$r <- expected_par$R[1]

  phi_names <- c()
  for (i in 1:26){
    phi_names <- append(phi_names, paste(paste("phi", i, sep = "")))
  }
  expected_modelSpec$par$phi <- array(expected_par$A, dim = c(26, 1), dimnames = list(phi_names, NULL))
  expected_modelSpec$par$x0 <- array(expected_par$x0, dim = c(2, 1), dimnames = list(c("x01", "x02"), NULL))
  expected_modelSpec$par$V0 <- expected_par$V0

  compared_par <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi")
  expect_equal(modelSpec.fit$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
  expect_equal(modelSpec.fit_acc$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)


  # Filtering
  filter_result <- uniModelFilter(data_train, modelSpec.fit)
  filter_result_acc <- uniModelFilter(data_train, modelSpec.fit_acc)
  
  filter_result$plot
  filter_result_acc$plot
  
  
  # Prediction
  predict_result <- uniModelPred(data, modelSpec.fit, out.sample = 20)
  predict_result_acc <- uniModelPred(data, modelSpec.fit_acc, out.sample = 20)
  
  expected_res <- readRDS("./tests/testthat/GE_expected_pred")
  expect_equal(predict_result$signal_pred, expected_res$volume_pred, tolerance = 1e-2)
  expect_equal(predict_result_acc$signal_pred, expected_res$volume_pred, tolerance = 1e-2)
  expect_equal(as.vector(as.matrix(predict_result$measure)), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-2)
  expect_equal(as.vector(as.matrix(predict_result_acc$measure)), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-2)
  
})



test_that("messages, stock = GE", {
  data(GE_volume)

  # Spec
  expect_error(uniModelSpec(fit = FALSE), regexp = "If fit = FALSE, a_eta, a_mu, var_eta, var_mu, r, phi, x0, V0 must have no NAs.")
  error_message <- paste("yyy is not model parameter. Thus its input value is ignored.\n",
                          "a_eta has wrong input value. Thus its input value is ignored.\n",
                         "Check whether the input parameter is number with no NA, Inf and right length.\n", sep = "")
  expect_warning(uniModelSpec(fit = TRUE, fixed.pars = list("yyy" = 1, "a_eta" = "a")), regexp = error_message)

  # Fitting
  modelSpec <- uniModelSpec(fit = TRUE)
  data <- GE_volume
  data_train <- GE_volume[, 1:104]
  data_error_test <- data_train
  data_error_test[1,1] <- NA

  expect_warning(uniModelFit(data_train, modelSpec, maxit = 1), regexp = "Warning! Reached maxit before parameters converged. Maxit was 1.\n")
  expect_output(uniModelFit(data_train, modelSpec, maxit = 1000, acceleration = TRUE, verbose = 0), regexp = "Success! abstol test passed at 22 iterations.")
  expect_error(uniModelFit(c(1,1), modelSpec), regexp = "data must be a matrix.")
  expect_error(uniModelFit(data_error_test, modelSpec), regexp = "data must have no NA.")

  modelSpec.fit_acc <- uniModelFit(data_train, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE, verbose = 0)

  expect_output(uniModelFit(data_train, modelSpec.fit_acc), "All parameters are already fixed.")

  n_bin <- 26
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec_check <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)

  ## missing component
  modelSpec_check1 <- modelSpec_check[c("par", "init")]
  expect_error(uniModelFit(data_train, modelSpec_check1), "Element fit_request is missing from the uniModel object.\n")

  ## missing element
  modelSpec_check2 <- modelSpec_check
  modelSpec_check2$par[["x0"]] <- NULL
  expect_error(uniModelFit(data_train, modelSpec_check2),"Element x0 is missing from the uniModel[$]par.\n")

  ## fixed paramenter with NA
  modelSpec_check3 <- modelSpec_check
  modelSpec_check3$fit_request[["a_eta"]] <- FALSE
  expect_error(uniModelFit(data_train, modelSpec_check3), "a_eta must be numeric, have no NAs, and no Infs.\n")

  ## wrong dimension/lenghth
  modelSpec_check4 <- modelSpec_check
  modelSpec_check4$par[["x0"]] <- 1
  modelSpec_check4$par[["var_eta"]] <- array(c(1,2))
  modelSpec_check4$par[["phi"]] <- matrix(2, 25)
  error_message <- paste("Dimension of uniModel[$]par[$]var_eta is wrong.\n","Length of uniModel[$]par[$]var_eta is wrong.\n",
                         "Dimension of uniModel[$]par[$]phi is wrong.\n","Dimension of uniModel[$]par[$]x0 is wrong.\n", sep = "")
  expect_error(uniModelFit(data_train, modelSpec_check4), error_message)

  # Filtering
  expect_error(uniModelFilter(data, modelSpec), regexp = "All parameters must be fitted.\n Parameter a_eta, a_mu, var_eta, var_mu, r, phi, x0, V0 is not fitted.")

  # Prediction
  expect_error(uniModelPred(data, modelSpec.fit_acc, 300), regexp = "out.sample must be smaller than the number of columns in data matrix.")
})
