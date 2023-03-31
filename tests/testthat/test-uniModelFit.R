test_that("uniModelFit params conincide with precomputed model", {
  data("data_log_volume")
  load("./tests/testthat/params.rda")
  precomputed_par <- params
  
  modelSpec <- uniModelSpec(fit = TRUE)
  
  data <- data_log_volume
  data <- as.matrix(data)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  control = list(maxit = 3000, abstol = 1e-4, log.switch = TRUE)
  
  args <- list(data = data, n_bin = n_bin,
               n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec,
               control = control)
  
  
  result <- do.call(MARSS_spec, args = args)
  kalman <- result$kalman
  At <- result$At
  
  kalman$par <- kalman$start
  args <- append(args, list(kalman = kalman, At = At))
  
  EM_result_test <- do.call(EM_param, args = args)
  params_p <- EM_result_test$model$par
  expect_equal(EM_result_test$model$par, precomputed_par, tolerance = 1e-3)
})

test_that("uniModelFit with partial fixed params conincide with precomputed model", {
  data("data_log_volume")
  load("./tests/testthat/params.rda")
  precomputed_par <- params
  
  data <- data_log_volume
  data <- as.matrix(data)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  fixed_phi <- precomputed_par$A
  
  fixed.pars <- list("x0" = matrix(list(10, 0), 2, 1), "phi" = fixed_phi)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  
  control = list(maxit = 3000, abstol = 1e-4, log.switch = TRUE)
  
  args <- list(data = data, n_bin = n_bin,
               n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec,
               control = control)
  
  
  result <- do.call(MARSS_spec, args = args)
  kalman <- result$kalman
  At <- result$At
  
  kalman$par <- kalman$start
  args <- append(args, list(kalman = kalman, At = At))
  
  EM_result_test <- do.call(EM_param, args = args)
  
  
  expect_equal(EM_result_test$model$par[c("R","B","Q","V0")], precomputed_par[c("R","B","Q","V0")], tolerance = 1e-1)
})
