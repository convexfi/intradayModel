test_that("uniModelFit params conincide with precomputed model", {
  data("data_log_volume")
  load("./tests/testthat/EM_result.rda")
  precomputed_par <- EM_result
  
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
  
  
  expect_equal(EM_result_test$model$par, precomputed_par, tolerance = 1e-3)
})
