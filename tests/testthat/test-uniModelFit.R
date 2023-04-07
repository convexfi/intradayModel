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

test_that("uniModelFit from raw (after zero constraint and initial noise), batch = 5", {
  data <- readRDS("data/batch5_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  
  # expected output
  expected_par <- readRDS("data/batch5_expected_par")
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

  plot(fetch_par_log(modelSpec.fit$par_log, "B")[1, ])
})

test_that("uniModelFit from raw (after zero constraint and initial noise), batch = 1", {
  data <- readRDS("data/batch1_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  
  # expected output
  expected_par <- readRDS("data/batch1_expected_par")
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
  
  plot(fetch_par_log(modelSpec.fit$par_log, "B")[1, ])
})

test_that("uniModelFit from raw (after zero constraint and initial noise), ACN", {
  data <- readRDS("data/ACN_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  
  # expected output
  expected_par <- readRDS("data/ACN_expected_par")
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
  
  plot(fetch_par_log(modelSpec.fit$par_log, "B")[1, ])
})
