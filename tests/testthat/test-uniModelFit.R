test_that("fit_volume from raw (after zero constraint and initial noise), stock = ADBE", {
  skip_on_cran()
  data <- readRDS(test_path("fixtures", "ADBE_volume"))[,1:104]
  modelSpec.fit <- fit_volume(data, control = list(maxit = 1000, abstol = 1e-4, log_switch = TRUE))
  modelSpec.fit_acc <- fit_volume(data, control = list(maxit = 1000, abstol = 1e-4, log_switch = TRUE, acceleration = TRUE))
  
  # expected output
  expected_par <- readRDS(test_path("fixtures", "ADBE_expected_par"))
  expected_modelSpec <- list()
  expected_modelSpec$par$a_eta <- expected_par$B[1]
  expected_modelSpec$par$a_mu <- expected_par$B[2]
  expected_modelSpec$par$var_eta <- expected_par$Q[1]
  expected_modelSpec$par$var_mu <- expected_par$Q[2]
  expected_modelSpec$par$r <- expected_par$R[1]
  expected_modelSpec$par$phi <- as.vector(expected_par$A)
  expected_modelSpec$par$x0 <- as.vector(expected_par$x0)
  expected_modelSpec$par$V0 <- as.vector(expected_par$V0)
  
  compared_par <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi")
  expect_equal(modelSpec.fit$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
  expect_equal(modelSpec.fit_acc$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)

  plot(fetch_par_log(modelSpec.fit$par_log, "a_eta")[1, ])
  plot(fetch_par_log(modelSpec.fit_acc$par_log, "a_eta")[1, ])
})

test_that("fit_volume from raw (after zero constraint and initial noise), stock = ACN", {
  skip_on_cran()
  data <- readRDS(test_path("fixtures", "ACN_volume"))[,1:104]
  modelSpec.fit <- fit_volume(data, control = list(maxit = 1000, abstol = 1e-4, log_switch = TRUE))
  modelSpec.fit_acc <- fit_volume(data, control = list(maxit = 1000, abstol = 1e-4, log_switch = TRUE, acceleration = TRUE))
  
  # expected output
  expected_par <- readRDS(test_path("fixtures", "ACN_expected_par"))
  expected_modelSpec <- list()
  expected_modelSpec$par$a_eta <- expected_par$B[1]
  expected_modelSpec$par$a_mu <- expected_par$B[2]
  expected_modelSpec$par$var_eta <- expected_par$Q[1]
  expected_modelSpec$par$var_mu <- expected_par$Q[2]
  expected_modelSpec$par$r <- expected_par$R[1]
  
  expected_modelSpec$par$phi <- as.vector(expected_par$A)
  expected_modelSpec$par$x0 <- as.vector(expected_par$x0)
  expected_modelSpec$par$V0 <- as.vector(expected_par$V0)
  
  compared_par <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi")
  expect_equal(modelSpec.fit$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
  expect_equal(modelSpec.fit_acc$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
  
  plot(fetch_par_log(modelSpec.fit$par_log, "a_eta")[1, ])
  plot(fetch_par_log(modelSpec.fit_acc$par_log, "a_eta")[1, ])
})

test_that("fit_volume from raw (after zero constraint and initial noise), stock = CVS", {
  skip_on_cran()
  data <- readRDS(test_path("fixtures", "CVS_volume"))[, 1:104]
  modelSpec.fit <- fit_volume(data, control = list(maxit = 1000, abstol = 1e-4, log_switch = TRUE))
  modelSpec.fit_acc <- fit_volume(data, control = list(maxit = 1000, abstol = 1e-4, log_switch = TRUE, acceleration = TRUE))
  
  # expected output
  expected_par <- readRDS(test_path("fixtures", "CVS_expected_par"))
  expected_modelSpec <- list()
  expected_modelSpec$par$a_eta <- expected_par$B[1]
  expected_modelSpec$par$a_mu <- expected_par$B[2]
  expected_modelSpec$par$var_eta <- expected_par$Q[1]
  expected_modelSpec$par$var_mu <- expected_par$Q[2]
  expected_modelSpec$par$r <- expected_par$R[1]
  expected_modelSpec$par$phi <- as.vector(expected_par$A)
  expected_modelSpec$par$x0 <- as.vector(expected_par$x0)
  expected_modelSpec$par$V0 <- as.vector(expected_par$V0)
  
  compared_par <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi")
  expect_equal(modelSpec.fit$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
  expect_equal(modelSpec.fit_acc$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
   
  plot(fetch_par_log(modelSpec.fit$par_log, "a_mu")[1, ])
  plot(fetch_par_log(modelSpec.fit_acc$par_log, "a_mu")[1, ])
})
