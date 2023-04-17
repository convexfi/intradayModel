test_that("uniModelFit from raw (after zero constraint and initial noise), stock = ADBE", {
  data <- readRDS("./tests/testthat/ADBE_volume")[,1:104]
  
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  modelSpec.fit_acc <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
  
  # expected output
  expected_par <- readRDS("./tests/testthat/ADBE_expected_par")
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

  plot(fetch_par_log(modelSpec.fit$par_log, "B")[1, ])
  plot(fetch_par_log(modelSpec.fit_acc$par_log, "B")[1, ])
})

test_that("uniModelFit from raw (after zero constraint and initial noise), stock = ACN", {
  data <- readRDS("./tests/testthat/ACN_volume")[,1:104]
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  modelSpec.fit_acc <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
  
  # expected output
  expected_par <- readRDS("./tests/testthat/ACN_expected_par")
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
  
  plot(fetch_par_log(modelSpec.fit$par_log, "B")[1, ])
  plot(fetch_par_log(modelSpec.fit_acc$par_log, "B")[1, ])
})

test_that("uniModelFit from raw (after zero constraint and initial noise), stock = CVS", {
  data <- exp(readRDS("./tests/testthat/CVS_log_volume"))[, 1:104]
  # data <- readRDS("./tests/testthat/CVS_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  modelSpec.fit_acc <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
    
  modelSpec_v2 <- uniModelSpec(fit = TRUE, init.pars = list(a_mu = 0))
  modelSpec.fit_acc2 <- uniModelFit(data, modelSpec_v2, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
  
  # expected output
  expected_par <- readRDS("./tests/testthat/CVS_expected_par")
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
  # expect_equal(modelSpec.fit_acc$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 1e-2)
  expect_equal(modelSpec.fit_acc2$par[compared_par], expected_modelSpec$par[compared_par], tolerance = 5e-2)
  
  plot(fetch_par_log(modelSpec.fit$par_log, "B")[2, ])
  plot(fetch_par_log(modelSpec.fit_acc$par_log, "B")[2, ])
  plot(fetch_par_log(modelSpec.fit_acc2$par_log, "B")[2, ])
})
