test_that("uniModelPred, stock = ADBE", {
  data.pred <- readRDS("./tests/testthat/ADBE_volume")
  data <- data.pred[,1:104]

  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  modelSpec.fit_acc <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
  
  log_volume_pred <- log(uniModelPred(data.pred, modelSpec.fit, out.sample = 20)$signal_pred)
  log_volume_pred_acc <- log(uniModelPred(data.pred, modelSpec.fit_acc, out.sample = 20)$signal_pred)
  log_volume_real <- log(tail(as.vector(data.pred), 26 * 20))
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("./tests/testthat/ADBE_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(log_volume_pred_acc, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})

test_that("uniModelPred, ACN", {
  data.pred <- readRDS("./tests/testthat/ACN_volume")
  data <- data.pred[,1:104]
  
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  modelSpec.fit_acc <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
  
  log_volume_pred <- log(uniModelPred(data.pred, modelSpec.fit, out.sample = 20)$signal_pred)
  log_volume_pred_acc <- log(uniModelPred(data.pred, modelSpec.fit_acc, out.sample = 20)$signal_pred)
  log_volume_real <- log(tail(as.vector(as.matrix(data.pred)), 26 * 20))
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("./tests/testthat/ACN_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(log_volume_pred_acc, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})

test_that("uniModelPred, stock = CVS", {
  data.pred <- readRDS("./tests/testthat/CVS_volume")
  data <- data.pred[,1:104]
  
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  
  modelSpec_v2 <- uniModelSpec(fit = TRUE, init.pars = list(a_mu = 0))
  modelSpec.fit_acc <- uniModelFit(data, modelSpec_v2, maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE)
  
  log_volume_pred <- log(uniModelPred(data.pred, modelSpec.fit, out.sample = 20)$signal_pred)
  log_volume_pred_acc <- log(uniModelPred(data.pred, modelSpec.fit_acc, out.sample = 20)$signal_pred)
  log_volume_real <- log(tail(as.vector(data.pred), 26 * 20))
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("./tests/testthat/CVS_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(log_volume_pred_acc, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-3)
})
