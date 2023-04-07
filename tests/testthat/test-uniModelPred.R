test_that("uniModelFilter, batch = 5", {
  data <- readRDS("data/batch5_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  data.pred <- readRDS("data/batch5_log_volume_pred")
  log_volume_pred <- uniModelPred(data.pred, modelSpec.fit, out.sample = 20)
  log_volume_real <- tail(as.vector(data.pred), 26 * 20)
  
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("data/batch5_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})

test_that("uniModelFilter, batch = 1", {
  data <- readRDS("data/batch1_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  data.pred <- readRDS("data/batch1_log_volume_pred")
  log_volume_pred <- uniModelPred(data.pred, modelSpec.fit, out.sample = 20)
  log_volume_real <- tail(as.vector(data.pred), 26 * 20)
  
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("data/batch1_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})

test_that("uniModelFilter, ACN", {
  data <- readRDS("data/ACN_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  data.pred <- readRDS("data/ACN_log_volume_pred")
  log_volume_pred <- uniModelPred(data.pred, modelSpec.fit, out.sample = 20)
  log_volume_real <- tail(as.vector(as.matrix(data.pred)), 26 * 20)
  
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("data/ACN_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})
