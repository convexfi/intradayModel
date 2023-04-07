test_that("uniModelFilter, stock = ADBE", {
  data <- readRDS("data/ADBE_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  data.pred <- readRDS("data/ADBE_log_volume_pred")
  log_volume_pred <- uniModelPred(data.pred, modelSpec.fit, out.sample = 20)
  log_volume_real <- tail(as.vector(data.pred), 26 * 20)
  
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("data/ADBE_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})

test_that("uniModelFilter, stock = CVS", {
  data <- readRDS("data/CVS_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  data.pred <- readRDS("data/CVS_log_volume_pred")
  log_volume_pred <- uniModelPred(data.pred, modelSpec.fit, out.sample = 20)
  log_volume_real <- tail(as.vector(data.pred), 26 * 20)
  
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS("data/CVS_expected_pred")
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-3)
})