test_that("forecast_unimodel, stock = ADBE", {
  skip_on_cran()
  data.pred <- readRDS(test_path("fixtures", "ADBE_volume"))
  data <- data.pred[,1:104]

  # modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- fit_unimodel(data, control = list(maxit = 1000, abstol = 1e-4, log.switch = TRUE))
  modelSpec.fit_acc <- fit_unimodel(data, control = list(maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE))
  
  log_volume_pred <- log(forecast_unimodel(data.pred, modelSpec.fit, out.sample = 20)$forecast.signal)
  log_volume_pred_acc <- log(forecast_unimodel(data.pred, modelSpec.fit_acc, out.sample = 20)$forecast.signal)
  log_volume_real <- log(tail(as.vector(data.pred), 26 * 20))
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS(test_path("fixtures", "ADBE_expected_pred"))

  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(log_volume_pred_acc, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-4)
})

test_that("forecast_unimodel, ACN", {
  skip_on_cran()
  data.pred <- readRDS(test_path("fixtures", "ACN_volume"))
  data <- data.pred[,1:104]
  
  # modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- fit_unimodel(data, control = list(maxit = 1000, abstol = 1e-4, log.switch = TRUE))
  modelSpec.fit_acc <- fit_unimodel(data, control = list(maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE))
  
  log_volume_pred <- log(forecast_unimodel(data.pred, modelSpec.fit, out.sample = 20)$forecast.signal)
  log_volume_pred_acc <- log(forecast_unimodel(data.pred, modelSpec.fit_acc, out.sample = 20)$forecast.signal)
  log_volume_real <- log(tail(as.vector(as.matrix(data.pred)), 26 * 20))
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS(test_path("fixtures", "ACN_expected_pred"))
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(log_volume_pred_acc, expected_res$log_volume_pred, tolerance = 1e-4)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 5e-4)
})

test_that("forecast_unimodel, stock = CVS", {
  skip_on_cran()
  data.pred <- readRDS(test_path("fixtures", "CVS_volume"))
  data <- data.pred[,1:104]
  
  # modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- fit_unimodel(data, control = list(maxit = 1000, abstol = 1e-4, log.switch = TRUE))
  
  # modelSpec_v2 <- uniModelSpec(fit = TRUE, init.pars = list(a_mu = 0))
  modelSpec.fit_acc <- fit_unimodel(data, init.pars = list(a_mu = 0), control = list(maxit = 1000, abstol = 1e-4, log.switch = TRUE, acceleration = TRUE))
  
  log_volume_pred <- log(forecast_unimodel(data.pred, modelSpec.fit, out.sample = 20)$forecast.signal)
  log_volume_pred_acc <- log(forecast_unimodel(data.pred, modelSpec.fit_acc, out.sample = 20)$forecast.signal)
  log_volume_real <- log(tail(as.vector(data.pred), 26 * 20))
  
  mae <-calculate_mae(log_volume_real, log_volume_pred)
  mape <- calculate_mape(log_volume_real, log_volume_pred)
  rmse <-calculate_rmse(log_volume_real, log_volume_pred)
  
  expected_res <- readRDS(test_path("fixtures", "CVS_expected_pred"))
  
  expect_equal(log_volume_pred, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(log_volume_pred_acc, expected_res$log_volume_pred, tolerance = 1e-3)
  expect_equal(c(mae, mape, rmse), c(expected_res$mae, expected_res$mape, expected_res$rmse), tolerance = 1e-3)
})
