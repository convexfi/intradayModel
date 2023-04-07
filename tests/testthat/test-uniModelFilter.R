test_that("uniModelFilter, stock = ADBE", {
  data <- readRDS("data/ADBE_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  uniModelPlot(data, filter_result, "daily")
  plot_decomposition(data, filter_result)
  
  
  filter_result2 <- uniModelFilter(data[, 51:100], modelSpec.fit)
  plot_decomposition(data[, 51:100], filter_result2)
  
})

