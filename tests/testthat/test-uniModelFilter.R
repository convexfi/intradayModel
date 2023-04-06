test_that("uniModelFilter, batch = 5", {
  data <- readRDS("data/batch5_log_volume")
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec.fit <- uniModelFit(data, modelSpec, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  filter_result <- uniModelFilter(data, modelSpec.fit)
  
  uniModelPlot(data, filter_result, "daily")
  
  plot_decomposition(data, filter_result$daily, filter_result$seasonal, filter_result$dynamic)
})

