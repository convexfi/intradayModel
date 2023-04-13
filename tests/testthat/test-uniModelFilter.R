test_that("uniModelFilter, stock = ADBE", {
  data <- exp(readRDS("data/ADBE_log_volume"))
  uniModel <- uniModelSpec(fit = TRUE)
  uniModel.fit <- uniModelFit(data, uniModel, maxit = 1000, abstol = 1e-4, log.switch = TRUE)
  components <- uniModelFilter(data, uniModel.fit)
  components$plot
  
  components_2 <- uniModelFilter(data[, 51:100], uniModel.fit)
  components_2$plot
  
})

