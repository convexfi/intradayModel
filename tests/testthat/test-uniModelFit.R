test_that("uniModelFit params conincide with precomputed model", {
  data("data_log_volume")
  model_spec <- uniModelSpec(fit = TRUE)
  model_par <- uniModelFit(data_log_volume, model_spec)$par
  expect_equal(model_par, load("./tests/testthat/EM_result.rda"))
})
