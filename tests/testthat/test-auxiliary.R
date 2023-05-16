test_that("specify_uniss works with partial fixed params", {
  skip_on_cran()
  data <- readRDS(test_path("fixtures", "ADBE_volume"))[,1:104]
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  ## reform data
  data_reform <- log(data) %>%
    as.list() %>%
    unlist()

  fixed_pars <- list()
  fixed_pars$"a_mu" <- 1
  fixed_pars$"var_eta" <- 4
  fixed_pars$"x0" <- matrix(0,2)
  fixed_pars$"phi" <- matrix(2, n_bin)
  model_test <- spec_volume_model(fixed_pars = fixed_pars)

  # predefined model
  model_predefined <- list()
 
  model_predefined$par <- list("a_eta" = NA, "a_mu" = 1, "var_eta" = 4, "var_mu" = NA, 
                        "r" = NA, "phi"= rep(2, n_bin), 
                        "x0" = c(0,0), "V0" = rep(NA, 3))
  model_predefined$init <- list()
  model_predefined$converged <- list("a_eta" = FALSE, "a_mu" = TRUE, "var_eta" = TRUE, "var_mu" = FALSE, 
                                       "r" = FALSE, "phi" = TRUE, "x0" = TRUE, "V0" = FALSE)
  
  class(model_predefined) <- "volume_model"
  expect_equal(model_test, model_predefined)
})

test_that("cleanParsList works", {
  skip_on_cran()
  fixed_pars <- list()
  fixed_pars$"a_mu" <- NA
  fixed_pars$"var_eta" <- 4
  fixed_pars$"x0" <- matrix(0, 2)
  fixed_pars$"V0" <- matrix(c(1,2,3,4), 2)
  
  fixed_pars <- clean_pars_list(fixed_pars)$input_list
  
  predefined.pars <- list()
  predefined.pars$"var_eta" <- 4
  predefined.pars$"x0" <- c(0, 0)
  
  expect_equal(fixed_pars, predefined.pars)
})
