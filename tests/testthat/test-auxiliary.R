test_that("specify_uniss works with partial fixed params", {
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
  model_test <- spec_unimodel(fixed_pars = fixed_pars)

  # predefined model
  model_predefined <- list()
 
  model_predefined$par <- list("a_eta" = NA, "a_mu" = 1, "var_eta" = 4, "var_mu" = NA, 
                        "r" = NA, "phi"= rep(2, n_bin), 
                        "x0" = c(0,0), "V0" = rep(NA, 3))
  model_predefined$init <- list()
  model_predefined$fit_request <- list("a_eta" = TRUE, "a_mu" = FALSE, "var_eta" = FALSE, "var_mu" = TRUE, 
                                       "r" = TRUE, "phi" = FALSE, "x0" = FALSE, "V0" = TRUE)
  
  class(model_predefined) <- "unimodel"
  expect_equal(model_test, model_predefined)
})

test_that("cleanParsList works", {
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
