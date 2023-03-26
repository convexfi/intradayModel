test_that("unimodel specification return coincide with the predefined ones", {
  init.pars <- list()
  init.pars$"a_eta" <- 1
  init.pars$"x0" <- matrix(0, 2)
  init.pars$"xxx" <- 3
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- NA
  fixed.pars$"var_eta" <- 4
  fixed.pars$"yyy" <- 0
  
  predefinde_model <- list()
  predefinde_model$par <- list("a_eta" =  NA, "a_mu" = NA,
                               "var_eta" = 4, "var_mu" = NA,
                               "r" = NA, "phi" = NA,
                               "x0" = matrix(NA, 2), "V0" = matrix(NA,3))
  predefinde_model$init <- list("a_eta" =  1, "a_mu" = NA,
                                "var_eta" = NA, "var_mu" = NA,
                                "r" = NA, "phi" = NA,
                                "x0" = matrix(0, 2), "V0" = matrix(NA,3))
  predefinde_model$fitFlag <- list("a_eta" =  TRUE, "a_mu" = TRUE,
                                   "var_eta" = FALSE, "var_mu" = TRUE,
                                   "r" = TRUE, "phi" = TRUE,
                                   "x0" = TRUE, "V0" = TRUE)
  
  
  expect_equal(uniModelSpec(fit = TRUE, init.pars = init.pars, fixed.pars = fixed.pars), predefinde_model)
  
})