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
  # predefinde_model$init <- list("a_eta" =  1, "a_mu" = NA,
  #                                "var_eta" = NA, "var_mu" = NA,
  #                                "r" = NA, "phi" = NA,
  #                                "x0" = matrix(0, 2), "V0" = matrix(NA,3))
  predefinde_model$init <- list("a_eta" =  1,"x0" = array(c(0,0), dim = c(2,1), dimnames = list(c("x01","x02"),NULL)))
  predefinde_model$fit_request <- list("a_eta" =  TRUE, "a_mu" = TRUE,
                                   "var_eta" = FALSE, "var_mu" = TRUE,
                                   "r" = TRUE, "phi" = TRUE,
                                   "x0" = TRUE, "V0" = TRUE)
  
  expect_equal(uniModelSpec(fit = TRUE, init.pars = init.pars, fixed.pars = fixed.pars), predefinde_model)
  
})

test_that("unimodel specification return coincide with the predefined ones version 2", {
  init.pars <- list()
  init.pars$"a_eta" <- 1
  init.pars$"x0" <- matrix(0, 2)
  init.pars$"xxx" <- 3
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- NA
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0, 2)
  
  predefinde_model <- list()
  predefinde_model$par <- list("a_eta" =  NA, "a_mu" = NA,
                               "var_eta" = 4, "var_mu" = NA,
                               "r" = NA, "phi" = NA,
                               "x0" = array(c(0, 0), dim = c(2, 1), dimnames = list(c("x01","x02"),NULL)),
                               "V0" = matrix(NA,3))
  # predefinde_model$init <- list("a_eta" =  1, "a_mu" = NA,
  #                                "var_eta" = NA, "var_mu" = NA,
  #                                "r" = NA, "phi" = NA,
  #                                "x0" = matrix(0, 2), "V0" = matrix(NA,3))
  predefinde_model$init <- list("a_eta" =  1)
  predefinde_model$fit_request <- list("a_eta" =  TRUE, "a_mu" = TRUE,
                                   "var_eta" = FALSE, "var_mu" = TRUE,
                                   "r" = TRUE, "phi" = TRUE,
                                   "x0" = FALSE, "V0" = TRUE)
  expect_equal(uniModelSpec(fit = TRUE, init.pars = init.pars, fixed.pars = fixed.pars), predefinde_model)
  
})

test_that("unimodel specification return coincide with the predefined ones version 3", {
  init.pars <- list()
  init.pars$"a_eta" <- 1
  init.pars$"x0" <- matrix(0, 2, 2)
  init.pars$"xxx" <- 3
  init.pars$"V0" <- matrix(1, 3)
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- NA
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(Inf, 2)
  fixed.pars$"V0" <- matrix(0, 2)
  
  predefinde_model <- list()
  predefinde_model$par <- list("a_eta" =  NA, "a_mu" = NA,
                               "var_eta" = 4, "var_mu" = NA,
                               "r" = NA, "phi" = NA,
                               "x0" = matrix(NA, 2),
                               "V0" = matrix(NA, 3))
  predefinde_model$init <- list("a_eta" =  1,
                                "V0" = array(c(1, 1, 1), dim = c(3, 1), dimnames = list(c("(1,1)","(2,1)","(2,2)"),NULL)))
  predefinde_model$fit_request <- list("a_eta" =  TRUE, "a_mu" = TRUE,
                                   "var_eta" = FALSE, "var_mu" = TRUE,
                                   "r" = TRUE, "phi" = TRUE,
                                   "x0" = TRUE, "V0" = TRUE)
  expect_equal(uniModelSpec(fit = TRUE, init.pars = init.pars, fixed.pars = fixed.pars), predefinde_model)
  
})

test_that("unimodel specification result error control",{
  init.pars <- list()
  init.pars$"a_eta" <- 1
  init.pars$"x0" <- matrix(0, 2)
  init.pars$"xxx" <- 3
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- NA
  fixed.pars$"var_eta" <- 4
  fixed.pars$"yyy" <- 0
  
  expect_error(uniModelSpec(fit = FALSE, init.pars = init.pars, fixed.pars = fixed.pars),
               c("Wrong input: unfitted model contains unknown parameters \n"))
  
})
