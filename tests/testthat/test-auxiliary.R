test_that("specify_uniss works with partial fixed params", {
  data <- readRDS(test_path("fixtures", "ADBE_volume"))[,1:104]
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  ## reform data
  data_reform <- log(data) %>%
    as.list() %>%
    unlist()

  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)

  args <- list(data = log(data),
               uniModel = modelSpec)

  model_test <- do.call(specify_uniss, args = args)

  # predefined model
  model_predefined <- list()
 
  model_predefined$par <- list("a_eta" = 1, "a_mu" = 1, "var_eta" = 4, "var_mu" = 1e-4, 
                        "r" = 1e-4, "phi"= rep(2, n_bin), 
                        "x0" = c(0,0), "V0" = c(1e-3, 1e-7, 1e-5))
  model_predefined$y <- unlist(as.list(log(data)))
  model_predefined$n_bin <- 26
  model_predefined$n_day <- 104
  model_predefined$n_bin_total <- 2704
  model_predefined$fit_request <- list("a_eta" = TRUE, "a_mu" = FALSE, "var_eta" = FALSE, "var_mu" = TRUE, 
                                       "r" = TRUE, "phi" = FALSE, "x0" = FALSE, "V0" = TRUE)
  
  
  expect_equal(model_test, model_predefined)
})

test_that("cleanParsList works", {
  fixed.pars <- list()
  fixed.pars$"a_mu" <- NA
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0, 2)
  fixed.pars$"V0" <- matrix(c(1,2,3,4), 2)
  
  fixed.pars <- clean_pars_list(fixed.pars)$input_list
  
  predefined.pars <- list()
  predefined.pars$"var_eta" <- 4
  predefined.pars$"x0" <- c(0, 0)
  
  expect_equal(fixed.pars, predefined.pars)
})

test_that("is_uniModel works", {
  n_bin <- 26
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  modelSpec_check1 <- modelSpec[c("par", "init")]
  expect_error(is_uniModel(modelSpec_check1), "Elements fit_request are missing from the model.\n")
  
  modelSpec_check2 <- modelSpec
  modelSpec_check2$par[["x0"]] <- NULL
  expect_error(is_uniModel(modelSpec_check2),"Elements x0 are missing from uniModel[$]par.\n")
  
  modelSpec_check3 <- modelSpec
  modelSpec_check3$fit_request[["var_eta"]] <- TRUE
  expect_error(is_uniModel(modelSpec_check3), "uniModel[$]par[$]var_eta and uniModel[$]fit_request[$]var_eta are conflicted.\n")

  modelSpec_check4 <- modelSpec
  modelSpec_check4$par[["x0"]] <- 1
  modelSpec_check4$par[["var_eta"]] <- array(c(1,2))
  error_message <- paste("Length of uniModel[$]par[$]var_eta is wrong.\n")
  expect_error(is_uniModel(modelSpec_check4, 25), error_message)
  
})
