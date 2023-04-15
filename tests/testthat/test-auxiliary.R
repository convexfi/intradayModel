test_that("marss_to_unimodel works", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  fixed.pars <- list()
  fixed.pars$"var_eta" <- 4
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  data <- data_log_volume
  data <- as.matrix(data)
  
  data_reform <- data %>%
    as.list() %>%
    unlist()
  
  args <- list(data = data,
               uniModel = modelSpec)
  
  
  kalman <- do.call(specify_marss, args = args)
  kalman$par <- kalman$start
  
  trans.param <- marss_to_unimodel(kalman$par, modelSpec$par)
  
  predefinde_params <- list()
  predefinde_params$"a_eta" <- 1
  predefinde_params$"a_mu" <- 0
  predefinde_params$"var_eta" <- 4
  predefinde_params$"var_mu" <- 1e-4
  predefinde_params$"r" <- 1e-4
  predefinde_params$"phi" <- matrix(rowMeans(matrix(data_reform, nrow = n_bin)) - mean(data_reform), nrow = n_bin)
  phi_names <- c()
  for (i in 1:n_bin){
    phi_names <- append(phi_names, paste(paste("phi", i, sep = "")))
  }
  dimnames(predefinde_params$"phi")[[1]] <- phi_names
  predefinde_params$"x0" <- matrix(c(mean(data_reform), 0), 2, 1)
  dimnames(predefinde_params$"x0")[[1]] <- c("x01","x02")
  predefinde_params$"V0" <- matrix(c(1e-3, 1e-7, 1e-5), 3, 1)
  dimnames(predefinde_params$"V0")[[1]] <- c("(1,1)","(2,1)","(2,2)")
  
  expect_equal(trans.param, predefinde_params)
  
})

test_that("specify_marss works without fixed params", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data_reform <- data %>%
    as.list() %>%
    unlist()
  modelSpec <- uniModelSpec(fit = TRUE)
  
  args <- list(data = data,
               uniModel = modelSpec)
  
  model_test <- do.call(specify_marss, args = args)
  
  # specify the marss in the original way
  MARSS_model <- list()
  MARSS_model$model.gen <- list()
  MARSS_model$init.gen <- list()
  
  ## State Equation
  Bt <- array(list(0), c(2, 2, n_bin_total))
  b1 <- matrix(list(1), n_bin)
  b1[1] <- extract_value("a_eta", modelSpec)
  Bt[1, 1, ] <- rep(b1, n_day)
  Bt[2, 2, ] <- extract_value("a_mu", modelSpec)
  
  Qt <- array(list(0), c(2, 2, n_bin_total))
  q1 <- matrix(list(1e-10), n_bin)
  q1[1] <- extract_value("var_eta", modelSpec)
  Qt[1, 1, ] <- rep(q1, n_day)
  Qt[2, 2, ] <- extract_value("var_mu", modelSpec)
  
  U <- "zero"
  
  ## Measurement Equation
  Z <- array(list(1, 1), c(1, 2))
  
  At = array(list(0), dim = c(1, 1, n_bin_total))
  a_vec = extract_value("phi", modelSpec)
  if (identical(a_vec, "phi") || length(a_vec) != n_bin){
    if (!identical(a_vec, "phi") && length(a_vec) != n_bin) warning("Dimensions of input data and pre-fixed phi aren't compatible.\n
                                       The values of fixed phi are ignored.")
    for (n in 1:n_bin) {
      a_vec[n] <- paste("phi", n, sep = "")
    }
    
  }
  
  At[1, 1, ] = rep(a_vec, n_day)
  
  R <- extract_value("r", modelSpec) %>%
    list() %>%
    matrix(1,1)
  
  ## Initial State
  x0 <- extract_value("x0", modelSpec) %>%
    matrix(2, 1)
  V0 <- extract_value("V0", modelSpec)
  
  ## predefined init value
  init_default <- list("x0" = matrix(c(mean(data_reform), 0), 2, 1),
                       "a_eta" = 1, "a_mu" = 0,
                       "r" = 1e-4,
                       "var_eta" = 1e-4, "var_mu" = 1e-4,
                       "V0" = matrix(c(1e-3, 1e-7, 1e-5), 3, 1),
                       "phi" = rowMeans(matrix(data_reform, nrow = n_bin)) - mean(data_reform)
  )
  ## Init param
  MARSS_model$init.gen <- extract_init(init_default, modelSpec$init, modelSpec$fit_request)
  
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  model_original <- MARSS::MARSS(data_reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE, silent = TRUE)
  
  
  expect_equal(model_test, model_original)
  
})

# need to be checked again
test_that("specify_marss works with partial fixed params", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  ## reform data
  data_reform <- data %>%
    as.list() %>%
    unlist()
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  args <- list(data = data, 
               uniModel = modelSpec)
  
  model_test <- do.call(specify_marss, args = args)
  
  # specify the marss in the original way
  MARSS_model <- list()
  MARSS_model$model.gen <- list()
  MARSS_model$init.gen <- list()
  
  ## State Equation
  Bt <- array(list(0), c(2, 2, n_bin_total))
  b1 <- matrix(list(1), n_bin)
  b1[1] <- "a_eta"
  Bt[1, 1, ] <- rep(b1, n_day)
  Bt[2, 2, ] <- 1
  
  Qt <- array(list(0), c(2, 2, n_bin_total))
  q1 <- matrix(list(1e-10), n_bin)
  q1[1] <- 4
  Qt[1, 1, ] <- rep(q1, n_day)
  Qt[2, 2, ] <- "var_mu"
  
  U <- "zero"
  
  ## Measurement Equation
  Z <- array(list(1, 1), c(1, 2))
  
  At <- array(list(0), dim = c(1, 1, n_bin_total))
  a_vec <- array(rep(2, n_bin), dim = c(n_bin,1))
  At[1, 1, ] = rep(a_vec, n_day)
  
  R <- matrix(list("r"), 1,1)
  
  ## Initial State
  x0 <- matrix(list(0,0), 2, 1)
  V0 <- "unconstrained"

  MARSS_model$init.gen <-  list(R = 1e-4,
                                V0 = matrix(c(1e-3, 1e-7, 1e-5), 3, 1),
                                B = matrix(c(1), 1, 1),
                                Q = matrix(c(1e-4), 1, 1)
  )
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  model_original <- MARSS::MARSS(data_reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE,silent = TRUE)
  
  
  expect_equal(model_test, model_original)
  
})

test_that("extract_value works", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data_reform <- data %>%
    as.list() %>%
    unlist()
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  expect_equal(extract_value("a_mu", modelSpec), 1)
  expect_equal(extract_value("x0", modelSpec), array(c(0, 0), dim = c(2,1), dimnames = list(c("x01","x02"),NULL)))
  expect_equal(extract_value("V0", modelSpec), "unconstrained")
 
})

test_that("extract_init works", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data_reform <- data %>%
    as.list() %>%
    unlist()
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  args <- list(data = data,
               uniModel = modelSpec)
  
  
  init_default <- list("x0" = matrix(c(10, 0), 2, 1),
                       "a_eta" = 1, "a_mu" = 0.7,
                       "r" = 0.08,
                       "var_eta" = 0.07, "var_mu" = 0.06,
                       "V0" = matrix(c(1e-3, 1e-7, 1e-5), 3, 1),
                       "phi" = rowMeans(matrix(data_reform, nrow = n_bin)) - mean(data_reform)
  )
  
  init_test <- extract_init(init_default, modelSpec$init, modelSpec$fit_request)
  
  ## Init param
  init_ori <- list(R = 0.08,
                   V0 = matrix(c(1e-3, 1e-7, 1e-5), 3, 1),
                   B = matrix(c(1), 1, 1),
                   Q = matrix(c(0.06), 1, 1)
  )
  
  expect_equal(init_test, init_ori)
})

test_that("cleanParsList works", {
  test.pars <- list()
  test.pars$"a_eta" <- "a"
  test.pars$"a_mu" <- 1
  test.pars$"var_eta" <- Inf
  test.pars$"x0" <- matrix(c(NA,0),2)
  test.pars$"V0" <- matrix(0,3)
  test.pars$"yyy" <- 6
  
  test.pars <- clean_pars_list(test.pars)
  
  predefined.pars <- list()
  predefined.pars$"a_mu" <- 1
  # predefined.pars$"V0" <- c(0,0,0)
  
  expect_equal(test.pars, predefined.pars)
})

test_that("is_uniModel works", {
  n_bin <- 26
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  data("data_log_volume")
  
  modelSpec_check1 <- modelSpec[c("par", "init")]
  expect_error(is_uniModel(modelSpec_check1), "Element fit_request is missing from the uniModel object.\n")
  
  modelSpec_check2 <- modelSpec
  modelSpec_check2$par[["x0"]] <- NULL
  expect_error(is_uniModel(modelSpec_check2),"Element x0 is missing from the uniModel[$]par.\n")
  
  modelSpec_check3 <- modelSpec
  modelSpec_check3$fit_request[["var_eta"]] <- TRUE
  expect_error(is_uniModel(modelSpec_check3), "uniModel[$]par[$]var_eta and uniModel[$]fit_request[$]var_eta are conflicted.\n")

  modelSpec_check4 <- modelSpec
  modelSpec_check4$par[["x0"]] <- 1
  modelSpec_check4$par[["var_eta"]] <- array(c(1,2))
  error_message <- paste("Dimension of uniModel[$]par[$]var_eta is wrong.\n","Length of uniModel[$]par[$]var_eta is wrong.\n",
                         "Dimension of uniModel[$]par[$]phi is wrong.\n","Dimension of uniModel[$]par[$]x0 is wrong.\n", sep = "")
  expect_error(is_uniModel(modelSpec_check4, 25), error_message)
  
})
