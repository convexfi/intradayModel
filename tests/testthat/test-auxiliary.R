test_that("trans_MARSStoIntra works", {
  data("data_log_volume")
  
  fixed.pars <- list()
  fixed.pars$"var_eta" <- 4
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  data <- data_log_volume
  data <- as.matrix(data)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  data.reform <- data %>%
    as.list() %>%
    unlist()
  
  args <- list(data = data, n_bin = n_bin,
               n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec)
  
  
  result <- do.call(MARSS_spec, args = args)
  kalman <- result$kalman
  kalman$par <- kalman$start
  
  trans.param <- trans_MARSStoIntra(kalman$par, modelSpec$par)
  
  predefinde_params <- list()
  predefinde_params$"a_eta" <- 1
  predefinde_params$"a_mu" <- 0.7
  predefinde_params$"var_eta" <- 4
  predefinde_params$"var_mu" <- 0.06
  predefinde_params$"r" <- 0.08
  predefinde_params$"phi" <- matrix(rowMeans(matrix(data.reform, nrow = n_bin)) - mean(data.reform), nrow = n_bin)
  phi_names <- c()
  for (i in 1:n_bin){
    phi_names <- append(phi_names, paste(paste("phi", i, sep = "")))
  }
  dimnames(predefinde_params$"phi")[[1]] <- phi_names
  predefinde_params$"x0" <- matrix(c(10, 0), 2, 1)
  dimnames(predefinde_params$"x0")[[1]] <- c("x01","x02")
  predefinde_params$"V0" <- matrix(c(1e-10, 0, 1e-10), 3, 1)
  dimnames(predefinde_params$"V0")[[1]] <- c("(1,1)","(2,1)","(2,2)")
  
  expect_equal(trans.param, predefinde_params)
  
})


test_that("MARSS_spec works without fixed params", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data.reform <- data %>%
    as.list() %>%
    unlist()
  modelSpec <- uniModelSpec(fit = TRUE)
  
  args <- list(data = data, n_bin = n_bin, n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec)
  
  model.test <- do.call(MARSS_spec, args = args)
  
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
  init.default <- list("x0" = matrix(c(10, 0), 2, 1),
                       "a_eta" = 1, "a_mu" = 0.7,
                       "r" = 0.08,
                       "var_eta" = 0.07, "var_mu" = 0.06,
                       "V0" = matrix(c(1e-10, 0, 1e-10), 3, 1),
                       "phi" = rowMeans(matrix(data.reform, nrow = n_bin)) - mean(data.reform)
  )
  ## Init param
  MARSS_model$init.gen <- extract_init(init.default, modelSpec$init, modelSpec$fitFlag)
  
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  model.original <- MARSS::MARSS(data.reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE)
  
  
  expect_equal(model.test$kalman, model.original)
  expect_equal(model.test$At, At)
  
})

test_that("MARSS_spec works with partial fixed params", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data.reform <- data %>%
    as.list() %>%
    unlist()
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  args <- list(data = data, n_bin = n_bin, n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec)
  
  model.test <- do.call(MARSS_spec, args = args)
  
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
  
  ## predefined init value
  # init.default <- list("x0" = matrix(c(10, 0), 2, 1),
  #                      "a_eta" = 1, "a_mu" = 0.7,
  #                      "r" = 0.08,
  #                      "var_eta" = 0.07, "var_mu" = 0.06,
  #                      "V0" = matrix(c(1e-10, 0, 1e-10), 3, 1),
  #                      "phi" = rowMeans(matrix(data.reform, nrow = n_bin)) - mean(data.reform)
  # )
  ## Init param
  # MARSS_model$init.gen <- list(B = matrix(c(1), 1, 1),
  #   R = 0.08,
  #   Q = matrix(c(0.06), 1, 1),
  #   V0 = matrix(c(1e-10, 0, 1e-10), 3, 1)
  # )
  MARSS_model$init.gen <-  list(R = 0.08,
                                V0 = matrix(c(1e-10, 0, 1e-10), 3, 1),
                                B = matrix(c(1), 1, 1),
                                Q = matrix(c(0.06), 1, 1)
  )
  ## EM
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  model.original <- MARSS::MARSS(data.reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE)
  
  
  expect_equal(model.test$kalman, model.original)
  expect_equal(model.test$At, At)
  
})

test_that("extract_value works", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data.reform <- data %>%
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
  # expect_equal(extract_value("x0", modelSpec), array(c(0, 0), dim = c(2,1)))
  
})

test_that("extract_init works", {
  data("data_log_volume")
  data <- as.matrix(data_log_volume)
  n_bin <- nrow(data)
  n_day <- ncol(data)
  n_bin_total <- n_bin * n_day
  
  ## reform data
  data.reform <- data %>%
    as.list() %>%
    unlist()
  
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  args <- list(data = data, n_bin = n_bin, n_day = n_day, n_bin_total = n_bin_total,
               modelSpec = modelSpec)
  
  
  init.default <- list("x0" = matrix(c(10, 0), 2, 1),
                       "a_eta" = 1, "a_mu" = 0.7,
                       "r" = 0.08,
                       "var_eta" = 0.07, "var_mu" = 0.06,
                       "V0" = matrix(c(1e-10, 0, 1e-10), 3, 1),
                       "phi" = rowMeans(matrix(data.reform, nrow = n_bin)) - mean(data.reform)
  )
  
  init.test <- extract_init(init.default, modelSpec$init, modelSpec$fitFlag)
  
  ## Init param
  init.ori <- list(R = 0.08,
                   V0 = matrix(c(1e-10, 0, 1e-10), 3, 1),
                   B = matrix(c(1), 1, 1),
                   Q = matrix(c(0.06), 1, 1)
  )
  
  expect_equal(init.test, init.ori)
})

test_that("transList works", {
  test.pars <- list()
  test.pars$"a_eta" <- "a"
  test.pars$"a_mu" <- 1
  test.pars$"var_eta" <- Inf
  test.pars$"x0" <- matrix(c(NA,0),2)
  test.pars$"V0" <- matrix(0,3)
  test.pars$"yyy" <- 6
  
  test.pars <- transList(test.pars)
  
  predefined.pars <- list()
  predefined.pars$"a_mu" <- 1
  predefined.pars$"V0" <- c(0,0,0)
  
  expect_equal(test.pars, predefined.pars)
})

# unfinished
test_that("isIntraModel works", {
  n_bin <- 26
  fixed.pars <- list()
  fixed.pars$"a_mu" <- 1
  fixed.pars$"var_eta" <- 4
  fixed.pars$"x0" <- matrix(0,2)
  fixed.pars$"phi" <- matrix(2, n_bin)
  modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
  
  modelSpec_check1 <- modelSpec[c("par", "init")]
  modelSpec_check2 <- modelSpec
  modelSpec_check2$par[["x0"]] <- NULL
  modelSpec_check2$par[["a_eta"]] <- NULL
  
  expect_error(isIntraModel(modelSpec_check1), "Element fitFlag is missing from the model object.\n")
  # expect_error(isIntraModel(modelSpec_check2), c("Element a_etax0 is missing from the model$par.\n"))
})

test_that("isIntraModel test 1", {
  data("data_log_volume")
  data <- data_log_volume
  data <- as.matrix(data)
  
  modelSpec <- uniModelSpec(fit = TRUE)
  modelSpec$par <- NULL
  modelSpec$init <- NULL
  
  # expect_error(uniModelFit(data, modelSpec), c("Element par & init is missing from the model object."))
  expect_error(uniModelFit(data, modelSpec), regexp = "Element par & init is missing from the model")
})

