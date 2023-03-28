# test_that("trans_MARSStoIntra works", {
#
#   fixed.pars <- list()
#   fixed.pars$"var_eta" <- 4
#   modelSpec <- uniModelSpec(fit = TRUE, fixed.pars = fixed.pars)
#
#
#
#
#
#   expect_equal(uniModelSpec(fit = TRUE, fixed.pars = fixed.pars), predefinde_model)
#
# })


test_that("MARSS_spec works", {
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
  if (a_vec == "phi" || length(a_vec) != n_bin){
    if (a_vec != "phi" && length(a_vec) != n_bin) warning("Dimensions of input data and pre-fixed phi aren't compatible.\n
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
  
  
  
  
  expect_equal(model.test, model.original)
  
})
