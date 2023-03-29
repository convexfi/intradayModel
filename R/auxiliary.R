# This is a library of auxiliary functions, which might be useful for other exported functions of this package.
# They should be invisible to package users

# transform the parameter form the format of MARSS to IntradayModel
trans_MARSStoIntra <- function(MARSS.par, intra.par){
  all.pars.name.MARSS <- c("A", "R", "B", "Q", "x0", "V0")
  MARSS.par <- MARSS.par[all.pars.name.MARSS]
  for (name in c("B","Q","R")){
    if (length(MARSS.par[[name]]) > 0){
      intra.name <- dimnames(MARSS.par[[name]])[[1]]
      fix_par <- as.list(MARSS.par[[name]])
      names(fix_par) <- intra.name
      MARSS.par[[name]] <- NULL
      MARSS.par <- append(MARSS.par, fix_par)
    }
    else {
      MARSS.par[[name]] <- NULL
    }
  }
  
  if (length(MARSS.par[["A"]]) > 0 ){
    MARSS.par[["phi"]] <- MARSS.par[["A"]]
  }
  
  for (name in names(intra.par)){
    if (anyNA(intra.par[[name]])){
      intra.par[[name]] <- MARSS.par[[name]]
    }
  }
  return (intra.par)
}

#
MARSS_spec <- function(...){
  args <- list(...)
  data <- args$data
  n_bin <- args$n_bin
  n_bin_total <- args$n_bin_total
  n_day <- args$n_day
  modelSpec <- args$modelSpec
  
  data.reform <- data %>%
    as.list() %>%
    unlist()
  
  ## MARSS parameters
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
  if (!identical(V0 ,"unconstrained")){
    V0 <- matrix(c(as.numeric(V0[1,1]),as.numeric(V0[2,1]),as.numeric(V0[2,1]),as.numeric(V0[3,1]) ), nrow = 2)
  }
  
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
  
  ## MARSS model
  MARSS_model$model.gen <- list(Z=Z,R=R,A=At,B=Bt, Q=Qt, U=U, x0=x0,V0=V0, tinitx=1)
  kalman <- MARSS::MARSS(data.reform, model=MARSS_model$model.gen, inits = MARSS_model$init.gen, fit=FALSE)
  return (kalman)
}


extract_value <- function(name, modelSpec) {
  if (modelSpec$fitFlag[[name]]) {
    name <- switch(name,
                   "x0"= list("x01","x02"),
                   "V0" = "unconstrained",
                   name)
    return(name)
  } else {
    return(modelSpec$par[[name]])
  }
}

extract_init <- function(init.default, init.pars, fitFlag){
  all.pars.name <- c("a_eta", "a_mu", "var_eta", "var_mu", "r", "phi", "x0", "V0")
  for (name in all.pars.name){
    if (!fitFlag[[name]]) {
      init.default[[name]] <- NULL
    }
    if (name %in% names(init.pars)){
      init.default[[name]] <- init.pars[[name]]
    }
  }
  init.marss <- list()
  var.init <- c()
  a.init <- c()
  for (name in names(init.default)){
    tmp <- switch(name,
                  "phi" = {init.marss$A <- init.default$phi},
                  "V0" = {init.marss$V0 <- init.default$V0},
                  "x0" = {init.marss$x0 <- init.default$x0},
                  "r" = {init.marss$R <- init.default$r},
                  "a_eta" = {a.init <- append(a.init, init.default$a_eta)},
                  "a_mu" = {a.init <- append(a.init, init.default$a_mu)},
                  "var_eta" = {var.init <- append(var.init, init.default$var_eta)},
                  "var_mu" = {var.init <- append(var.init, init.default$var_mu)}
    )
  }
  if (length(a.init) > 0){
    init.marss$B <- matrix(a.init, length(a.init), 1)
  }
  if (length(var.init) > 0){
    init.marss$Q <- matrix(var.init, length(var.init), 1)
  }
  return (init.marss)
}

