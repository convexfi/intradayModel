
init.pars <- list()
init.pars$"a_eta" <- 1
init.pars$"x0" <- matrix(0, 2)
init.pars$"xxx" <- 3

fixed.pars <- list()
fixed.pars$"a_mu" <- NA
fixed.pars$"var_eta" <- 4
fixed.pars$"yyy" <- 0

# fit = TRUE
uniModelSpec(fit = TRUE, init.pars = init.pars, fixed.pars = fixed.pars) 
uniModelSpec(fit = FALSE, init.pars = init.pars, fixed.pars = fixed.pars) 