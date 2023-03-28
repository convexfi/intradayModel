# This is a library of auxiliary functions, which might be useful for other exported functions of this package.
# They should be invisible to package users

trans_MARSStoIntra <- function(MARSS.model, intra.par){
  all.pars.name.MARSS <- c("A", "R", "B", "Q", "x0", "V0")
  MARSS.par <- MARSS.model$par[all.pars.name.MARSS]
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
