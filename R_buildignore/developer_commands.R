##
## User installation
##
# Local installation
install.packages(file.choose(), repos = NULL, type = "source")
# Installation from GitHub
devtools::install_github("convexfi/intradayModel")
# Installation from CRAN
install.packages("intradayModel")
# Getting help
library(intradayModel)
help(package = "intradayModel")
?uniModelFit
citation("intradayModel")
vignette(package = "intradayModel")


##
## Developer commands (https://r-pkgs.org/)
##
devtools::load_all()  #or Ctrl-Shift-L
devtools::document()  #to generate all documentation via roxygen
devtools::install()
devtools::install(dependencies = FALSE)
library(intradayModel)


# Code tests
devtools::test()
#covr::package_coverage()  #coverage of tests


# CRAN check and submission (https://r-pkgs.org/release.html)
#  checklist: https://kalimu.github.io/post/checklist-for-r-package-submission-to-cran/
devtools::check()  # run_dont_test = TRUE
devtools::check(args = c('--ignore-vignettes'), build_args = c('--no-build-vignettes'))
rcmdcheck::rcmdcheck()  # build_args = "--run-donttest"
rcmdcheck::rcmdcheck(args = c('--ignore-vignettes'), build_args = c('--no-build-vignettes'))
devtools::build()
devtools::build(args = c('--no-build-vignettes'))
#devtools::revdep(pkg = "intradayModel")  # to check reverse dependencies
#devtools::check_win_release()  #to check under windows
#R CMD build . --resave-data  # this is to generate tarball
#R CMD check intradayModel_0.0.1.tar.gz --as-cran --run-donttest  # this is before submission to CRAN
#R CMD install intradayModeling_0.0.1.tar.gz
# check Mac builder at: https://mac.r-project.org/macbuilder/submit.html
#submit the tarball directly via the webform: https://cran.r-project.org/submit.html
