# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

get_stage("install") %>% 
  add_code_step(reticulate::install_miniconda()) %>% 
  add_code_step(tensorflow::install_tensorflow()) %>% 
  add_code_step(keras::install_keras())

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  do_pkgdown()
}
