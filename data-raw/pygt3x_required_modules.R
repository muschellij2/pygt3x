## code to prepare `pygt3x_required_modules` dataset goes here
pygt3x_required_modules = readLines("inst/gt3x/requirements.txt")
usethis::use_data(pygt3x_required_modules, overwrite = TRUE)
