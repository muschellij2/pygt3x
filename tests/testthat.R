library(testthat)
library(pygt3x)

skip_read_gt3x = function() {
  testthat::skip_if_not_installed("read.gt3x")
}

skip_zoo = function() {
  testthat::skip_if_not_installed("zoo")
}

skip_readr = function() {
  testthat::skip_if_not_installed("readr")
}

skip_python = function() {
  testthat::skip_if(!pygt3x:::have_python_requirements(),
                    message = "Python, or Python module not installed")
}

test_check("pygt3x")
