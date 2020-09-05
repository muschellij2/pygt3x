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

test_check("pygt3x")
