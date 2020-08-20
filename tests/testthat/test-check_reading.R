path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
                   package = "pygt3x")

testthat::test_that("read.gt3x and py_read_gt3x agree", {
  rg = read.gt3x::read.gt3x(path, verbose = TRUE, debug = TRUE, asDataFrame = TRUE)
  rg = rg[, c("X", "Y", "Z")]
  class(rg) = "data.frame"


  res = py_read_gt3x(path, verbose = TRUE)
  test = res$data
  zero = rowSums(test[, c("X", "Y", "Z")] == 0) == 3
  test = test[!zero, ]
  zero = rowSums(is.na(test[, c("X", "Y", "Z")])) == 3
  test = test[!zero, ]
  testthat::expect_true(max(abs(as.matrix(rg) - as.matrix(test))) == 0)

})




