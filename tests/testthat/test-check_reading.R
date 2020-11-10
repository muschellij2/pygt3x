path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
                   package = "pygt3x")

testthat::test_that("read.gt3x and py_read_gt3x agree", {
  skip_python()
  rg = read.gt3x::read.gt3x(path, verbose = FALSE,
                            debug = FALSE, asDataFrame = TRUE)
  rg = rg[, c("X", "Y", "Z")]
  class(rg) = "data.frame"


  res = pygt3x::py_read_gt3x(path, verbose = 2)
  test = res$data
  zero = rowSums(test[, c("X", "Y", "Z")] == 0) == 3
  stopifnot(!any(zero))
  testthat::expect_true(max(abs(as.matrix(rg) - as.matrix(test))) == 0)

})



testthat::test_that("imputing zeros read.gt3x and py_read_gt3x agree", {
  skip_python()
  rg = read.gt3x::read.gt3x(path, verbose = FALSE,
                            imputeZeroes = TRUE,
                            debug = FALSE, asDataFrame = TRUE)
  rg = rg[, c("X", "Y", "Z", "time")]
  class(rg) = "data.frame"


  res = py_read_gt3x(path, verbose = FALSE)
  res = impute_zeros(res$data, res$dates, res$header)

  out = dplyr::inner_join(res, rg, by = "time", suffix = c("_py", "_rg"))
  testthat::expect_true(all(out$X_py == out$X_rg))
  testthat::expect_true(all(out$Y_py == out$Y_rg))
  testthat::expect_true(all(out$Z_py == out$Z_rg))

  out = dplyr::anti_join(rg, res, by = "time")
  testthat::expect_true(all(out$X == 0))
  testthat::expect_true(all(out$Y == 0))
  testthat::expect_true(all(out$Z == 0))

  range_dates = range(res$time)
  if (nrow(out) > 0) {
    range_aj = range(out$time)
    # test that all the non-joined values are outside of the data
    # aka nothing in the "middle"
    testthat::expect_false(
      any(dplyr::between(range_aj, range_dates[1], range_dates[2]))
    )
  }


  out = dplyr::anti_join(res, rg, by = "time")
  testthat::expect_true(nrow(out) == 0)


})




