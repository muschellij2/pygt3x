url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
destfile = tempfile(fileext = ".zip")
dl = download.file(url, destfile = destfile)
gt3x_file = unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
path = gt3x_file

testthat::test_that("Reading in Old format works", {
  res = pygt3x::py_read_gt3x(path, verbose = FALSE)
  res = pygt3x::impute_zeros(res$data, res$dates, res$header)
  testthat::expect_equal(colnames(res), c("time", "X", "Y", "Z"))

  cm <- unname(colMeans(res[, c("X", "Y", "Z")]))
  testthat::expect_equal(cm, c(
    -0.228402625555557,
    0.447592941851854,
    0.11958707074074
  ), tolerance = 1e-5)
  testthat::expect_equal(unname(res[4823, "Y", drop = TRUE]), 0.528)

  all_attr <- attributes(res)
  rm(res)
})

testthat::test_that("OLD read.gt3x and py_read_gt3x agree", {
  testthat::skip("Not working")
  skip_read_gt3x()
  rg = read.gt3x::read.gt3x(path, verbose = FALSE,
                            asDataFrame = TRUE)
  rg_time = rg$time
  rg = rg[, c("X", "Y", "Z")]
  class(rg) = "data.frame"


  res = pygt3x::py_read_gt3x(path, verbose = FALSE)
  test = pygt3x::impute_zeros(res$data, res$dates, res$header)
  max(test$X)
  max(test$Y)
  max(test$Z)
  zero = rowSums(test[, c("X", "Y", "Z")] == 0) == 3
  d = abs(as.matrix(rg) - as.matrix(test[, c("X", "Y", "Z")]))
  bad = which(rowSums(d > 0) > 0)
  test[bad,]
  testthat::expect_true(max(d) == 0)

})



testthat::test_that("imputing zeros read.gt3x and py_read_gt3x agree", {
  testthat::skip("Not working")
  skip_read_gt3x()
  rg = read.gt3x::read.gt3x(path, verbose = FALSE,
                            imputeZeroes = TRUE,
                            asDataFrame = TRUE)
  rg = rg[, c("X", "Y", "Z", "time")]
  class(rg) = "data.frame"
  imp_rg = rg

  rg = read.gt3x::read.gt3x(path, verbose = FALSE,
                            imputeZeroes = TRUE,
                            asDataFrame = TRUE)
  rg = rg[, c("X", "Y", "Z", "time")]
  class(rg) = "data.frame"

  testthat::expect_equal(rg, imp_rg)


  res = py_read_gt3x(path, verbose = FALSE)
  res = impute_zeros(res$data, res$dates, res$header)

  # same_time = lubridate::floor_date(res$time) == lubridate::floor_date(rg$time)
  # all()
  d =  abs(as.matrix(rg[, c("X", "Y", "Z")]) - as.matrix(res[, c("X", "Y", "Z")]))
  testthat::expect_true( max(d  == 0))


  out = dplyr::inner_join(res, rg, by = "time", suffix = c("_py", "_rg"))
  testthat::expect_true(all(out$X_py == out$X_rg))
  testthat::expect_true(all(out$Y_py == out$Y_rg))
  testthat::expect_true(all(out$Z_py == out$Z_rg))

  out = dplyr::anti_join(rg, res, by = "time")
  testthat::expect_true(all(out$X == 0))
  testthat::expect_true(all(out$Y == 0))
  testthat::expect_true(all(out$Z == 0))

  range_dates = range(res$time)
  range_aj = range(out$time)
  # test that all the non-joined values are outside of the data
  # aka nothing in the "middle"
  testthat::expect_false(
    any(dplyr::between(range_aj, range_dates[1], range_dates[2]))
  )


  out = dplyr::anti_join(res, rg, by = "time")
  testthat::expect_true(nrow(out) == 0)


})




