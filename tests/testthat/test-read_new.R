testthat::context("Reading in ACTIVITY2 Packets")


idf = list(
  name_gt3x = "PU3_CLE2B21130054_2017-03-16.gt3x.gz",
  download_url_gt3x = "https://ndownloader.figshare.com/files/21855807",
  id = "PU3", serial = "CLE2B21130054",
  name_csv = "PU3_CLE2B21130054_2017-03-16.csv.gz",
  download_url_csv = "https://ndownloader.figshare.com/files/24488492")
download = function(url, name) {
  destfile = file.path(tempdir(), name)
  if (!file.exists(destfile)) {
    download.file(url, destfile)
  }
  destfile
}
gt3x_file = download(idf$download_url_gt3x, idf$name_gt3x)
csv_file = download(idf$download_url_csv, idf$name_csv)

testthat::test_that("read.gt3x and py_read_gt3x agree", {
  skip_read_gt3x()
  rg = read.gt3x::read.gt3x(gt3x_file, verbose = 2,
                            debug = FALSE, asDataFrame = TRUE)
  at = attributes(rg)
  stopifnot(!at$old_version)
  rg = rg[, c("X", "Y", "Z")]
  class(rg) = "data.frame"


  res = pygt3x::py_read_gt3x(gt3x_file, verbose = FALSE)
  test = res$data
  zero = rowSums(test[, c("X", "Y", "Z")] == 0) == 3
  testthat::expect_true(!any(zero))
  testthat::expect_true(max(abs(as.matrix(rg) - as.matrix(test))) == 0)

})


testthat::test_that("CSV an py_read_gt3x agree", {
  skip_readr()
  csv = read_acc_csv(csv_file)
  hdr = csv$header
  csv = csv$data


  test = pygt3x::py_read_gt3x(gt3x_file, verbose = FALSE)
  thdr = test$header
  test =  pygt3x::impute_zeroes(test)
  same_time = test$time %in% csv$time
  testthat::expect_true(all(same_time))

  same_time = csv$time %in% test$time

  # just one Hz
  # testthat::expect_true(sum(!same_time) == thdr$Sample_Rate)

  # fixed!
  testthat::expect_true(sum(!same_time) == 0)

  csv = csv[same_time, ]
  testthat::expect_equal(dim(csv), dim(test))
  testthat::expect_true(all(test$time == csv$time))

  xyz = c("X", "Y", "Z")
  d = abs(csv[, xyz] - test[, xyz])
  bad = unname(rowSums(d) > 0)
  bad_gt3x = test[bad,xyz]
  testthat::expect_true(all(bad_gt3x == 0))
  rm(bad_gt3x)
  rm(d)

  skip_zoo()
  test = idle_na_locf(test)
  d = abs(csv[, xyz] - test[, xyz])
  testthat::expect_true(all(d == 0))
  rm(d)
  rm(csv)
  rm(test)

})
