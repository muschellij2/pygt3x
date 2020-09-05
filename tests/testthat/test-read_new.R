testthat::context("Reading in ACTIVITY2 Packets")

idle_na_locf = function(df) {
  zero = unname(rowSums(df[, c("X", "Y", "Z")] == 0) == 3)
  df$X[zero] = NA
  df$Y[zero] = NA
  df$Z[zero] = NA
  df$X = zoo::na.locf(df$X, na.rm = FALSE)
  df$Y = zoo::na.locf(df$Y, na.rm = FALSE)
  df$Z = zoo::na.locf(df$Z, na.rm = FALSE)

  df$X[is.na(df$X)] = 0
  df$Y[is.na(df$Y)] = 0
  df$Z[is.na(df$Z)] = 0
  df
}

sub_thing = function(hdr, string) {
  x = hdr[grepl(string, hdr)]
  x = gsub(string, "", x)
  x = trimws(x)
}

read_acc_csv = function(file, ...) {
  hdr = readLines(file, n = 10)
  st = sub_thing(hdr, "Start Time")
  sd = sub_thing(hdr, "Start Date")
  format = sub(".*date format (.*) at.*", "\\1", hdr[1])
  if (format == "") {
    warning("No format for date in the header, using mdy")
    format = "mdy"
  } else {
    format = tolower(format)
    format = c(sapply(strsplit(format, "/"), substr, 1,1))
    format = paste(format, collapse = "")
  }
  all_formats = c("ydm", "dym", "ymd", "myd", "dmy", "mdy")
  stopifnot(format %in% all_formats)
  lubridate_func = paste0(format, "_hms")
  lubridate_func = getFromNamespace(lubridate_func, "lubridate")
  start_date = do.call(lubridate_func, args = list(paste0(sd, " ", st)))
  srate = as.numeric(sub(".*at (\\d*) Hz.*", "\\1", hdr[1]))

  suppressWarnings({
    df = readr::read_csv(
      file, skip = 10,
      col_types = readr::cols(
        .default = readr::col_double(),
        Date = readr::col_character(),
        Time = readr::col_time(format = "")
      ), ...)
  })
  readr::stop_for_problems(df)



  df$time = seq(0, nrow(df) - 1)/srate
  df$time = start_date + df$time
  class(df) = "data.frame"
  colnames(df) = trimws(sub("Accelerometer", "", colnames(df)))

  stopifnot(!anyNA(df$time))
  df = df[, c("time", "X", "Y", "Z")]
  list(
    header = hdr,
    data = df
  )
}

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
  stopifnot(!any(zero))
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
  testthat::expect_true(sum(!same_time) == thdr$Sample_Rate)

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
