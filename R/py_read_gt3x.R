#' Read GT3X File
#'
#' @param path file to GT3X file
#' @param create_time Should time stamps be created?
#'
#' @return A list of meta data, accelerometry data, and time stamps
#' @export
#'
#' @examples
#'
#' url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
#' destfile = tempfile(fileext = ".zip")
#' dl = download.file(url, destfile = destfile)
#' gt3x_file = unzip(destfile, exdir = tempdir())
#' gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path = gt3x_file
#'
#' res = py_read_gt3x(path)
py_read_gt3x = function(path,
                        create_time = FALSE) {
  options(digits.secs = 2)
  import_path = system.file("gt3x", "gt3x", package = "pygt3x")
  gt3x = reticulate::import_from_path(
    "gt3x_functions", import_path,
    convert = FALSE)
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  path = unzip_zipped_gt3x(path, cleanup = TRUE)
  remove = attr(path, "remove")
  attr(path, "remove") = NULL
  out = gt3x$read_gt3x(path, create_time = create_time)
  if (attr(path, "remove")) {
    file.remove(path)
  }

  data = out[[0]]
  data = reticulate::py_to_r(data)
  data = round(data, 3)
  colnames(data) = c("X", "Y", "Z")
  data = tibble::as_tibble(data)

  meta = out[[2]]
  meta = reticulate::py_to_r(meta)
  meta$Start_Date = ticks2datetime(meta$Start_Date)
  meta$Stop_Date = ticks2datetime(meta$Stop_Date)
  meta$Download_Date = ticks2datetime(meta$Download_Date)
  meta$Sample_Rate = as.numeric(meta$Sample_Rate)

  dates = out[[1]]
  dates = reticulate::py_to_r(dates)

  np = reticulate::import("numpy")
  dates = np$asarray(dates, dtype='datetime64[s]')
  dates = c(dates)
  # convert the array to 64 bit milliseconds and add a time delta of a range of ms within a 1000ms window
  step_size = 1 / meta$Sample_Rate
  times = seq(0, 1 - step_size, by = step_size)
  dates = c(outer(times, dates, "+"))
  rm(times)



  rm(out)

  L = list(
    data = data,
    dates = dates,
    header = meta
  )
}


ticks2datetime = function (ticks, tz = "GMT")
{
  if (is.null(ticks)) {
    return(NULL)
  }
  ticks <- as.numeric(ticks)
  seconds <- ticks/1e+07
  datetime <- as.POSIXct(seconds, origin = "0001-01-01", tz = tz)
  datetime
}
