#' Read GT3X File
#'
#' @param path file to GT3X file
#' @param create_time Should time stamps be created?
#' @param verbose print diagnostic messages
#'
#' @return A list of meta data, accelerometry data, and time stamps
#' @export
#'
#' @examples
#' reticulate::py_config()
#'
#' path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#' package = "pygt3x")
#' if (pygt3x:::have_python_requirements()) {
#' res = py_read_gt3x(path)
#' }
#'
#' \dontrun{
#' if (pygt3x:::have_python_requirements()) {
#'   out = impute_zeros(res$data, res$dates, res$header)
#'   out = impute_zeros(res)
#'
#'   url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
#'   destfile = tempfile(fileext = ".zip")
#'   dl = download.file(url, destfile = destfile, mode = "wb")
#'   gt3x_file = unzip(destfile, exdir = tempdir())
#'   gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#'   path = gt3x_file
#'
#'   res = py_read_gt3x(path)
#'   df = impute_zeros(res$data, res$dates, res$header)
#' }
#' }
py_read_gt3x = function(path,
                        create_time = FALSE,
                        verbose = FALSE) {
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  options(digits.secs = 2)
  import_path = system.file("gt3x", "gt3x", package = "pygt3x")
  check_python_requirements()
  gt3x = reticulate::import_from_path(
    "gt3x_functions", import_path,
    convert = FALSE)
  path = unzip_zipped_gt3x(path, cleanup = TRUE)
  remove = attr(path, "remove")
  attr(path, "remove") = NULL
  output = gt3x$read_gt3x(path, create_time = create_time,
                          verbose = verbose > 0,
                          trim = FALSE)
  if (remove) {
    file.remove(path)
  }

  if (verbose > 1) {
    print("output things")
    print(length(output))
    print(class(output))
  }
  data = output[[0]]
  if (verbose > 1) {
    print("data things py")
    print(class(data))
    print(data)
  }
  data = reticulate::py_to_r(data)
  if (verbose > 1) {
    print("data things")
    print(class(data))
    print(utils::head(data))
  }
  data = round_away_zero(data, 3)
  colnames(data) = c("X", "Y", "Z")
  data = tibble::as_tibble(data)
  if (any(abs(data) > 20)) {
    warning(
      paste0(
        "Really large values of X/Y/Z",
        "- rerun and see if still there",
        " also open issue on ",
        "https://github.com/muschellij2/pygt3x/issues")
    )
  }

  meta = output[[2]]
  meta = reticulate::py_to_r(meta)
  meta$Start_Date = ticks2datetime(meta$Start_Date)
  meta$Stop_Date = ticks2datetime(meta$Stop_Date)
  meta$Download_Date = ticks2datetime(meta$Download_Date)
  meta$Last_Sample_Time = ticks2datetime(meta$Last_Sample_Time)
  meta$Sample_Rate = as.numeric(meta$Sample_Rate)

  dates = output[[1]]
  dates = reticulate::py_to_r(dates)
  dates = c(dates)


  L = list(
    data = data,
    dates = dates,
    header = meta
  )

  L$dates_created = TRUE
  if (!inherits(dates, "POSIXt")  &
      !inherits(dates, "Date") &
      !inherits(dates, "POSIXct") &
      dates[1] == 0) {
    dates = meta$Start_Date + seq(0, nrow(data) - 1) / meta$Sample_Rate
  } else {
    nr = as.integer(nrow(data) / length(dates))
    if (! ( nr == as.integer(meta$Sample_Rate) ) ) {
      warning(
        paste0(
          "Size of data not length(dates) * sample_rate, ",
          "not making POSIXct dates, and returning data as is")
      )
      L$dates_created = FALSE
      return(L)
    }
    # np = reticulate::import("numpy")
    secs = seq(0, meta$Sample_Rate - 1)/meta$Sample_Rate
    dates = sapply(dates, function(x) x + secs)
    dates = c(dates)
    dates = as.POSIXct(dates, tz = "GMT", origin = "1970-01-01")
  }
  rm(output)
  L$dates = dates
  if (!is.null(meta$old_format) && meta$old_format) {
    if (!is.null(meta$est_n_samples)) {
      L$data = L$data[seq(meta$est_n_samples),]
      L$dates = L$dates[seq(meta$est_n_samples)]
    }
  }
  return(L)
}

#' Impute Zeros and Missing data
#'
#' @param data data output from \code{\link{py_read_gt3x}}
#' @param dates dates vector from \code{\link{py_read_gt3x}}
#' @param header header metadata from \code{\link{py_read_gt3x}}
#'
#' @return A tibble of the data with zeros
#' @export
impute_zeros = function(data, dates, header) {
  if (missing(dates) & missing(header)) {
    dates = data$dates
    header = data$header
    data = data$data
  }
  time = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "time"))

  stopifnot(
    !is.null(data),
    !is.null(dates),
    !is.null(header)
  )
  old_format = header$old_format
  if (is.null(old_format)) {
    old_format = FALSE
  }
  data$time = dates


  rdates = range(dates)
  rdates = range(
    lubridate::floor_date(rdates),
    lubridate::ceiling_date(rdates)
  )
  if (!is.null(header$Last_Sample_Time) && is.na(header$Last_Sample_Time)) {
    header$Last_Sample_Time = NULL
  }
  if (!old_format & !is.null(header$Last_Sample_Time)) {
    rdates[2] = max(rdates[2], header$Last_Sample_Time)
  }
  # rdates[2] = max(rdates[2], meta$Last_Sample_Time, meta$Stop_Date, na.rm = TRUE)
  rdates[1] = min(rdates[1], header$Start_Date)
  rdates = seq(rdates[1], rdates[2] - lubridate::as.period(1, "sec"), by = "sec")
  secs = seq(0, header$Sample_Rate - 1)/header$Sample_Rate
  rdates = sapply(rdates, function(x) x + secs)
  rdates = c(rdates)
  rdates = as.POSIXct(rdates, tz = "GMT", origin = "1970-01-01")
  df = tibble::tibble(time = rdates)
  if (old_format) {
    df = dplyr::filter(
      df, dplyr::between(time, min(data$time), max(data$time))
    )
  }
  df = dplyr::left_join(df, data, by= "time")
  df = dplyr::arrange(df, time)
  df = dplyr::mutate(df,
                     X = ifelse(is.na(X), 0, X),
                     Y = ifelse(is.na(Y), 0, Y),
                     Z = ifelse(is.na(Z), 0, Z))
  return(df)

}

#' @rdname impute_zeros
#' @export
impute_zeroes = impute_zeros


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

