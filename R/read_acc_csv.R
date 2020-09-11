#' Replace Values with Last Observation Carried Forward from Idle Sleep Mode
#'
#' @param df An object with columns `X`, `Y`, and `Z``
#'
#' @return A data set with the zeros filled in
#' @export
#' @examples
#' df = data.frame(
#'   X = c(0.3/sqrt(0.5), rep(0, 3)),
#'   Y = c(0.4/sqrt(0.5), rep(0, 3)),
#'   Z = c(0.5/sqrt(0.5), rep(0, 3)),
#'   stringsAsFactors = FALSE)
#' idle_na_locf(df)
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



#' Read ActiGraph Accelerometer CSV
#'
#' @param file CSV file to read in
#' @param ... additional arguments to pass to \code{\link{read_csv}}
#'
#' @return A list of the header and the data set
#' @export
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
  lubridate_func = utils::getFromNamespace(lubridate_func, "lubridate")
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
