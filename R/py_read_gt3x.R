
#' Read GT3X File
#'
#' @param file file to GT3X file
#'
#' @return A list of meta data, accelerometry data, and time stamps
#' @export
#'
#' @examples
py_read_gt3x = function(file) {
  options(digits.secs = 2)
  path = system.file("gt3x", "gt3x", package = "pygt3x")
  gt3x = reticulate::import_from_path(
    "gt3x_functions", path,
    convert = FALSE)
  out = gt3x$read_gt3x(file)
  data = out[[0]]
  data = reticulate::py_to_r(data)
  colnames(data) = c("X", "Y", "Z")
  data = tibble::as_tibble(data)
  dates = out[[1]]
  dates = reticulate::py_to_r(dates)
  data$time = dates
  rm(dates)
  meta = out[[2]]
  meta = reticulate::py_to_r(meta)
  rm(out)

  L = list(

  )
}
