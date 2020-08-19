
#' Read GT3X File
#'
#' @param file file to GT3X file
#'
#' @return A list of things
#' @export
#'
#' @examples
py_read_gt3x = function(file) {
  path = system.file("gt3x", package = "pygt3x")
  gt3x = reticulate::import_from_path("gt3x_functions", path)
  out = gt3x$read_gt3x(file)
}
