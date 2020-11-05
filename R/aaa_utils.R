have_python_requirements = function(
  packages = pygt3x::pygt3x_required_modules
) {
  res = check_python_requirements(packages)
  if (any(!res)) {
    no_pkg = names(res)[!res]
    tfile = tempfile()
    dput(no_pkg, file = tfile)
    np = readLines(tfile)
    msg = paste0(paste(no_pkg, collapse = ", "),
                 " packages not found, please try",
                 " to install using reticulate::py_install(",
                 np, ")\n",
                 "pygt3x may not work")
    warning(msg)
  }
  return(all(res))
}

check_python_requirements = function(
  packages = pygt3x::pygt3x_required_modules
) {
  if (!reticulate::py_available(initialize = TRUE)) {
    return(FALSE)
  }
  n = names(packages)
  names(packages)[n == ""] = packages[n == ""]
  sapply(packages, reticulate::py_module_available)
  res = sapply(packages, reticulate::py_module_available)
  return(res)
}

install_python_requirements = function(
  packages = pygt3x::pygt3x_required_modules,
  force = FALSE
) {
  res = check_python_requirements(packages = packages)
  if (any(!res) || force) {
    no_pkg = names(res)[!res]
    x = try(reticulate::py_install(no_pkg, channel = c("defaults", "conda-forge", "bioconda")))
    res = check_python_requirements(packages = packages)
    if (inherits(x, "try-error") || any(!res)) {
      reticulate::py_install(no_pkg, pip = TRUE)
    }
  }
  res = check_python_requirements(packages = packages)
  return(res)
}
