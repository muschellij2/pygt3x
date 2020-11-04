testthat::test_that("install requirements", {
  testthat::skip_on_cran()
  if (!pygt3x:::have_python_requirements() &
      reticulate::py_available(initialize = FALSE)) {
    pygt3x:::install_python_requirements()
    testthat::expect_true(pygt3x:::have_python_requirements())
  }
})
