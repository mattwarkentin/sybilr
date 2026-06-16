sybil <- NULL
sitk <- NULL

.onLoad <- function(libname, pkgname) {
  Sys.setenv(RETICULATE_PYTHON = "managed")
  reticulate::py_require(
    packages = c(
      "scikit-learn==1.0.2",
      "lifelines==0.26.4",
      "sybil@git+https://github.com/reginabarzilaygroup/Sybil.git"
    ),
    python_version = "3.10"
  )
  sybil <<- reticulate::import("sybil", delay_load = TRUE)
  sitk <<- reticulate::import("SimpleITK", delay_load = TRUE)
  S7::S7_on_load()
}

S7::S7_on_build()
