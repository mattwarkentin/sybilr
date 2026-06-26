#' Find a DICOM Series
#'
#' Given a `path` to a directory of DICOM files on disk, either find the set of
#'   SeriesInstanceUIDs in the directory (`find_series_in_dir()`) or find the
#'   DICOM files belonging to a specific series (`find_dicoms_in_series()`) in
#'   the directory.
#'
#' @param path Path to a folder of DICOM files.
#' @param series SeriesInstanceUID for a series of interest.
#'
#' @return For `find_series_in_dir()`, a character vector of DICOM
#'   SeriesInstanceUIDs. For `find_dicoms_in_series()`, a character vector of
#'   DICOM files belonging to the `series`.
#'
#' @export
find_series_in_dir <- function(path) {
  check_scalar_character(path)
  if (!fs::dir_exists(path)) {
    rlang::abort(
      message = "`path` does not exist."
    )
  }
  path <- fs::path_real(path)
  reader <- sitk$ImageSeriesReader()
  series <- reader$GetGDCMSeriesIDs(path)
  as.character(series)
}

#' @rdname find_series_in_dir
#' @export
find_dicoms_in_series <- function(path, series) {
  check_scalar_character(path)
  check_scalar_character(series)
  if (!fs::dir_exists(path)) {
    rlang::abort(
      message = "`path` does not exist."
    )
  }
  path <- fs::path_real(path)
  reader <- sitk$ImageSeriesReader()
  series_files <- reader$GetGDCMSeriesFileNames(path, series)
  as.character(series_files)
}
