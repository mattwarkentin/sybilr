#' Find or Select a DICOM Series
#'
#' Given a path to a directory of DICOM files on disk, either find the
#'   SeriesInstanceUIDs in the directory (`find_series_in_dir()`) or find the
#'   DICOM files belonging to a specific series (`find_dicoms_in_series()`) in
#'   the directory.
#'
#' @param path Path to folder of DICOM files.
#' @param series SeriesInstanceUID for a series of interest.
#'
#' @return For `find_series_in_dir()`, a character vector of DICOM
#'   SeriesInstanceUIDs. For `find_dicoms_in_series()`, a character vector of
#'   DICOM file belonging to `series`.
#'
#' @export
find_series_in_dir <- function(path) {
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
