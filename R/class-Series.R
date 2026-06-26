#' Load a Series
#'
#' Load a CT series to estimate lung cancer risk.
#'
#' @param dicoms Character vector of DICOM files.
#' @param voxel_spacing Optional. Numeric vector of voxel spacings associated
#'   with the input CT as (row spacing, col spacing, slice thickness).
#' @param label Optional. Label for whether the individual has or ever developed
#'   lung cancer (`0`=No, `1`=Yes).
#' @param censor_time Optional. Number of years until the lung cancer diagnosis.
#'   If less than 1 year, set to `0`. Must be present if `label` is provided.
#' @param file_type Optional. File type of CT slices. One of `"png"` or
#'   `"dicom"`. Default is `"dicom"`.
#' @param split Optional. Data split that the series belongs to. One of
#'   `"train"`, `"dev"`, or `"test"`. Default is `"test"`.
#' @param x An `S7` object of class `"Series"`.
#'
#' @return An `S7` object of class `"Series"`.
#'
#' @export
Series <-
  S7::new_class(
    name = "Series",
    properties = list(
      dicoms = new_property(
        class = class_character,
        validator = function(value) {
          if (all(fs::file_exists(value))) {
            return(NULL)
          }
          "All files must exist."
        }
      ),
      voxel_spacing = new_property(
        class = class_numeric,
        validator = function(value) {
          if (rlang::is_empty(value) | length(value) == 3) {
            return(NULL)
          }
          "Must be a numeric vector of length 3: [row, col, slice]."
        }
      ),
      label = new_property(
        class = class_integer,
        default = NULL
      ),
      censor_time = new_property(
        class = class_integer,
        default = NULL
      ),
      file_type = new_property(
        class = class_character,
        default = "dicom",
        validator = function(value) {
          if (value %in% c("png", "dicom")) {
            return(NULL)
          }
          'Must be one of "png" or "dicom".'
        }
      ),
      split = new_property(
        class = class_character,
        default = "test",
        validator = function(value) {
          if (value %in% c("train", "dev", "test")) {
            return(NULL)
          }
          'Must be one of "train", "dev", or "test".'
        }
      ),
      num_images = new_property(
        class = class_integer,
        getter = function(self) {
          length(self@dicoms)
        }
      ),
      serie = new_property(
        class = class_environment,
        getter = function(self) {
          sybil$Serie(
            dicoms = self@dicoms,
            voxel_spacing = self@voxel_spacing,
            label = self@label,
            censor_time = self@censor_time,
            file_type = self@file_type,
            split = self@split
          )
        }
      )
    ),
    validator = function(self) {
      if (!rlang::is_empty(self@label) & rlang::is_empty(self@censor_time)) {
        "`censor_time` must be provided if `label` is provided."
      }

      if (rlang::is_empty(self@label) & !rlang::is_empty(self@censor_time)) {
        "`label` must be provided if `censor_time` is provided."
      }
    }
  )

#' @rdname Series
#' @export
has_label <- function(x) {
  if (rlang::is_empty(x@label)) {
    return(FALSE)
  }
  TRUE
}

method(print, Series) <- function(x) {
  cat(glue::glue("Series ({x@file_type}: {x@num_images} images)"))
}
