Series <-
  R6::R6Class(
    classname = "Series",
    public = list(
      dicoms = NULL,
      voxel_spacing = NULL,
      censor_time = NULL,
      file_type = NULL,
      split = NULL,
      series = NULL,

      #' @param dicoms Character vector of DICOM file paths.
      #' @param voxel_spacing The voxel spacing associated with input CT
      #'   as `c(row spacing, col spacing, slice thickness)`.
      #' @param label Whether the patient associated with this series has or
      #'   ever developped cancer.
      #' @param censor_time Number of years until cancer diagnosis. If less than
      #'   1 year, should be 0.
      #' @param file_type File type of CT slices (`"png"` or `dicom`).
      #' @param split Dataset split into which the serie falls into.Assumed to
      #'   be test by default.
      initialize = function(
        dicoms,
        voxel_spacing = NULL,
        label = NULL,
        censor_time = NULL,
        file_type = "dicom",
        split = "test"
      ) {
        self$dicoms <- dicoms
        self$voxel_spacing <- voxel_spacing
        self$censor_time <- censor_time
        self$file_type <- file_type
        self$split <- split

        self$series <- sybil$Serie(
          dicoms = self$dicoms,
          voxel_spacing = self$voxel_spacing,
          label = label,
          censor_time = self$censor_time,
          file_type = self$file_type,
          split = self$split
        )
      },

      print = function() {
        base_str <- "Series"
        if (self$has_label()) {
          str <- glue::glue(
            "{base_str} ({self$file_type}, label: {self$get_label()})"
          )
        } else {
          str <- glue::glue("{base_str} ({self$file_type})")
        }

        cat(str)
      },

      get_label = function() {
        self$series$get_label()
      },

      has_label = function() {
        self$series$has_label()
      },

      get_raw_images = function() {
        self$series$get_raw_images()
      },

      get_volume = function() {
        self$series$get_volume()
      }
    ),

    active = list(
      label = function() {
        self$get_label()
      }
    )
  )
