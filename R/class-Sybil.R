#' Load a Sybil Model
#'
#' Load a Sybil model to estimate lung cancer risk at 1 to 6 years. The Sybil
#'   model weights will be downloaded to disk on first use (to `cache`) and may
#'   take several minutes. More information about the Sybil model can be found
#'   in Mikhael et al. (2023) <doi:10.1200/jco.22.01345>.
#'
#' @param name_or_path Alias to a provided pretrained Sybil model or path
#'   to a Sybil checkpoint. Names must be one of `"sybil_base"`, `"sybil_1"`,
#'   `"sybil_2"`, `"sybil_3"`, `"sybil_4"`, `"sybil_5"`, or `"sybil_ensemble"`.
#'   Default is `"sybil_ensemble"`.
#' @param cache Directory to download model checkpoints. Default is in `.sybil/`
#'   in the users home directory (`~`).
#' @param calibrator_path Optional. Path to calibrator pickle file corresponding
#'   with the chosen model.
#' @param device Optional. If provided, will run inference using this device. By
#'   default, uses GPU with the most free memory, if available.
#'
#' @import S7
#'
#' @return An `S7` object of class `"Sybil"` with properties `@name_or_path`,
#'   `@cache`, `@calibrator_path`, `@device`, and `@model`. `@model` provides
#'   access to the Python Sybil model object. This is a dynamic property and
#'   will load a model each time the property is accessed in case model
#'   properties change.
#'
#' @examples
#' model <- Sybil()
#' @export
Sybil <- S7::new_class(
  name = "Sybil",
  properties = list(
    name_or_path = new_property(
      class = class_character,
      default = "sybil_ensemble",
      validator = function(value) {
        if (
          value %in%
            c(
              "sybil_base",
              "sybil_1",
              "sybil_2",
              "sybil_3",
              "sybil_4",
              "sybil_5",
              "sybil_ensemble"
            ) |
            fs::file_exists(value)
        ) {
          return(NULL)
        }
        'Must be one of "sybil_base", "sybil_1", "sybil_2", "sybil_3", "sybil_4", "sybil_5", "sybil_ensemble" or a path to a file on disk.'
      }
    ),
    cache = new_property(class_character, default = "~/.sybil/"),
    calibrator_path = new_property(class_character),
    device = new_property(class_character),
    model = new_property(
      class = class_environment,
      getter = function(self) {
        if (rlang::is_empty(self@calibrator_path)) {
          calib <- NULL
        } else {
          calib <- self@calibrator_path
        }

        if (rlang::is_empty(self@device)) {
          dev <- NULL
        } else {
          dev <- self@device
        }

        sybil$Sybil(
          name_or_path = self@name_or_path,
          cache = self@cache,
          calibrator_path = calib,
          device = dev
        )
      }
    )
  )
)

#' @rdname Sybil
#' @export
load_model <- function(
  name_or_path,
  cache = "~/.sybil/",
  calibrator_path = character(),
  device = character()
) {
  Sybil(name_or_path, cache, calibrator_path, device)
}

#' @rdname Sybil
#' @export
Sybil_base <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_base", cache, device)
}

#' @rdname Sybil
#' @export
Sybil_1 <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_1", cache, device)
}

#' @rdname Sybil
#' @export
Sybil_2 <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_2", cache, device)
}

#' @rdname Sybil
#' @export
Sybil_3 <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_3", cache, device)
}

#' @rdname Sybil
#' @export
Sybil_4 <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_4", cache, device)
}

#' @rdname Sybil
#' @export
Sybil_5 <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_5", cache, device)
}

#' @rdname Sybil
#' @export
Sybil_ensemble <- function(cache = "~/.sybil/", device = character()) {
  load_model("sybil_ensemble", cache, device)
}

method(print, Sybil) <- function(x) {
  cat(glue::glue("Sybil ({x@name_or_path})"))
}
