Sybil <-
  R6::R6Class(
    classname = "Sybil",
    public = list(
      #' @description
      #' @param name description
      initialize = function(
        name_or_path = "sybil_ensemble",
        cache = "~/.sybil/",
        calibrator_path = NULL,
        device = NULL
      ) {
        self$name_or_path <- name_or_path
        self$cache <- cache
        self$calibrator_path <- calibrator_path
        self$device <- device
        self$sybil <- sybil$Sybil(
          name_or_path = self$name_or_path,
          cache = self$cache,
          calibrator_path = self$calibrator_path,
          device = self$device
        )
      },

      #' @field name_or_path Alias to a provided pretrained Sybil model or path
      #'   to a sybil checkpoint.
      name_or_path = NULL,

      #' @field cache Directory to download model checkpoints.
      cache = NULL,

      #' @field calibrator_path Path to calibrator pickle file corresponding
      #'   with the chosen model.
      calibrator_path = NULL,

      #' @field device If provided, will run inference using this device. By
      #'   default, uses GPU with the most free memory, if available.
      device = NULL,

      #' @field sybil Python `sybil.Sybil` object.
      sybil = NULL,

      #' @description Print method `Sybil` object.
      #' @param ... Not currently used.
      print = function(...) {
        cat(glue::glue("Sybil ({self$name_or_path})"))
      },

      evaluate = function(series, return_attentions = FALSE) {
        check_scalar_logical(return_attentions)
        check_scalar_integer(threads)

        if (is_series(series)) {
          series <- list(series)
        }
        check_list_series(series)

        if (!all(purrr::map_lgl(series, \(x) x$has_label()))) {
          rlang::abort(
            message = "`series` must be a list of `Series` with labels."
          )
        }

        self$sybil$evaluate(
          series = series,
          return_attentions = return_attentions,
          threads = threads
        )
      },

      predict = function(series, return_attentions = FALSE, threads = 0L) {
        check_scalar_logical(return_attentions)
        check_scalar_integer(threads)

        if (is_series(series)) {
          series <- list(series)
        }
        check_list_series(series)

        self$sybil$predict(
          series = series,
          return_attentions = return_attentions,
          threads = threads
        )
      }
    )
  )
