#' Predict Series using Sybil
#'
#' For a series or list of series, estimate lung cancer risk at 1 to 6 years
#'   and optionally attentions maps.
#'
#' @param object An `S7` object with class `"Sybil"`.
#' @param series An `S7` object with class `"Series"`.
#' @param return_attentions Logical. Whether to return attentions?
#'   Default is `FALSE`.
#' @param threads Number of CPU threads to use for PyTorch inference. Default
#'   is `0L`.
#' @param ... Not currently used.
#'
#' @return An `S7` object with properties `@scores`, and `@attentions`.
#'   `@attentions` will be an empty list unless `return_attentions` was `TRUE`.
#'
#' @name predict.Sybil
#' @rdname predict.Sybil
S7::method(predict, Sybil) <- function(
  object,
  series,
  return_attentions = FALSE,
  threads = 0L,
  ...
) {
  rlang::check_dots_empty()
  check_scalar_logical(return_attentions)
  check_scalar_integer(threads)

  if (!rlang::is_list(series) & S7::S7_inherits(series, Series)) {
    series <- list(series)
  }
  check_list_series(series)

  pyseries <- purrr::map(series, \(x) x@serie)

  res <- object@model$predict(pyseries, return_attentions, threads)

  scores <- res$scores

  attentions <- list()
  if (return_attentions) {
    attentions <- res$attentions
  }

  Prediction(res$scores, attentions)
}

Prediction <- S7::new_class(
  name = 'Prediction',
  properties = list(
    scores = class_list,
    attentions = class_list
  )
)
