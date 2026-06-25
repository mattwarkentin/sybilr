#' Evaluate a Series using Sybil
#'
#' For a list of labelled `Series`, estimate lung cancer risk predictions (and
#'   optionally attentions), and also the C-index and time-dependent area
#'   under the receiver operating characteristic curve (AUC) model performance
#'   metrics.
#'
#' @param object An `S7` object with class `"Sybil"`.
#' @param series An `S7` object with class `"Series"`.
#' @param return_attentions Logical. Whether to return attentions?
#'   Default is `FALSE`.
#' @param ... Not currently used.
#'
#' @return An `S7` object with properties `@auc`, `@c_index`, `@scores`, and
#'   `@attentions`, if `return_attentions` was `TRUE`.
#'
#' @export
evaluate <- function(object, series, return_attentions = FALSE, ...) {
  rlang::check_dots_empty()
  S7::check_is_S7(object, Sybil)
  check_scalar_logical(return_attentions)

  if (!rlang::is_list(series) & S7::S7_inherits(series, Series)) {
    series <- list(series)
  }
  check_list_series(series)

  if (!all(purrr::map_lgl(series, has_label))) {
    rlang::abort("`evaluate()` can only be used if all `Series` have labels.")
  }
  pyseries <- purrr::map(series, \(x) x@serie)
  res <- object@model$evaluate(pyseries, return_attentions)
  attentions <- list()
  if (return_attentions) {
    attentions <- res$attentions
  }
  Evaluation(
    auc = res$auc,
    c_index = res$c_index,
    scores = res$scores,
    attentions = attentions
  )
}

Evaluation <- S7::new_class(
  name = "Evaluation",
  properties = list(
    auc = class_list,
    c_index = class_numeric,
    scores = class_list,
    attentions = class_list
  )
)
