#' Visualize Attentions Maps
#'
#' Visualize attention map from a Sybil model for an input series. If `dir` is
#'   set, this function will write a GIF to disk for each series. The GIF is
#'   the input series with the attention maps superimposed to show which images
#'   are having the largest impact on the lung cancer risk prediction.
#'
#' @param series List of `Series`.
#' @param attentions List of attentions.
#' @param dir Directory to save GIFs.
#' @param gain Gain. Default is `3`.
#'
#' @return Images with attentions overlaid on the original series if `dir` is
#'   not set, otherwise returns `dir`, invisibly.
#'
#' @export
visualize_attentions <- function(series, attentions, dir = NULL, gain = 3) {
  check_scalar_integer(gain)
  if (!rlang::is_null(dir)) {
    check_scalar_character(dir)
    dir <- fs::path_expand(dir)
  }
  pyseries <- purrr::map(series, \(x) x@serie)
  series_with_attention <- sybil$visualize_attentions(
    pyseries,
    attentions = attentions,
    save_directory = dir,
    gain = gain
  )
  if (!rlang::is_null(dir)) {
    return(invisible(dir))
  }
  series_with_attention
}
