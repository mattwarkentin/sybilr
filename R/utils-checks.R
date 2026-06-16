check_scalar_logical <- function(x) {
  if (rlang::is_scalar_logical(x)) {
    return(invisible(x))
  }
  nm <- deparse(substitute(x))
  rlang::abort(
    message = glue::glue("`{nm}` must be a scalar logical.")
  )
}

check_scalar_character <- function(x) {
  if (rlang::is_scalar_character(x)) {
    return(invisible(x))
  }
  nm <- deparse(substitute(x))
  rlang::abort(
    message = glue::glue("`{nm}` must be a scalar character.")
  )
}

check_scalar_integer <- function(x) {
  if (rlang::is_scalar_integerish(x)) {
    return(invisible(x))
  }
  nm <- deparse(substitute(x))
  rlang::abort(
    message = glue::glue("`{nm}` must be a scalar integer.")
  )
}

check_scalar_numeric <- function(x) {
  if (rlang::is_scalar_double(x)) {
    return(invisible(x))
  }
  nm <- deparse(substitute(x))
  rlang::abort(
    message = glue::glue("`{nm}` must be a scalar numeric.")
  )
}

check_path_exists <- function(x) {
  if (fs::dir_exists(x)) {
    return(invisible(x))
  }
  rlang::abort(
    message = glue::glue("path does not exit: {x}")
  )
}

check_list_series <- function(x) {
  if (
    rlang::is_list(x) &&
      all(purrr::map_lgl(x, \(x) S7::S7_inherits(x, Series)))
  ) {
    return(invisible(x))
  }
  nm <- deparse(substitute(x))
  rlang::abort(
    message = glue::glue("`{nm}` must be a list of `Series` S7 objects.")
  )
}
