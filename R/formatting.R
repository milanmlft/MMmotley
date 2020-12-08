#' Format numbers as percentages
#'
#' This is a simple wrapper around [scales::label_percent()] to convert numeric
#' input to percentages and return them as characters.
#'
#' @param x         a numeric vector to format.
#' @param ...       other arguments passed on to [scales::label_percent()]
#'
#' @return A character vector of `length(x)`.
#' @export
#'
#' @examples
#' x <- seq(0, 1, length.out = 5)
#' format_percentage(x)
#'
format_percentage <- function(x, ...) {
  labeller <- scales::label_percent(...)
  labeller(x)
}


#' Format columns for printing
#'
#' Wraps around [base::format()] and [dplyr::mutate()] to format columns of a
#' `tibble` or `data.frame`. Note that any formatted columns will be converted
#' to `character` mode, so use this function only for printing.
#'
#' @param .data  A `data.frame` or `tibble`.
#' @param .cols <[`tidy-select`][dplyr::dplyr_tidy_select]> columns to format.
#'              If `NULL` (the default), will format all `double` columns.
#' @inheritDotParams format -x
#'
#' @return An object of the same type as `.data` with any formatted columns
#' converted to `character` mode.
#'
#' @seealso [format][base::format]
#' @export
#'
#' @examples
#' ## Convert to tibble for better printing
#' mtcars_tbl <- tibble::as_tibble(mtcars)
#'
#' ## Formatting all columns
#' format_cols(mtcars_tbl, digits = 0)
#'
#' ## Specific columns
#' format_cols(mtcars_tbl, c("wt", "qsec"), digits = 0)
#'
#' ## Other formatting options
#' format_cols(mtcars_tbl, scientific = TRUE)
#'
#' format_cols(mtcars_tbl, trim = TRUE)
#'
format_cols <- function(.data, .cols = NULL, ...) {
  if (is.null(.cols)) {
    dplyr::mutate(
      .data,
      dplyr::across(where(is.double), format, ...)
    )
  } else {
    dplyr::mutate(
      .data,
      dplyr::across(.cols = .cols, format, ...)
    )
  }
}

## Set `where` as a global variable, workaround for
## https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables("where")
