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
format_percentage <- function(x, ...) {
	labeller <- scales::label_percent(...)
	labeller(x)
}
