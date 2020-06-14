#' Create p-value histogram
#'
#' Use [ggplot2::geom_histogram()] to create a histogram of p-values but with
#' better binwidths and axis limits.
#'
#' @details
#' By default, [ggplot2::geom_histogram()] centers the first and last bins of
#' the histogram on the x-axis lower and upper limits, respectively.
#' In case of p-values, which lie in the interval \[0, 1\], it makes more
#' sense to set the boundaries of the first and last bins at 0 and 1.
#' The binwidths are also set at 0.05, commonly used as a cut-off for significance,
#' so that the height of the first bar represents the number of p-values lying
#' between 0 and 0.05, i.e. the number of cases that would be deemed significant.
#'
#' @param p_values Vector of p-values.
#'
#' @return A ggplot object.
#'
#' @examples
#' pvals <- runif(n = 1000, min = 0, max = 1)
#' gg_pval_hist(pvals)
#'
#' @export
#' @importFrom ggplot2 ggplot aes
gg_pval_hist <- function(p_values) {
  # Check input
  if (!is.double(p_values)) {
    stop("`p_values` must be a numeric vector:\n",
      "You have provided an object of class: ", class(p_values)[1],
      call. = FALSE
    )
  }
  if (any(p_values < 0) || any(p_values > 1)) {
    stop("`p_values` should be between 0 and 1.")
  }

  ggplot(mapping = aes(x = p_values)) +
    ggplot2::geom_histogram(
      fill = "grey65", col = "black",
      binwidth = 0.05, boundary = 0
    ) +
  	# Sets x-axis ticks at intervals of 0.1
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  	# Lets the bottoms of the histogram bars touch the x-axis
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
}
