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
#' @param binwidth Binwidth of the histogram. A sensible choice is to use the
#'                 significance cut-off so that the height of the first bar
#'                 corresponds to the number of significant cases.
#'                 Default: 0.05.
#'
#' @return A ggplot object.
#'
#' @examples
#' pvals <- runif(n = 1000, min = 0, max = 1)
#' gg_pval_hist(pvals)
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram scale_x_continuous scale_y_continuous expansion
gg_pval_hist <- function(p_values, binwidth = 0.05) {
  # Check input
  if (!is.double(p_values)) {
    stop("`p_values` must be a numeric vector:\n",
      "You have provided an object of class: ", class(p_values)[1],
      call. = FALSE
    )
  }
  if (any(is.na(p_values))) {
    stop("`p_values` contains missing values", call. = FALSE)
  }
  if (any(p_values < 0) || any(p_values > 1)) {
    stop("`p_values` should be between 0 and 1.", call. = FALSE)
  }

  ggplot(mapping = aes(x = p_values)) +
    geom_pval_hist(
      fill = "grey65", col = "black",
      binwidth = binwidth
    ) +
    # Sets x-axis ticks at intervals of 0.1
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    # Lets the bottoms of the histogram bars touch the x-axis
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
}


#' @inheritParams ggplot2::geom_histogram
#' @inheritDotParams ggplot2::geom_histogram -mapping
#'
#' @rdname gg_pval_hist
#'
#' @examples
#' ## Using a data.frame in combination with `ggplot`
#' library(ggplot2)
#' df <- data.frame(pvalue = runif(1000))
#' ggplot(df, aes(pvalue)) + geom_pval_hist()
#'
#' @export
#' @importFrom ggplot2 geom_histogram
geom_pval_hist <- function(mapping = NULL, ..., binwidth = 0.05) {
  geom_histogram(
    mapping = mapping,
    ...,
    binwidth = binwidth,
    boundary = 0
  )
}



#' WIP: annotating ggplots with prop. of zeros
#'
#' @param mapping
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_propzero <- function(mapping = NULL, ...) {
  prop_zero <- data %>%
    group_by(group) %>%
    summarise(prop = mean(y == 0))

  geom_text(
    data = prop_zero,
    aes(group, 0, label = paste("Prop. zero:", format_percentage(prop))),
    col = "black"
  )
}



#' Save multiple plots to a single PDF
#'
#' Takes a list of plots and exports them to a single PDF with one plot per page.
#'
#' @param plot_list List of plot objects. Can be any type as long as
#'                  it has a `print()` output.
#' @param filename  Character string. Can also be specified as path.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'
#'   # Generate list of plots
#'   mtcars_split <- split(mtcars, mtcars$cyl)
#'   plot_list <- lapply(mtcars_split, function(d) {
#'     ggplot(d, aes(mpg, wt)) +
#'       geom_point()
#'   })
#'
#'   # Save plots
#'   save_plots(plot_list, "plots.pdf")
#' }
#'
#' @export
#' @importFrom grDevices pdf dev.off
save_plots <- function(plot_list, filename) {
  pdf(filename)
  invisible(lapply(plot_list, print))
  dev.off()
}
