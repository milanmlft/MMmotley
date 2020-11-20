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
