#' Annotating ggplots with prop. of zeros
#'
#' @inheritParams ggplot2::stat_count
#' @param format.fun Function to format labels when using `geom = "text"` (the
#'                   default) to depict the proportion of zeros.
#'                   By default, uses [scales::label_percent()].
#'
#' @section Computed variables:
#' `stat_propzero()` provides the following variables:
#' \describe{
#'   \item{propzero}{proportion of zeros}
#' }
#'
#' @export

# TODO: add examples

stat_propzero <- function(mapping = NULL, data = NULL, geom = "text",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...,
                          format.fun = scales::label_percent()) {

  ggplot2::layer(
    stat = StatPropzero, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(format.fun = format.fun, na.rm = na.rm, ...)
  )
}

# ggproto object for stat_propzero
StatPropzero <- ggplot2::ggproto("StatPropzero", ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, format.fun = scales::label_percent()) {
    grouped <- dplyr::group_by(data, x)
    out <- dplyr::summarise(grouped, propzero = mean(y == 0))
    data.frame(
      x = out$x, y = 0, propzero = out$propzero,
      label = format.fun(out$propzero)
    )
  }
)
