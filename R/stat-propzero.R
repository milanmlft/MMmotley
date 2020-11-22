#' WIP: annotating ggplots with prop. of zeros
#'
#' @param mapping
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
stat_propzero <- function(mapping = NULL, data = NULL, geom = "text",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  # TODO: replace `layer` with `annotate`?
  # Might allow more flexible positioning of text annotation
  ggplot2::layer(
    stat = StatPropZero, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# FIXME: figure out how to supply `format` arugment to format the proportions
# e.g. as percentages or rounded decimals
# ggproto object for stat_propzero
StatPropZero <- ggplot2::ggproto("StatPropZero", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {
    grouped <- dplyr::group_by(data, x)
    prop_zero <- dplyr::summarise(grouped, label = mean(y == 0))
    data.frame(x = prop_zero$x, y = 0, label = prop_zero$label)
  }
)
