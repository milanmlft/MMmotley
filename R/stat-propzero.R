#' Annotating ggplots with prop. of zeros
#'
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' `stat_propzero()` provides the following variables:
#' \describe{
#'   \item{propzero}{proportion of zeros}
#' }
#'
#' @export
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
  default_aes = aes(label = stat(propzero)),

  compute_group = function(data, scales) {
    grouped <- dplyr::group_by(data, x)
    out <- dplyr::summarise(grouped, propzero = mean(y == 0))
    data.frame(x = out$x, y = 0, propzero = out$propzero)
  }
)
