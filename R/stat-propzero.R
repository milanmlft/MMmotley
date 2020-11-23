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
#' @examples
#' library(ggplot2)
#' ## Generate some random zero-inflated data
#' n <- 100
#' p <- 0.123
#' y <- ifelse(rbinom(n, size = 1, prob = p) > 0, 0, rpois(n, lambda = 2))
#' df <- data.frame(x = rep(c("A", "B"), each = n / 2), y = y)
#'
#' ## Make plot
#' p <- ggplot(df, aes(x, y)) + geom_jitter(alpha = 0.2, width = 0.1)
#' p + stat_propzero()
#'
#' ## Use a different formatting function
#' p + stat_propzero(format.fun = scales::label_scientific())
#' p + stat_propzero(format.fun = function(x) format(x, digits = 1))
#' p + stat_propzero(format.fun = scales::label_percent(accuracy = 0.01))
#'
#' ## Adjust label position with `hjust` and `vjust`
#' p + stat_propzero(vjust = 3)
#' p + stat_propzero(hjust = 3)
#' p + stat_propzero(hjust = 3, vjust = 3)
#'
#' ## Using a different `geom` than text
#' ## This will require mapping of the `propzero` stat to another aesthetic
#' ## with `after_stat`.
#' p + stat_propzero(aes(col = after_stat(propzero)), geom = "point", size = 5)
#' p + stat_propzero(aes(size = after_stat(propzero)), geom = "point")
#'
#' @export
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
