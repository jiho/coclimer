#' Plot a multivariate time series of occurrence
#'
#' @param x data.frame with a column called `date` and other columns for concentration of various species.
#' @param ... columns to plot, specified in a way compatible with `dplyr::select()`
#' @param trans name of a transformation function applied to y axis. Exemples are "identity" for no transformation "sqrt" for square root, "log10" for log in base 10, "log1p" for log(n+1).
#'
#' @return A ggplot2 plot.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' plot_multi(ost, benthic:planktonic)
#' plot_multi(ost, benthic:planktonic, trans="sqrt")
#' plot_multi(ost, benthic:planktonic, trans="log1p")
plot_multi <- function(x, ..., trans="identity") {
  if (!"date" %in% names(x)) {
    stop("`x` should have a column called date")
  }
  if (is.Date(x$date)) {
    stop("`x$date` should be of class `Date`")
  }

  d <- dplyr::select(x, date, ...) %>%
    tidyr::gather(key="var", val="val", ...)
  ggplot(d, aes(date, val)) + geom_point(size=0.5) + geom_path(size=0.25, alpha=0.5) + facet_grid(var~., scales="free_y") + scale_y_continuous(trans=trans)
}
