#' Plot a multivariate time series of occurrence seasonally
#'
#' @inheritParams plot_multi
#'
#' @return A ggplot2 plot.
#'
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' plot_seasonal(ost, benthic:planktonic)
#' plot_seasonal(ost, benthic:planktonic, trans="sqrt")
#' # try to define a treshold
#' library("ggplot2")
#' plot_seasonal(ost, benthic:planktonic, trans="sqrt") +
#'   geom_hline(aes(yintercept=200000), colour="red")
plot_seasonal <- function(x, ..., trans="identity") {
  if (! "date" %in% names(x)) {
    stop("`x` should have a column called date")
  }
  if (!lubridate::is.Date(x$date)) {
    stop("`x$date` should be of class `Date`")
  }

  d <- select(x, date, ...) %>%
    mutate(
      yday=lubridate::yday(date),
      year=lubridate::year(date)
    ) %>%
    tidyr::gather(key="var", val="val", ...)
  ggplot(d, aes(yday, val)) + geom_point(size=0.5) + geom_path(size=0.25, alpha=0.5) + facet_grid(var~., scales="free_y") + scale_y_continuous(trans=trans)
}
