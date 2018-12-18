#' Compute descriptive statistics per year
#'
#' @param s Data.frame output by `yearly_stats()`.
#' @param type Type of trend to test: "monotonous" for Mann-Kendall test for a monotonous trend (and plot of a loose polynomial fit to the data), "median" for a quantile regression of the median (and a plot of the corresponding regression result).
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @return Table with year and one column per statistic
#'
#' @examples
#' s <- yearly_stats(ost$date, ost$benthic, bloom_threshold=200000)
#' yearly_trend(s)
#' yearly_trend(s, type="median")
yearly_trend <- function(s, type=c("monotonous", "median")) {
  type <- match.arg(type)

  st <- tidyr::gather(s, key="stat", val="val", -year) %>%
    mutate(stat=factor(stat, levels=unique(stat)))

  if (type == "monotonous") yearly_trend_mono(st)
  if (type == "median") yearly_trend_quant(st)
}

yearly_trend_mono <- function(st) {
  tt <- st %>%
    group_by(stat) %>%
    do({
      mkt <- trend::mk.test(.$val)
      test <- broom::tidy(mkt) %>% select(statistic:p.value)
    }) %>%
    ungroup()

  st <- left_join(st, tt, by="stat")

  ggplot(st, aes(year, val)) +
    geom_smooth(aes(linetype=p.value<0.05), se=F, method="loess", span=length(unique(st$year))/5) +
    scale_linetype_manual(values=c("11", "solid"), guide="none") +
    geom_point() +
    facet_wrap(~stat, scales="free_y") +
    scale_x_continuous(breaks=seq(min(s$year), max(s$year), by=2))
}

yearly_trend_quant <- function(st) {
  st <- st %>%
    group_by(stat) %>%
    do({
      m <- quantreg::rq(val ~ year, data=., tau=0.5)
      x <- broom::augment(m, interval="confidence", type="percentile", se="boot", R=1000, bsmethod="xy")
      test <- broom::tidy(m, se="boot", R=1000, bsmethod="xy") %>% select(statistic:p.value)
      test <- test[1,]
      x$p.value <- test$p.value
      x
    }) %>%
    ungroup()

  ggplot(st, aes(year, val)) +
    geom_ribbon(aes(ymin=.conf.low, ymax=.conf.high), alpha=0.1, fill="#3366FF") +
    geom_path(aes(y=.fitted, linetype=p.value < 0.05), colour="#3366FF", size=1) +
    scale_linetype_manual(values=c("11", "solid")) +
    geom_point() +
    facet_wrap(~stat, scales="free_y") +
    scale_x_continuous(breaks=seq(min(s$year), max(s$year), by=2))
}