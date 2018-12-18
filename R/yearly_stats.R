#' Compute descriptive statistics per year
#'
#' @param date Vector of dates of observation, of class `Date`.
#' @param conc Vector of concentrations at these dates.
#' @param bloom_threshold Threshold value of concentration to consider that a bloom is occuring.
#'
#' @details Computes
#' - maximum abundance
#' - date of maximum abundance
#' - date of start and end of the bloom
#' - number of days of bloom
#' - integrated yearly abundance
#'
#' @import dplyr
#' @export
#'
#' @return Table with year and one column per statistic
#'
#' @examples
#' yearly_stats(ost$date, ost$benthic, bloom_threshold=200000)
yearly_stats <-  function(date, conc, bloom_threshold) {
  # checks
  if (! lubridate::is.Date(date)) {
    stop("`date` needs to be a Date object. Use as.Date() to convert it if needed")
  }
  if (! is.numeric(conc)) {
    stop("`conc` needs to be a number")
  }

  # interpolate to daily data, to make the rest of the computation easier
  dates <- seq(from=min(date), to=max(date), by=1)
  d <- tibble(dates, y=stats::approx(x=date, y=conc, xout=dates)$y) %>%
    rename(date=dates) %>%
    mutate(
      yday=lubridate::yday(date),
      year=lubridate::year(date),
      bloom=y>bloom_threshold
    )

  stats <- d %>% group_by(year) %>%
    summarise(
      # maximum concentration
      max_conc=max(y, na.rm=TRUE),
      # integrated yearly concentration
      integr_conc=sum(y, na.rm=TRUE),
      # dates
      # of max concentration
      day_max_conc=yday[which.max(y)],
      # of start and end of bloom
      day_start_bloom=min(yday[bloom]),
      day_end_bloom=max(yday[bloom]),
      # duraction of bloom
      nb_days_bloom=day_end_bloom-day_start_bloom
    )

  return(stats)
}
