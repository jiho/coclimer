#' Look for correlations with environment
#'
#' @param y vector of the response variable: observed concentrations, presence/absence, etc.
#' @param env data.frame or matrix of environmental variables associated with these observations
#' @param n number of environmental variables to display, ordered in decreasing order of importance.
#' @param ... passed to `ranger::ranger()`
#'
#' @return A ggplot2 plot, with one subplot per variable, ordered in decreasing order of importance (the percentage of "importance measure" is in the label of the subplot; NB: this is *not* a percentage of variance explained).
#'
#' @export
#'
#' @examples
#' library("dplyr")
#'
#' # correlate raw benthic concentrations
#' correlate(ost$benthic, env=select(ost, chla:temperature), n=3)
#'
#' # correlate only non-zero, transformed concentrations
#' ost_present <- filter(ost, benthic > 0)
#' conc <- sqrt(ost_present$benthic)
#' env <- select(ost_present, chla:temperature)
#' correlate(conc, env, n=3)
#'
#' # correlate presence/absence only
#' correlate(factor(ost$benthic>0), env=select(ost, chla:temperature))
correlate <- function(y, env, n=6, ...) {
  d <- data.frame(y, env)

  # fit a Random Forest regression of concentration on all environmental variables
  m <- ranger::ranger(y ~ ., data=d, importance="permutation", ...)

  # sort variable importance in decreasing order
  imp <- sort(m$variable.importance, decreasing=TRUE)
  percent_imp <- round(imp/sum(imp)*100, 1)

  # keep only the n most important variables
  vars <- names(imp[1:n])
  vars_labels <- paste0(vars, " (", percent_imp[1:n], "%)")

  # plot effect of relevant variables
  dt <- d %>%
    select(y, vars) %>% tidyr::gather(key="var", val="val", -y) %>%
    mutate(
      var=factor(var, levels=vars, labels=vars_labels)
    )
  if (is.factor(dt$y)) {
    ggplot(dt, aes(x=y, y=val)) +
      geom_violin() + coord_flip() +
      facet_wrap(~var, scales="free_x")
  } else {
    ggplot(dt, aes(x=val, y=y)) +
      geom_point(size=1, alpha=0.5, shape=16) +
      geom_smooth(method="loess", se=FALSE) +
      facet_wrap(~var, scales="free_x")
  }
}
