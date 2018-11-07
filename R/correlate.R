#' Look for correlations with environment
#'
#' @param y vector of the response variable: observed concentrations, presence/absence, etc.
#' @param env data.frame or matrix of environmental variables associated with these observations
#' @param n number of environmental variables to display, ordered in decreasing order of importance.
#' @param ... passed to `ranger::ranger()`
#'
#' @return A ggplot2 plot, with one subplot per variable, ordered in decreasing order of importance (the percentage of "importance" is in the label of the subplot; for regression this is the percentage of the part of the variance that the model explains which is attributable to that variable = sums to 100% for all variables, but that does not mean that the model explains 100% of the variance in the data of course).
#'
#' @export
#' @import ggplot2
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
  m <- ranger::ranger(y ~ ., data=d, importance="impurity", keep.inbag=TRUE, ...)

  # predict effect of relevant variables
  ranges <- lapply(env, function(x) {
    seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), length.out=100)
  })
  means <- lapply(env, mean, na.rm=TRUE)
  pred <- lapply(1:ncol(env), function(i) {
    grid <- data.frame(ranges[i], means[-i])
    # NB: prediction with SE fails sometimes
    # pred <- predict(m, data=grid, type="se")
    # pred <- data.frame(pred[c("predictions", "se")]) %>%
    #   rename(y=predictions)
    # pred$var <- names(env)[i]
    # pred$val <- ranges[[i]]
    pred <- predict(m, data=grid, type="response")
    data.frame(
      y=pred$predictions,
      var=names(env)[i],
      val=ranges[[i]]
    )
  }) %>% do.call(bind_rows, .)

  # sort variable importance in decreasing order
  imp <- sort(m$variable.importance, decreasing=TRUE)
  percent_imp <- round(imp/sum(imp)*100, 1)

  # keep only the n most important variables
  vars <- names(imp[1:n])
  vars_labels <- paste0(vars, " (", percent_imp[1:n], "%)")

  # plot effect of relevant variables
  dt <- d %>%
    select(y, vars) %>% tidyr::gather(key="var", val="val", -y) %>%
    mutate(var=factor(var, levels=vars, labels=vars_labels))

  dp <- pred %>%
    filter(var %in% vars) %>%
    mutate(var=factor(var, levels=vars, labels=vars_labels))

  if (is.factor(dt$y)) {
    ggplot(dt, aes(x=y, y=val)) +
      geom_violin() + coord_flip() +
      facet_wrap(~var, scales="free_x")
  } else {
    ggplot(dt, aes(x=val, y=y)) +
      geom_point(alpha=0.4, shape=16) +
      # geom_smooth(method="loess", se=FALSE) +
      # geom_ribbon(aes(ymin=y-se, ymax=y+se), data=dp, alpha=0.5, fill="dodgerblue") +
      geom_path(data=dp, colour="dodgerblue", size=1) +
      facet_wrap(~var, scales="free_x")
  }
}
