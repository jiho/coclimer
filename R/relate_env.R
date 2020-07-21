#' Link abundances with environment
#'
#' @param y vector of the response variable: observed abundances/concentrations
#' @param env data.frame or matrix of environmental variables associated with these observations
#' @param n number of environmental variables to display, ordered in decreasing order of importance.
#' @param tau quantile to predict. By default this is 0.75 in order to focus on the observations of large concentrations rather than on the mean; those observations are more relevant for HABs, where the most important cases are those featuring large abundances.
#' @param min.node.size size of the nodes in the Random Forest trees. When this is large, this allows for more robust and smoother predictions; but making it too large just flattens the response curves.
#' @param grid.resolution resolution of the grid for partial dependence plots. Making this larger gives more precise plots but is longer to compute.
#' @param ... passed to `ranger::ranger()`
#'
#' @details This function performs a quantile-based regression of the response variable on environmental variables using the Random Forest algorithm. Then it computes partial dependence plots depicting the univariate effect of the `n` most relevant variables.
#'
#' @return A ggplot2 plot, with one subplot per variable, ordered in decreasing order of importance (the percentage of "importance" is in the label of the subplot; this is the percentage of the part of the variance that the model explains which is attributable to that variable = sums to 100% for all variables, but that does not mean that the model explains 100% of the variance in the data of course).
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' library("dplyr")
#'
#' # correlate raw benthic concentrations with a few variables
#' relate_env(ost$benthic, env=select(ost, chla, temperature, poc), n=3)
#'
#' # correlate only non-zero, transformed concentrations
#' ost_present <- filter(ost, benthic > 0)
#' conc <- sqrt(ost_present$benthic)
#' env <- select(ost_present, chla, temperature, poc)
#' relate_env(conc, env, n=3)
#'
#' # make a finer, but also more noisy, model
#' relate_env(conc, env, n=3, min.node.size=1, grid.resolution=50)
relate_env <- function(y, env, n=3, tau=0.75, min.node.size=5, grid.resolution=20, ...) {
  d <- data.frame(y, env)

  # fit a Random Forest regression of concentration on all environmental variables
  m <- ranger::ranger(y ~ ., data=d, importance="impurity", quantreg=TRUE, min.node.size=min.node.size, ...)
  print(m)

  # sort variable importance in decreasing order
  imp <- sort(m$variable.importance, decreasing=TRUE)
  (percent_imp <- round(imp/sum(imp)*100))

  # keep only the n most important variables
  n <- min(length(imp), n) # if there are fewer than n, keep all of them
  vars <- names(imp[1:n])
  vars_labels <- paste0(vars, " (", percent_imp[1:n], "%)")

  # custom prediction function which predicts a quantile
  pred_ranger <- function(object, newdata) {
    stats::predict(object, data=newdata, type="quantiles", quantiles=tau)$prediction %>% as.vector()
  }

  # for all relevant
  pd <- lapply(vars, function(v) {
    # predict the partial dependence plot
    pd <- pdp::partial(m, v, train=d, grid.resolution=grid.resolution, pred.fun=pred_ranger)
    # identify the variable
    names(pd)[1] <- "val"
    pd$var <- v
    return(unclass(pd))
  })
  pd <- dplyr::bind_rows(pd)

  # plot effect of relevant variables
  dt <- d %>%
    select(y, vars) %>% tidyr::gather(key="var", val="val", -y) %>%
    mutate(var=factor(var, levels=vars, labels=vars_labels))
  pd <- mutate(pd, var=factor(var, levels=vars, labels=vars_labels))

  ggplot(dt) + facet_wrap(~var, scales="free_x") +
    geom_point(aes(x=val, y), size=1, alpha=0.25, shape=16) +
    geom_line(aes(x=val, y=yhat, group=yhat.id), data=pd, alpha=10/length(unique(pd$yhat.id)), size=0.25, colour="#3366FF") +
    stat_summary(aes(x=val, y=yhat), data=pd, fun.y=median, col="#3366FF", geom="line") +
    theme(panel.grid.major.y=element_blank(), panel.grid.minor=element_blank(), axis.title.x=element_blank())
}
