% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_multi.R
\name{plot_multi}
\alias{plot_multi}
\title{Plot a multivariate time series of occurrence}
\usage{
plot_multi(x, ..., trans = "identity")
}
\arguments{
\item{x}{data.frame with a column called \code{date} and other columns for concentration of various species.}

\item{...}{columns to plot, specified in a way compatible with \code{dplyr::select()}}

\item{trans}{name of a transformation function applied to y axis. Exemples are "identity" for no transformation "sqrt" for square root, "log10" for log in base 10, "log1p" for log(n+1).}
}
\value{
A ggplot2 plot.
}
\description{
Plot a multivariate time series of occurrence
}
\examples{
plot_multi(ost, benthic:planktonic)
plot_multi(ost, benthic:planktonic, trans="sqrt")
plot_multi(ost, benthic:planktonic, trans="log1p")
}
