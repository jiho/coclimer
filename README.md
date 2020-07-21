# coclimer

R package to standardise statistical analyses of time series in the
CoClime project.

## Installation

The package is not on CRAN. Install it with

``` r
# if needed
install.packages("remotes")
# then
remotes::install_github("jiho/coclimer")
```

## Usage

Load the package

``` r
library("coclimer")
```

A test dataset is included in the package, containing the concentration
of *Ostreopsis ovata* in two forms and environemental variables. Read
more about it with

``` r
?ost
```

Make it available and inspect it with

``` r
data(ost)
head(ost)
```

    ## # A tibble: 6 x 13
    ##   date       benthic planktonic  chla   no2   no3 oxygen    po4   poc   pon
    ##   <date>       <dbl>      <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
    ## 1 2007-01-01       0          0 0.416 0.077 0.438   5.64 0.08    78.1  7.9 
    ## 2 2007-01-08       0          0 0.416 0.077 0.438   5.64 0.08    78.1  7.9 
    ## 3 2007-01-15       0          0 0.367 0.129 0.378   5.62 0.0873  60.0  5.84
    ## 4 2007-01-22       0          0 0.309 0.189 0.309   5.65 0.0958  38.8  3.44
    ## 5 2007-01-29       0          0 0.351 0.209 0.462   5.73 0.243   41.2  6.19
    ## 6 2007-02-05       0          0 0.398 0.336 0.654   5.80 0.145   60.9  3.87
    ## # … with 3 more variables: salinity <dbl>, sioh4 <dbl>, temperature <dbl>

Your data should be made to look the same: a `date` column, columns for
species concentrations/abundances, columns for environmental variables.

Functions `plot_multi()` and `plot_seasonnal()` allow to represent the
data of multiple series graphically

``` r
# full time series
plot_multi(ost, benthic:planktonic)
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

``` r
plot_multi(ost, benthic:planktonic, trans="sqrt")
```

![](README_files/figure-gfm/plot-2.png)<!-- -->

``` r
# seasonal view
plot_seasonal(ost, benthic:planktonic, trans="sqrt")
```

![](README_files/figure-gfm/plot-3.png)<!-- -->

The function `yearly_stats()` allows to compute standardised statistics
for each year.

``` r
# for benthic concentration
yearly_stats(ost$date, ost$benthic, bloom_threshold=200000)
```

    ## # A tibble: 11 x 7
    ##     year max_conc integr_conc day_max_conc day_start_bloom day_end_bloom
    ##    <dbl>    <dbl>       <dbl>        <dbl>           <dbl>         <dbl>
    ##  1  2007 1565514.   12076718.          183             177           189
    ##  2  2008  359865.    7606196.          197             191           204
    ##  3  2009  481637.    5229863.          180             176           185
    ##  4  2010  443729.    6993577.          193             190           205
    ##  5  2011  755554.   20583752.          207             189           222
    ##  6  2012  293989.    7575923.          191             188           235
    ##  7  2013  871727.   14008834.          203             197           235
    ##  8  2014  878630.   10595108.          196             185           200
    ##  9  2015  212300.    4037950.          180             180           180
    ## 10  2016  288864.    7033000.          214             212           222
    ## 11  2017  469755.   17286613.          198             175           223
    ## # … with 1 more variable: nb_days_bloom <dbl>

To regress abundances of HAB-forming organisms on environmental
variables, use the function `relate_env()`. This computes a
quantile-based regression of abundances on all environmental variables
using the Random Forest algorithm, computes partial dependence plots for
the most relevant variables and plots them.

``` r
suppressMessages(library("tidyverse"))
d <- filter(ost, benthic>0) %>% as.data.frame()
relate_env(sqrt(d$benthic), select(d, chla:temperature), n=3)
```

    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(y ~ ., data = d, importance = "impurity", quantreg = TRUE,      min.node.size = min.node.size, ...) 
    ## 
    ## Type:                             Regression 
    ## Number of trees:                  500 
    ## Sample size:                      122 
    ## Number of independent variables:  10 
    ## Mtry:                             3 
    ## Target node size:                 5 
    ## Variable importance mode:         impurity 
    ## Splitrule:                        variance 
    ## OOB prediction error (MSE):       56327.04 
    ## R squared (OOB):                  -0.02175949

    ## Warning: `fun.y` is deprecated. Use `fun` instead.

![](README_files/figure-gfm/corr-1.png)<!-- -->

Read the help of each function for more information.
