# yield
Calculate and visualize HIV testing yield

### This package is under development, use with caution.

An r package to estimate and visualize HIV testing yield under various hypothetical testing strategies.

yield can be installed by running

```r
library(devtools)
install_github("edwardsjk/yield")
```
## Functions 

The package contains the following functions. 
1. `yieldstrategy`: A function to estimate HIV testing yield under a single candidate strategy
2. `calcyield`: A function to summarize HIV testing yield under multiple strategies
3. `plotyield`: A function to plot results

## Inputs

All functions require inputting a dataset with one record per participant *not previously diagnosed with HIV*.  If data were obtained using a survey sampling approach, the package accommodates use of survey sampling weights and clustering variables. The package requires input datasets to contain columns of indicators of whether participants would have been tested under each candidate strategy.  Details on function calls can be found using `?yieldstrategy`, `?calcyield`, and `?plotyield`. 

## Outputs

`calcyield` returns a dataframe with one row per candidate strategy listing HIV testing yield and the expected number needed to test to identify 1 new case of HIV.

`plotyield` produces a forest plot of results.

## Use

A `shiny` app implementing this package using point-and-click interface is available at https://edwardsjk.shinyapps.io/yieldapp/ (under development, use with caution)