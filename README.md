<!-- README.md is generated from README.Rmd. Please edit that file -->
estudy2
=======

An implementation of a most commonly used event study methodology, including both parametric and nonparametric tests. It contains variety aspects of the rate of return estimation (the core calculation is done in C++), as well as three classical market models: mean adjusted returns, market adjusted returns and single-index market models. There are 6 parametric and 6 nonparametric tests provided, which examine cross-sectional daily abnormal return (see the documentation of the functions for more information). Furthermore, tests for the cumulative abnormal returns are included.

Instalation
-----------

The package is not submitted to CRAN, and is located in a private repository. If one has an access to the github repo, it is possible to install the last development version:

``` r
# install.packages("devtools")
devtools::install_github("irudnyts/estudy2")
```
