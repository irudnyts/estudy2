<!-- README.md is generated from README.Rmd. Please edit that file -->
estudy2
=======

[![Travis-CI Build Status](https://travis-ci.com/irudnyts/estudy2.svg?token=Spwopnmy37EZMsL4nDza&branch=master)](https://travis-ci.com/irudnyts/estudy2.svg?token=Spwopnmy37EZMsL4nDza&branch=master)

Overview
--------

An implementation of a most commonly used event study methodology, including both parametric and nonparametric tests. It contains variety aspects of the rate of return estimation (the core calculation is done in C++), as well as three classical market models: mean adjusted returns, market adjusted returns and single-index market models. There are 6 parametric and 6 nonparametric tests provided, which examine cross-sectional daily abnormal return (see the documentation of the functions for more information). Furthermore, tests for the cumulative abnormal returns are included.

Instalation
-----------

The package is not submitted to CRAN and is located in a GitHub repository. To install the development version of estudy2 use:

``` r
# install.packages("devtools")
# library("devtools")
devtools::install_github("irudnyts/estudy2")
```
