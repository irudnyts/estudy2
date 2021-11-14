## Test environments

* local MacOS 11.5.2, R 4.1.1
* Ubuntu 16.04 (on Travis-CI), R-release
* Windows (on AppVayor), R-release
* win-builder (R-release, R-devel)
* R-hub Windows Server 2008 R2 SP1, R-release, 32/64 bit

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Iegor Rudnytskyi <iegor.rudnytskyi@gmail.com>’

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2331331
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
  DOI: 10.2307/2490543
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
  DOI: 10.2307/253695
    From: DESCRIPTION
          DESCRIPTION
    Status: Forbidden
    Message: 403
    

### Comments:    
    
* DOI are correct and accessible.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmission

This is a resubmission. In this version I have:

* Modify examples to COVID-19, so that vignette does not have any issues with 
unsupported tickers
* Fail `get_prices_from_tickers()` gracefully, when there is not internet connection
* Equip the package with a demo Shiny app
