## Test environments

* local MacOS 10.15.1, R 3.6.3
* local MacOS 10.15.1, R 4.0.0
* Ubuntu 16.04 (on Travis-CI), R 3.6.0
* Windows (on AppVayor), R-release
* win-builder (R-release, R-devel)
* R-hub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * Ubuntu Linux 16.04 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran
    * Debian Linux, R-release, GCC
    * Debian Linux, R-devel, GCC ASAN/UBSAN

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

* Fix a bug in documentation related to implicit coercion
