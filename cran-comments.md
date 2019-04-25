## Test environments

* local MacOS 10.14.1, R 3.5.3
* local Ubuntu 18.04.2, R 3.5.3 (warning)
* win-builder (release, devel)
* R-hub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * Fedora Linux, R-devel, clang, gfortran
    * Debian Linux, R-devel, GCC ASAN/UBSAN
    * Ubuntu Linux 16.04 LTS, R-release, GCC (warning, note)

## R CMD check results

0 errors | 1 warnings | 1 notes

* checking compilation flags used ... WARNING
Compilation used the following non-portable flag(s):
  ‘-Wdate-time’ ‘-Werror=format-security’ ‘-Wformat’
  
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
* Warning is related to compilation flag. According to (this thread)[https://stat.ethz.ch/pipermail/r-package-devel/2018q3/002878.html] I ignored this warning.


## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmission
This is a resubmission. In this version I have:

* Updated documentation
* Improved validation of arguments
* Implemented new nonparametric tests 
