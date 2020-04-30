# estudy2 0.9.2

* Fix a bug in the vignette and examples

# estudy2 0.9.1

* Fix a bug in examples of `returns()`

# estudy2 0.9.0

* Fix documentation for `brown_warner_1985()`
* Fix documentation for `car_brown_warner1985()`
* Update documentation on the returning object for each test
* Implement the CAR non-parametric rank test described in Corrado in the extension by Cowan (1992, p. 6)
* Change returning object from `list` to `data.frame` for all CAR parametric tests, add mean return over the CAR period
* Add validation and handling of `NA` to `returns()`

# estudy2 0.8.5

* Replace function `tseries::get.hist.quote()` by `quantmod::getSymbols()`

* Remove `ELE.PA` ticker from all `tickers` vectors in examples, vignettes, and
data
