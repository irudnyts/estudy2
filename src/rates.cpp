#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getContinuousRates(NumericVector prices) {
    NumericVector rates(prices.size() - 1);
    for(int i = 0; i <= prices.size() - 2; i++)
    {
        rates[i] = log(prices[i + 1] / prices[i]);
    }
    return rates;
}


// [[Rcpp::export]]
NumericVector getDiscreteRates(NumericVector prices) {
    NumericVector rates(prices.size() - 1);
    for(int i = 0; i <= prices.size() - 2; i++)
    {
        rates[i] = (prices[i + 1] - prices[i]) / prices[i];
    }
    return rates;

}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
