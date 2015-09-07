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
