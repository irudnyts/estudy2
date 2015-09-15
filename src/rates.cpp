#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getRates(NumericMatrix prices,
                                 bool compounding) {
    NumericMatrix rates(prices.nrow() - 1, prices.ncol());

    if(compounding == TRUE)
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                while(R_IsNA(prices(i + 1, j)))
                {
                    i++;
                }
                rates(i, j) = log(prices(i + 1, j) / prices(i, j));
            }
        }
    }
    else
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                while(R_IsNA(prices(i + 1, j)))
                {
                    i++;
                }
                rates(i, j) = (prices(i + 1, j) - prices(i, j)) / prices(i, j);
            }
        }
    }
    return rates;
}
