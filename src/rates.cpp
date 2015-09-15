#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getRates(NumericMatrix prices,
                                 bool continuous) {
    NumericMatrix rates(prices.nrow() - 1, prices.ncol());
    int k; //counter, initialized here to avoid initialization in loop

    if(continuous)
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                if(!R_IsNA(prices(i, j)))
                {
                    k = i + 1;
                    while((k < prices.nrow() - 1) & R_IsNA(prices(k, j)))
                    {
                        rates(k, j) = NA_REAL;
                        k++;
                    }
                    if(k <= prices.nrow() - 1)
                    {
                        rates(i, j) = log(prices(k, j) / prices(i, j));
                    }
                    else
                    {
                        rates(i, j) = NA_REAL;
                    }
                    i = k;
                }
                else
                {
                    rates(i, j) = NA_REAL;
                }
            }
        }
    }
    /*else
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                while(R_IsNA(prices(i + 1, j)) & i > prices.nrow() - 2)
                {
                    i++;
                }
                rates(i, j) = (prices(i + 1, j) - prices(i, j)) / prices(i, j);
            }
        }
    }*/
    return rates;
}
