#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix getMultiDayRates(NumericMatrix prices,
                                 bool continuous,
                                 bool Open) {
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
                    while((k < prices.nrow() - 1) && R_IsNA(prices(k, j)))
                    {
                        rates(k, j) = NA_REAL;
                        k++;
                    }
                    if(k <= prices.nrow() - 1)
                    {
                        if(Open || k == i + 1)
                        {
                            rates(i, j) = log(prices(k, j) / prices(i, j));
                        }
                        else
                        {
                            rates(i, j) = NA_REAL;
                            rates(k - 1, j) = log(prices(k, j) / prices(i, j));
                        }
                    }
                    else
                    {
                        rates(i, j) = NA_REAL;
                    }
                    i = k - 1; // k - 1 because next iteration: i = i + 1
                }
                else
                {
                    rates(i, j) = NA_REAL;
                }
            }
        }
    }
    else
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                if(!R_IsNA(prices(i, j)))
                {
                    k = i + 1;
                    while((k < prices.nrow() - 1) && R_IsNA(prices(k, j)))
                    {
                        rates(k, j) = NA_REAL;
                        k++;
                    }
                    if(k <= prices.nrow() - 1)
                    {
                        if(Open || k == i + 1)
                        {
                            rates(i, j) = prices(k, j) / prices(i, j) - 1;
                        }
                        else
                        {
                            rates(i, j) = NA_REAL;
                            rates(k - 1, j) = prices(k, j) / prices(i, j) - 1;
                        }
                    }
                    else
                    {
                        rates(i, j) = NA_REAL;
                    }
                    i = k - 1; // k - 1 because next iteration: i = i + 1
                }
                else
                {
                    rates(i, j) = NA_REAL;
                }
            }
        }
    }
    return rates;
}

// [[Rcpp::export]]
NumericMatrix getSingleDayRates(NumericMatrix prices,
                               bool continuous) {
    NumericMatrix rates(prices.nrow() - 1, prices.ncol());

    if(continuous)
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                if(!R_IsNA(prices(i, j)) && !R_IsNA(prices(i + 1, j)))
                {
                    rates(i, j) = log(prices(i + 1, j) / prices(i, j));
                }
                else
                {
                    rates(i, j) = NA_REAL;
                }
            }
        }
    }
    else
    {
        for(int j = 0; j <= prices.ncol() - 1; j++)
        {
            for(int i = 0; i <= prices.nrow() - 2; i++)
            {
                if(!R_IsNA(prices(i, j)) && !R_IsNA(prices(i + 1, j)))
                {
                    rates(i, j) = prices(i + 1, j) / prices(i, j) - 1;
                }
                else
                {
                    rates(i, j) = NA_REAL;
                }
            }
        }
    }
    return rates;
}

