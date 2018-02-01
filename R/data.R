#' Stock prices of 9 major insurance companies from 2000-01-01 to 2002-01-01
#'
#' A list, which contains 9 elements, each of which is \code{zoo} object,
#' containing daily Close stock prices of 9 major insurance companies from
#' 2000-01-01 to 2002-01-01. The code for generatin this dataset can be seen in
#' the documentation of \code{get_prices_from_tickers} function. The variables
#' are as follows:
#'
#' @format A list of 9 \code{zoo} elements:
#' \itemize{
#'   \item ALV.DE
#'   \item CS.PA
#'   \item ELE.PA
#'   \item G.MI
#'   \item HNR1.HA
#'   \item HSX.L
#'   \item MUV2.DE
#'   \item RSA.L
#'   \item TOP.CO
#' }
"prices"

#' Rates of returns of 9 major insurance companies from 2000-01-01 to 2002-01-01
#'
#' A list, which contains 9 elements, each of which is \code{zoo} object,
#' containing daily rates of returns of corresponding 9 major insurance
#' companies from 2000-01-01 to 2002-01-01. The code for generatin this dataset
#' can be seen in the documentation of \code{get_rates_from_prices} function.
#' The variables are as follows:
#'
#' @format A list of 10 \code{zoo} elements:
#' \itemize{
#'   \item ALV.DE
#'   \item CS.PA
#'   \item ELE.PA
#'   \item G.MI
#'   \item HNR1.HA
#'   \item HSX.L
#'   \item MUV2.DE
#'   \item RSA.L
#'   \item TOP.CO
#' }
"rates"

#' Rates of returns of ESTX50 EUR P index from 2000-01-01 to 2002-01-01
#'
#' A list of one element, which is presented by \code{zoo} class and contains
#' daily rates of returns of ESTX50 EUR P index from 2000-01-01 to 2002-01-01.
#' The code for generatin this dataset can be seen in the documentation of
#' \code{apply_market_model} function.
#'
#' @format A list of one \code{zoo} object.
#'
"rates_indx"

#' Returns of 9 major insurance companies from 2000-01-01 to 2002-01-01
#'
#' A list of length 9, elements of which are objects of the class
#' \code{returns}. The list contains all necessary returns of 9 major insurance
#' companies from 2000-01-01 to 2002-01-01. The code for generatin this dataset
#' can be seen in the documentation of \code{apply_market_model} function. The
#' variables are as follows:
#'
#' @format A list of 10 \code{zoo} elements:
#' \itemize{
#'   \item ALV.DE
#'   \item CS.PA
#'   \item ELE.PA
#'   \item G.MI
#'   \item HNR1.HA
#'   \item HSX.L
#'   \item MUV2.DE
#'   \item RSA.L
#'   \item TOP.CO
#' }
"securities_returns"
