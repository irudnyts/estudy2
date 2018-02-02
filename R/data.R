#' Stock prices of nine major insurance companies from 2000-01-03 to 2002-01-01
#'
#' A \code{zoo} object of nine columns containing daily Close stock prices of
#' nine major insurance companies from 2000-01-03 to 2002-01-01. The code for
#' generatin this dataset can be seen in the documentation of
#' \code{get_prices_from_tickers} function.
#'
#' @format A \code{zoo} object of nine columns:
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

#' Prices of of ESTX50 EUR P index from 2000-01-03 to 2001-12-31
#'
#' A \code{zoo} object containing daily prices of ESTX50 EUR P index from
#' 2000-01-03 to 2001-12-31. The code for generatin this dataset can be seen in
#' the documentation of \code{get_prices_from_tickers} function.
#'
#' @format A \code{zoo} object.
"prices_indx"

#' Rates of returns of nine major insurance companies from 2000-01-04 to
#' 2002-01-01
#'
#' A \code{zoo} object of nine columns containing  daily rates of returns of
#' nine major insurance companies from 2000-01-04 to 2002-01-01. The code for
#' generatin this dataset can be seen in the documentation of
#' \code{get_rates_from_prices} function.
#'
#' @format A \code{zoo} object of nine columns:
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

#' Rates of returns of ESTX50 EUR P index from 2000-01-04 to 2001-12-31
#'
#' A \code{zoo} object containing daily rates of returns of ESTX50 EUR P index
#' from 2000-01-04 to 2001-12-31. The code for generatin this dataset can be
#' seen in the documentation of \code{get_rates_from_prices} function.
#'
#' @format A \code{zoo} object.
"rates_indx"

#' Returns of nine major insurance companies from 2000-01-04 to 2001-12-29
#'
#' A list of length nine, elements of which are objects of the class
#' \code{returns}. The list contains all necessary returns of nine major
#' insurance companies from 2000-01-04 to 2001-12-29. The code for generatin
#' this dataset can be seen in the documentation of \code{apply_market_model}
#' function.
#'
#' @format A list of nine \code{zoo} elements:
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
