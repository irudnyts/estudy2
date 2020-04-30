#' Stock prices of eight major European insurance companies from 2000-01-03 to
#' 2002-01-01
#'
#' A \code{zoo} object of eight columns containing daily Close stock prices of
#' eight major European insurance companies from 2000-01-03 to 2002-01-01. See
#' examples of \code{\link{get_prices_from_tickers}} for the dataset generation.
#'
#' @format A \code{zoo} object of eight columns:
#' \itemize{
#'   \item ALV.DE
#'   \item CS.PA
#'   \item G.MI
#'   \item HNR1.HA
#'   \item HSX.L
#'   \item MUV2.DE
#'   \item RSA.L
#'   \item TOP.CO
#' }
"prices"

#' Prices of of EURONEXT 100 index from 2000-01-03 to 2001-12-31
#'
#' A \code{zoo} object containing daily prices of EURONEXT 100 index from
#' 2000-01-03 to 2001-12-31. See examples of
#' \code{\link{get_prices_from_tickers}} for the dataset generation.
#'
#' @format A \code{zoo} object.
"prices_indx"

#' Rates of returns of eight major European insurance companies from 2000-01-04
#' to 2002-01-01
#'
#' A \code{zoo} object of eight columns containing  daily rates of returns of
#' eight major European insurance companies from 2000-01-04 to 2002-01-01. See
#' examples of \code{\link{get_rates_from_prices}} for the dataset generation.
#'
#' @format A \code{zoo} object of eight columns:
#' \itemize{
#'   \item ALV.DE
#'   \item CS.PA
#'   \item G.MI
#'   \item HNR1.HA
#'   \item HSX.L
#'   \item MUV2.DE
#'   \item RSA.L
#'   \item TOP.CO
#' }
"rates"

#' Rates of returns of EURONEXT 100 index from 2000-01-04 to 2001-12-31
#'
#' A \code{zoo} object containing daily rates of returns of EURONEXT 100 index
#' from 2000-01-04 to 2001-12-31. See examples of
#' \code{\link{get_rates_from_prices}} for the dataset generation.
#'
#' @format A \code{zoo} object.
"rates_indx"

#' Returns of eight major insurance companies from 2000-01-04 to 2001-12-29
#'
#' A list of length eight, elements of which are objects of the class
#' \code{returns}. The list contains all necessary returns of eight major
#' insurance companies from 2000-01-04 to 2001-12-29. See examples of
#' \code{\link{apply_market_model}} for the dataset generation.
#'
#' @format A list of eight \code{zoo} elements:
#' \itemize{
#'   \item ALV.DE
#'   \item CS.PA
#'   \item G.MI
#'   \item HNR1.HA
#'   \item HSX.L
#'   \item MUV2.DE
#'   \item RSA.L
#'   \item TOP.CO
#' }
"securities_returns"
