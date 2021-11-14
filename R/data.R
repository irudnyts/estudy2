#' Stock prices of seven companies from 2019-04-01 to 2020-04-01
#'
#' A \code{zoo} object of seven columns containing daily Close stock prices from
#' 2019-04-01 to 2020-04-01 of seven companies, which could profit from COVID-19
#' lockdown. See examples of \code{\link{get_prices_from_tickers}} for the
#' dataset generation.
#'
#' @format A \code{zoo} object of eight columns:
#' \itemize{
#'   \item AMZN
#'   \item ZM
#'   \item UBER
#'   \item NFLX
#'   \item SHOP
#'   \item FB
#'   \item UPWK
#' }
"prices"

#' Prices of S&P 500 index from 2019-04-01 to 2020-04-01
#'
#' A \code{zoo} object containing daily prices of S&P 500 index from
#' 2019-04-01 to 2020-04-01. See examples of
#' \code{\link{get_prices_from_tickers}} for the dataset generation.
#'
#' @format A \code{zoo} object.
"prices_indx"

#' Rates of returns of seven companies from 2019-04-01 to 2020-04-01
#'
#' A \code{zoo} object of seven columns containing daily rates of returns from
#' 2019-04-01 to 2020-04-01 of seven companies, which could profit from COVID-19
#' lockdown. See examples of \code{\link{get_rates_from_prices}} for the dataset
#' generation.
#'
#' @format A \code{zoo} object of eight columns:
#' \itemize{
#'   \item AMZN
#'   \item ZM
#'   \item UBER
#'   \item NFLX
#'   \item SHOP
#'   \item FB
#'   \item UPWK
#' }
"rates"

#' Rates of returns of S&P 500 index from 2019-04-01 to 2020-04-01
#'
#' A \code{zoo} object containing daily rates of returns of S&P 500 index
#' from 2019-04-01 to 2020-04-01. See examples of
#' \code{\link{get_rates_from_prices}} for the dataset generation.
#'
#' @format A \code{zoo} object.
"rates_indx"

#' Returns of seven companies from 2019-04-01 to 2020-04-01
#'
#' A list of length seven, elements of which are objects of the class
#' \code{returns}. The list contains all necessary returns from 2019-04-01 to
#' 2020-04-01 of seven companies, which could profit from COVID-19 lockdown. See
#' examples of \code{\link{apply_market_model}} for the dataset generation.
#'
#' @format A list of eight \code{zoo} elements:
#' \itemize{
#'   \item AMZN
#'   \item ZM
#'   \item UBER
#'   \item NFLX
#'   \item SHOP
#'   \item FB
#'   \item UPWK
#' }
"securities_returns"
