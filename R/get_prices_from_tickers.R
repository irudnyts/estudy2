#' Get daily prices of securities.
#'
#' Returns daily Open or Close prices between \code{start} and \code{end}
#' date for given tickers.
#'
#' This function uses the function \code{get.hist.quote} form the \code{tseries}
#' package. The provider is set automatically to Yahoo Finance. The function
#' returns the data in different class-containers: list of \code{zoo}'s,
#' \code{zoo}, or \code{data.frame}.
#'
#' @param ... character vectors indicating tickers (should be valid in Yahoo
#' Finance).
#' @param start an object of \code{Date} class specifying the first date of the
#' observed time period.
#' @param end an object of \code{Date} class specifying the last date of the
#' observed time period.
#' @param quote a character indicating the type of the price: \code{"Open"}
#' (default) or \code{"Close"}.
#' @param retclass a character specifying the return class: \code{"list"}
#' (default), \code{"zoo"} or \code{"data.frame"}.
#' @return Prices of securities as \code{"list"}, \code{"zoo"}, or
#' \code{"data.frame"}.
#'
#' @seealso \code{\link[tseries]{get.hist.quote}}
#'
#' @examples
#' ## Download historical prices of nine European insurance companies'
#' ## stocks:
#' \dontrun{
#' library("magrittr")
#' tickers <- c("ALV.DE", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA", "HSX.L",
#'              "MUV2.DE", "RSA.L", "TOP.CO")
#' prices <- tickers %>%
#'     get_prices_from_tickers(start = as.Date("2000-01-01"),
#'                             end = as.Date("2002-01-01"),
#'                             quote = "Close",
#'                             retclass = "zoo")
#' }
#' ## The result of the above code is stored in:
#' data(prices)
#'
#' ## Download historical prices of ESTX50 EUR P index:
#' \dontrun{
#' prices_indx <- get_prices_from_tickers("^STOXX50E",
#'                                        start = as.Date("2000-01-01"),
#'                                        end = as.Date("2002-01-01"),
#'                                        quote = "Close",
#'                                        retclass = "zoo")
#' }
#' ## The result of the above code is stored in:
#' data(prices_indx)
#'
#' @export
get_prices_from_tickers <- function(..., start, end,
                                    quote = c("Open", "Close"),
                                    retclass = c("list", "zoo", "data.frame")) {
    quote <- match.arg(quote)
    retclass <- match.arg(retclass)
    tickers <- c(...)
    # for avoiding check inside the loop, the if/else is done outside the loop,
    # that is why so many repetition of the code. Done for optimality and speed.
    if(retclass == "list") {
        prices <- list()
        for(ticker in tickers) {
            prices[[ticker]] <- tseries::get.hist.quote(instrument = ticker,
                                                        start = start,
                                                        end = end,
                                                        quote = quote,
                                                        provider = "yahoo",
                                                        compression = "d",
                                                        retclass = "zoo")
        }
    } else if(retclass == "zoo") {
        prices <- NULL
        for(ticker in tickers) {
            if(is.null(prices)) {
                prices <- tseries::get.hist.quote(instrument = ticker,
                                                  start = start,
                                                  end = end, quote = quote,
                                                  provider = "yahoo",
                                                  compression = "d",
                                                  retclass = "zoo")
            } else {
                prices <- merge(prices,
                                tseries::get.hist.quote(instrument = ticker,
                                                        start = start,
                                                        end = end,
                                                        quote = quote,
                                                        provider = "yahoo",
                                                        compression = "d",
                                                        retclass = "zoo"),
                                all = TRUE)
            }
        }
        colnames(prices) <- tickers
    } else if(retclass == "data.frame") {
        prices <- NULL
        for(ticker in tickers) {
            current_prices <- tseries::get.hist.quote(instrument = ticker,
                                                      start = start, end = end,
                                                      quote = quote,
                                                      provider = "yahoo",
                                                      compression = "d",
                                                      retclass = "zoo")
            current_prices_df <- data.frame(
                date = zoo::index(current_prices),
                prices = zoo::coredata(current_prices)
            )
            colnames(current_prices_df) <- c("date", ticker)
            if(is.null(prices)) {
                prices <- current_prices_df
            } else {
                prices <- merge(prices, current_prices_df, by = "date",
                                all = TRUE)
            }
        }
    }
    return(prices)
}


