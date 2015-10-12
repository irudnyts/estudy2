#' Returns the daily prices for securities.
#'
#' Returns the daily Open or Close prices between \code{start} and \code{end}
#' date for given tickers.
#'
#' This function uses the function \code{get.hist.quote} form packege
#' \code{tseries}. The providor set automatically to Yahoo Finance. The function
#' returns the data in different class-conrainers: list of zoo's, zoo, or
#' data.frame.
#'
#' @param ... character vectors indicating the tickers (should be valid in Yahoo
#' Finance).
#' @param start the object of the Date class, which specifies the first date of
#' observed time period.
#' @param end the object of the Date class, which specifies the last date of
#' observed time period.
#' @param quote a character indicating the type of the price (Open or Close).
#' The default value is Open
#' @param retclass a character specifying the return class. "list", "zoo"
#' , or "data.frame", by default is "list"
#' @return the data about prices in given class container: "list", "zoo",
#'  or "data.frame".
#'
#' @seealso \code{\link[tseries]{get.hist.quote}}
#' @export
get_prices_form_tickers <- function(..., start, end,
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
                                                        retclass = "zoo"))
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
            if(is.null(prices)) {
                prices <- data.frame(date = time(current_prices), prices =
                                            zoo::coredata(current_prices))
            } else {
                prices <- merge(prices, data.frame(date = time(current_prices),
                                                   prices =
                                                      coredata(current_prices)),
                                by = "date")
            }
        }
        colnames(prices) <- c("date", tickers)
    }
    return(prices)
}


