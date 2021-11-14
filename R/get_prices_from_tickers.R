#' Get daily prices of securities.
#'
#' Returns daily Open or Close prices between \code{start} and \code{end}
#' date for given tickers.
#'
#' This function uses the function \code{getSymbols} form the \code{quantmod}
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
#' @seealso \code{\link[quantmod]{getSymbols}}
#'
#' @examples
#' ## Download historical prices of seven companies' stocks:
#' \dontrun{
#' library("magrittr")
#' tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")
#' prices <- tickers %>%
#'     get_prices_from_tickers(start = as.Date("2019-04-01"),
#'                             end = as.Date("2020-04-01"),
#'                             quote = "Close",
#'                             retclass = "zoo")
#' }
#' ## The result of the above code is stored in:
#' data(prices)
#'
#' ## Download historical prices of S&P 500 index:
#' \dontrun{
#' prices_indx <- get_prices_from_tickers("^GSPC",
#'                                        start = as.Date("2019-04-01"),
#'                                        end = as.Date("2020-04-01"),
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
    if (!curl::has_internet()) {
        message("Please make sure you have an internet connection.")
        return(invisible(NULL))
    }
    # for avoiding check inside the loop, the if/else is done outside the loop,
    # that is why so many repetition of the code. Done for optimality and speed.
    if(retclass == "list") {
        prices <- list()
        for(ticker in tickers) {
            prices[[ticker]] <- quantmod::getSymbols(Symbols = ticker,
                                                     verbose = FALSE,
                                                     warnings = FALSE,
                                                     auto.assign = FALSE,
                                                     from = start,
                                                     to = end,
                                                     src = "yahoo",
                                                     return.class = "zoo")
            colnames(prices[[ticker]]) <- sub(".*\\.",
                                              "",
                                              colnames(prices[[ticker]]))
            prices[[ticker]] <- prices[[ticker]][, quote, drop = FALSE]
        }
    } else if(retclass == "zoo") {
        prices <- NULL
        for(ticker in tickers) {
            if(is.null(prices)) {
                prices <- quantmod::getSymbols(Symbols = ticker,
                                               verbose = FALSE,
                                               warnings = FALSE,
                                               auto.assign = FALSE,
                                               from = start,
                                               to = end,
                                               src = "yahoo",
                                               return.class = "zoo")
                colnames(prices) <- sub(".*\\.", "", colnames(prices))
                prices <- prices[, quote, drop = FALSE]
            } else {
                current_prices <- quantmod::getSymbols(Symbols = ticker,
                                                       verbose = FALSE,
                                                       warnings = FALSE,
                                                       auto.assign = FALSE,
                                                       from = start,
                                                       to = end,
                                                       src = "yahoo",
                                                       return.class = "zoo")
                colnames(current_prices) <- sub(".*\\.",
                                                "",
                                                colnames(current_prices))
                current_prices <- current_prices[, quote, drop = FALSE]
                prices <- merge(prices, current_prices, all = TRUE)
            }
        }
        colnames(prices) <- tickers
    } else if(retclass == "data.frame") {
        prices <- NULL
        for(ticker in tickers) {
            current_prices <- quantmod::getSymbols(Symbols = ticker,
                                                   verbose = FALSE,
                                                   warnings = FALSE,
                                                   auto.assign = FALSE,
                                                   from = start,
                                                   to = end,
                                                   src = "yahoo",
                                                   return.class = "zoo")
            colnames(current_prices) <- sub(".*\\.",
                                            "",
                                            colnames(current_prices))
            current_prices <- current_prices[, quote, drop = FALSE]
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


