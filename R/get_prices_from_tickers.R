get_prices_form_tickers <- function(..., start, end,
                                    quote = c("Open", "Close")) {

    # validity of arguments
    quote <- match.arg(quote)
    tickers <- c(...)
    result <- NULL

    for(ticker in tickers) {
        if(is.null(result)) {
            result <- get.hist.quote(instrument = ticker, start = start,
                                     end = end, quote = quote,
                                     provider = "yahoo",
                                     compression = "d", retclass = "zoo")
        } else {
            result <- merge(result, get.hist.quote(instrument = ticker,
                                                   start = start,
                                                   end = end, quote = quote,
                                                   provider = "yahoo",
                                                   compression = "d",
                                                   retclass = "zoo"))
        }
    }
    time(result) <- as.Date(time(result))
    colnames(result) <- tickers
    return(result)
}


