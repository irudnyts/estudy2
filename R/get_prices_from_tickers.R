get_prices_form_tickers <- function(..., start, end,
                                    quote = c("Open", "Close")) {

    # validity of arguments
    quote <- match.arg(quote)
    tickers <- c(...)
    result <- zoo()

    for(i in seq_along(tickers)) {
        prices <- get.hist.quote(instrument = tickers[i], start = start,
                                 end = end, quote = quote, provider = "yahoo",
                                 compression = "d", retclass = "zoo")
        result <- merge(result, prices)
    }
    time(result) <- as.Date(time(result))
    colnames(result) <- tickers
    return(result)
}


