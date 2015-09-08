get_prices_form_tickers <- function(..., start, end,
                                    quote = c("Open", "Close"),
                                    retclass = c("list", "data.frame", "zoo")) {

    # validity of arguments
    quote <- match.arg(quote)
    retclass <- match.arg(retclass)
    tickers <- c(...)
    if(retclass == "list") {
        prices <- list()
        for(ticker in tickers) {
            prices[[ticker]] <- get.hist.quote(instrument = ticker,
                                               start = start, end = end,
                                               quote = quote,
                                               provider = "yahoo",
                                               compression = "d",
                                               retclass = "zoo")
        }
    } else if(retclass == "zoo") {
        prices <- NULL
        for(ticker in tickers) {
            if(is.null(prices)) {
                prices <- get.hist.quote(instrument = ticker, start = start,
                                         end = end, quote = quote,
                                         provider = "yahoo",
                                         compression = "d", retclass = "zoo")
            } else {
                prices <- merge(prices, get.hist.quote(instrument = ticker,
                                                       start = start, end = end,
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
            current_prices <- get.hist.quote(instrument = ticker, start = start,
                                             end = end, quote = quote,
                                             provider = "yahoo",
                                             compression = "d",
                                             retclass = "zoo")
            if(is.null(prices)) {
                prices <- data.frame(date = time(current_prices), prices =
                                            coredata(current_prices))
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


