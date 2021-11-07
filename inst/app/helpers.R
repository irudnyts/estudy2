download_prices <- function(ticker, start, end, quote, retclass) {
    tryCatch(
        expr = {
            get_prices_from_tickers(
                ticker,
                start = start,
                end = end,
                quote = quote,
                retclass = retclass
            )
        },
        error = function(cond) {
            return(NULL)
        }
    )
}
