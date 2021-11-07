download_prices <- function(ticker, start, end, quote) {
    tryCatch(
        expr = {
            get_prices_from_tickers(
                ticker,
                start = start,
                end = end,
                quote = quote,
                retclass = "list"
            )
        },
        error = function(cond) {
            return(NULL)
        }
    )
}
