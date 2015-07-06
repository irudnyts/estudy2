get_rates_from_prices <- function(prices, quote = c("Open", "Close")) {

    # implementation by neglecting the days between rate of return
    quote <- match.arg(quote)
    if(is.null(attributes(prices)$dim)) {
        attributes(prices)$dim <- c(length(prices), 1)
    }

    rates <- zoo()
    for(j in 1:ncol(prices)) {
        column_prices <- prices[, j][!is.na(prices[, j])]
        column_rates <- zoo((coredata(column_prices)[2:length(column_prices)] -
                        coredata(column_prices)[1:(length(column_prices) - 1)])
                        /
                        coredata(column_prices)[1:(length(column_prices) - 1)])

        if(quote == "Open") {
            time(column_rates) <-
                time(column_prices[1:(length(column_prices) - 1)])
        } else {
            time(column_rates) <-
                time(column_prices[2:length(column_prices)])
        }
        rates <- merge(rates, column_rates)

    }
    time(rates) <- as.Date(time(rates))
    if(is.null(attributes(rates)$dim)) {
        attributes(rates)$dim <- c(length(rates), 1)
    }
    colnames(rates) <- colnames(prices)
    return(rates)
}

# k <- get_prices_form_tickers("MSFT", "AAPL", start = as.Date("2000-01-01"),
#                              end = as.Date("2000-01-10"), quote = "Open")
# get_rates_from_prices(k)
