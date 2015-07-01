GetRatesFromPrices <- function(prices, quote = c("Open", "Close")) {

    # implementation by neglecting the days between rate of return
    quote <- match.arg(quote)
    if(is.null(attributes(prices)$dim)) {
        attributes(prices)$dim <- c(length(prices), 1)
    }

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
        if(j == 1) {
            rates <- column_rates
        } else {
            rates <- merge(rates, column_rates)
        }
    }
    return(rates)
}
