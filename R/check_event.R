CheckEvent.character <- function(..., index, model, estimation_start,
                                 estimation_end, event_start, event_end,
                                 quote = c("Close", "Open"),
                                 critical_percentage) {
    # ...: can be either the list of
    # index: either the ticker of the index or the column_prices of historical prices
    # model: market model, such as OLS, mean adjusted return, market adjusted return, etc.
    # estimation_start
    # estimation_end
    # event_start
    # event_end
    # quote : Open, Close etc
    # critical_percentage: how many companies are taken into account for given
    #                      date


    # return the same as before + delta? + percentage of used companies?

    # download date if needed
    # apply the model (estimate alpha and beta) and calculate abnormals
    # analyze the abormals in event window
    # return the table of results



    # dispatched for character
    tickers <- c(...)
    companies_prices <- zoo() # data.frame()
    for(t in tickers) {
        column_prices <- get.hist.quote(instrument = t,
                                        start = estimation_start,
                                        end = event_end, quote = "Open",
                                        provider = "yahoo", compression = "d",
                                        retclass = "zoo")
        if(nrow(companies_prices) == 0) {
            companies_prices <- column_prices
        } else {
            companies_prices <- merge(companies_prices, column_prices)
        }
    }
    colnames(companies_prices) <- tickers

    quote <- match.arg(quote)



    # check
    if(inherits(index, "zoo")) {
        index_prices <- index
    } else {
        index_prices <- get.hist.quote(instrument = index,
                                       start = estimation_start,
                                       end = event_end, quote = "Open",
                                       provider = "yahoo", compression = "d",
                                       retclass = "zoo")
    }


    # calculate rate of return
    companies_rates <- GetRatesFromPrices(companies_prices, quote = quote)
    index_rates <- GetRatesFromPrices(index_prices, quote = quote)

    # calculate aanormals
    abnormals <- GetAbnormalReturns(companies_rates, index_rates, model,
                                    estimation_start,
                                    estimation_end, event.window.start,
                                    event_end)

    # analythe abnormals with critical_percentage

    return(companies_prices)
}




k <- CheckEvent("AAPL", "IBM", estimation_start = as.Date("2000-01-01"),
                event_end = as.Date("2001-01-01"))
coredata(k)
time(k)

m <- k[1:10, 1]
attributes(m)$dim <- c(length(m), 1)

