check_event <- function(..., index, model, estimation_start, estimation_end,
                        event_start, event_end, quote = c("Close", "Open"),
                        critical_percentage) {
    # ...: can be either the list of
    # index: either the ticker of the index or the
    #        column_prices of historical prices
    # model: market model, such as OLS,
    #        mean adjusted return, market adjusted return, etc.
    # estimation_start
    # estimation_end
    # event_start
    # event_end
    # quote : Open, Close etc
    # critical_percentage: how many companies are taken into
    #                      account for given date


    # return the same as before + delta + percentage of used
    # companies

    # download data if needed
    # apply the model (estimate alpha and beta) and calculate
    # abnormals
    # analyze the abormals in event window
    # return the table of results



    if(inherits(..., "zoo")) {
        companies_prices <- merge(...)
    } else if(inherits(..., "character")) {
        tickers <- c(...)
        companies_prices <- get_prices_form_tickers(...,
                                                    start = estimation_start,
                                                    end = event_end,
                                                    quote = quote)
    }
    quote <- match.arg(quote)

    if(inherits(index, "zoo")) {
        index_prices <- index
    } else if(inherits(index, "character")) {
        index_prices <- get_prices_form_tickers(index, start = estimation_start,
                                                end = event_end, quote = quote)
    }


    # calculate rate of return

    companies_rates <- get_rates_from_prices(companies_prices, quote = quote)
    index_rates <- get_rates_from_prices(index_prices, quote = quote)

    # calculate aanormals
    # abnormals <- GetAbnormalReturns(companies_rates, index_rates, model,
    #                                     estimation_start, estimation_end,
    #                                     event_start, event_end)
    #
    #     # analyze abnormals with critical_percentage
    #
    #     return(companies_prices)

    return(list(companies_rates, index_rates))
}


# test

library("zoo")
library("tseries")

index <- get_prices_form_tickers("AAPL", start = as.Date("2000-01-01"),
                             end = as.Date("2001-01-10"), quote = "Open")
coredata(index)
time(index)

check_event("AAPL", "IBM", index = index, estimation_start = as.Date("2000-01-01"), event_end = as.Date("2000-01-15"), quote = "Open")
