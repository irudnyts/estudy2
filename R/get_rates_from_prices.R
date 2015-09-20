get_rates_from_prices <- function(prices, quote = c("Open", "Close"),
                                  multi_day = FALSE,
                                  compounding = c("discrete", "continuous")) {
    # in plans to add two arguments:
    #   -dividends: for taking into account dividends
    #   -multi_day: the period between two prices for rate of return could be
    #               either one or coupe days (how it is currently impplemented)
    UseMethod("get_rates_from_prices")
}

get_rates_from_prices.list <- function(prices, quote = c("Open", "Close"),
                                       compounding = c("discrete",
                                                       "continuous")) {
    quote <- match.arg(quote)
    compounding <- match.arg(compounding)
    continuous <- if(compounding == "continuous") {
        TRUE
    } else if(compounding == "discrete") {
        FALSE
    }

    # coert list to data.frame and then to matrix
    prices_df <- NULL
    for(i in seq_along(prices)) {
        if(is.null(prices_df)) {
            prices_df <- data.frame(date = time(prices[[i]]),
                                    prices = coredata(prices[[i]]))
        } else {
            prices_df <- merge(prices_df, data.frame(date = time(prices[[i]]),
                                                prices = coredata(prices[[i]])),
                               by = "date")
        }
    }
    # calling C++ function to compute rate of return
    rates <- getRates(matrix(prices_df[, -1], nrow = nrow(prices_df),
                             ncol = ncol(prices_df) - 1), continuous)
    # variable for result list
    result <- list()
    for(i in ncol(rates)) {
        if(quote == "Open") {
            result[[i]] <- zoo(rates[, i], prices_df[1:(nrow(prices_df) - 1),
                                                     1])
        } else {
            result[[i]] <- zoo(rates[, i], prices_df[2:nrow(prices_df), 1])
        }
    }
    try(names(result) <- names(prices), T)
    return(rates)
}

get_rates_from_prices.data.frame <- function(prices, quote = c("Open", "Close"),
                                       compounding = c("discrete",
                                                       "continuous")) {
    quote <- match.arg(quote)
    compounding <- match.arg(compounding)

}




