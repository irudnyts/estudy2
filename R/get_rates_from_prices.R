get_rates_from_prices <- function(prices, quote = c("Open", "Close"),
                                  multi_day = TRUE,
                                  compounding = c("discrete", "continuous")) {
    # in plans to add two arguments:
    #   -dividends: for taking into account dividends
    #   -multi_day: the period between two prices for rate of return could be
    #               either one or coupe days (how it is currently impplemented)
    UseMethod("get_rates_from_prices")
}

get_rates_from_prices.list <- function(prices, quote = c("Open", "Close"),
                                       multi_day = TRUE,
                                       compounding = c("discrete",
                                                       "continuous")) {
    quote <- match.arg(quote)
    compounding <- match.arg(compounding)
    continuous <- if(compounding == "continuous") {
        TRUE
    } else if(compounding == "discrete") {
        FALSE
    }
    open <- if(quote == "Open") {
        TRUE
    } else if(quote == "Close") {
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
    if(multi_day) {
        rates <- getMultiDayRates(as.matrix(prices_df[, -1]), continuous, open)
    } else {
        rates <- getSingleDayRates(as.matrix(prices_df[, -1]), continuous, open)
    }
    # variable for result list
    result <- list()
    for(i in 1:ncol(rates)) {
        if(open) {
            result[[i]] <- zoo(rates[, i], prices_df[1:(nrow(prices_df) - 1),
                                                     1])
        } else {
            result[[i]] <- zoo(rates[, i], prices_df[2:nrow(prices_df), 1])
        }
    }
    try(names(result) <- names(prices), T)
    return(result)
}


get_rates_from_prices.data.frame <- function(prices, quote = c("Open", "Close"),
                                             multi_day = TRUE,
                                             compounding = c("discrete",
                                                             "continuous")) {
    quote <- match.arg(quote)
    compounding <- match.arg(compounding)
    continuous <- if(compounding == "continuous") {
        TRUE
    } else if(compounding == "discrete") {
        FALSE
    }
    open <- if(quote == "Open") {
        TRUE
    } else if(quote == "Close") {
        FALSE
    }

    # calling C++ function to compute rate of return
    if(multi_day) {
        rates <- getMultiDayRates(as.matrix(prices[, -1]), continuous, open)
    } else {
        rates <- getSingleDayRates(as.matrix(prices[, -1]), continuous, open)
    }

    # bind with column for rates
    if(open) {
        rates <- cbind(data.frame(date = prices[1:(nrow(prices) - 1), 1]),
                       rates)
    } else {
        rates <- cbind(data.frame(date = prices[2:nrow(prices), 1]), rates)
    }
    colnames(rates) <- colnames(prices)
    return(rates)
}

get_rates_from_prices.zoo <- function(prices, quote = c("Open", "Close"),
                                       multi_day = TRUE,
                                       compounding = c("discrete",
                                                       "continuous")) {
    quote <- match.arg(quote)
    compounding <- match.arg(compounding)
    continuous <- if(compounding == "continuous") {
        TRUE
    } else if(compounding == "discrete") {
        FALSE
    }
    open <- if(quote == "Open") {
        TRUE
    } else if(quote == "Close") {
        FALSE
    }

    # coert list to data.frame and then to matrix
    prices_df <- NULL
    for(i in 1:ncol(prices)) {
        if(is.null(prices_df)) {
            prices_df <- data.frame(date = time(prices[, i]),
                                    prices = coredata(prices[, i]))
        } else {
            prices_df <- merge(prices_df, data.frame(date = time(prices[, i]),
                                                prices = coredata(prices[, i])),
                               by = "date")
        }
    }

    # calling C++ function to compute rate of return
    if(multi_day) {
        rates <- getMultiDayRates(as.matrix(prices_df[, -1]), continuous, open)
    } else {
        rates <- getSingleDayRates(as.matrix(prices_df[, -1]), continuous, open)
    }

    if(open) {
        result <- zoo(rates, prices_df[1:(nrow(prices_df) - 1), 1])
    } else {
        result <- zoo(rates, prices_df[2:nrow(prices_df), 1])
    }

    colnames(result) <- colnames(prices)
    return(result)
}



