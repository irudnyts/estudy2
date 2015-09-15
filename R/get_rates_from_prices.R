get_rates_from_prices <- function(prices, quote = c("Open", "Close"),
                                  multi_day = FALSE,
                                  compounding = c("discrete", "continuous"),
                                  dividends) {
    UseMethod("get_rates_from_prices")
}

get_rates_from_prices.list <- function(prices, quote = c("Open", "Close"),
                                       multi_day = FALSE,
                                       compounding = c("discrete",
                                                       "continuous"),
                                       dividends) {
    quote <- match.arg(quote)
    compounding <- match.arg(compounding)
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
    #### try(colnames(prices_df) <- names(prices), silent = TRUE)
    browser()


    if(missing(dividends)) {
        if(compounding == "discrete") {
            rates <-
                getRatesDiscreteWithoutDividends(as.matrix(prices_df[, -1]))
        } else if(compounding == "continuous") {
            rates <-
                getRatesContinuousWithoutDividends(as.matrix(prices_df[, -1]))

        }
    } else {
        # fix devidends
    }


    if(missing(dividends)) {
        dividends <- zoo(rep(0, length(prices)), time(prices))
    } else {
        #----------------------------------------
        # transfrom devidents to appropriate form
        #----------------------------------------
    }

    if(compounding == "discrete")
    {
        rates <- zoo((coredata(prices[2:length(prices)]) +
                          coredata(dividends[2:length(prices)]) -
                          coredata(prices[1:(length(prices) - 1)]))
                     / coredata(prices[1:(length(prices) - 1)]))
    } else {
        rates <- zoo(log((coredata(prices[2:length(prices)])
                          + coredata(dividends[2:length(dividends)]))
                         / coredata(prices[1:(length(prices)  - 1)])))
    }

    if(quote == "Open")
    {
        time(rates) <- time(prices[1:(length(prices)  - 1)])
    } else {
        time(rates) <- time(prices[2:length(prices)])
    }

    return(rates)

#     # implementation by neglecting the number days between rate of return
#     quote <- match.arg(quote)
#     if(is.null(attributes(prices)$dim)) {
#         attributes(prices)$dim <- c(length(prices), 1)
#     }
#
#     rates <- zoo()
#     for(j in 1:ncol(prices)) {
#         column_prices <- prices[, j][!is.na(prices[, j])]
#         column_rates <- zoo((coredata(column_prices)[2:length(column_prices)] -
#                         coredata(column_prices)[1:(length(column_prices) - 1)])
#                         /
#                         coredata(column_prices)[1:(length(column_prices) - 1)])
#
#         if(quote == "Open") {
#             time(column_rates) <-
#                 time(column_prices[1:(length(column_prices) - 1)])
#         } else {
#             time(column_rates) <-
#                 time(column_prices[2:length(column_prices)])
#         }
#         browser()
#         rates <- merge(rates, column_rates)
#
#     }
#     time(rates) <- as.Date(time(rates))
#     if(is.null(attributes(rates)$dim)) {
#         attributes(rates)$dim <- c(length(rates), 1)
#     }
#     colnames(rates) <- colnames(prices)
#     return(rates)
}

