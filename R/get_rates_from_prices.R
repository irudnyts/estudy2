#' Calculate rates of return for given prices.
#'
#' \code{get_rates_from_prices} is used for computing rates of returns from
#' prices for different classes.
#'
#' The generic function, dispatched for such classes as \code{list},
#' \code{data.frame}, and \code{zoo}, which represent prices.
#'
#' The calculation is made in C++ (\code{Rcpp}) for high performance.
#'
#' If \code{prices} is data.frame, than the first column should be of the class
#' Date and contains ordered dates of prices.
#'
#' The correspondence between dates and values of the rates depends on the quote,
#' which can be either Open or Close. If the quote is Open, than the value of
#' rate belongs to the first date. Otherwise, to the last. This is also applied
#' for the algorithm, if multiday is allowed: the value of the rate of return
#' is assigned to the last day in case of Close price, and to the first day in
#' in case of Open quote.
#'
#' The \code{multi_day} parameter specifies what to do with missed values and
#' handling of the weekend. If it is TRUE, the function ignores missing values
#' and the rates are calculated between non-missing prices. If it is FALSE, than
#' only one-day period rates of return are computed (between two consecutive
#' calendar dates).
#'
#' The function uses either continuous (by default) or discrete (periodic)
#' compounding.
#'
#' @param prices the object either of class \code{list}, or \code{data.frame},
#' or \code{zoo}. Represents prices, from with the rates of returns will be
#' calculated.
#' @param quote character, which specifies the type of quote: "Open" or "Close".
#' By default is "Open".
#' @param multi_day logical, is rate of return between more than 1 day is
#' allowed?
#' @param compounding character, defines the type of compounding: "discrete" or
#' "continuous". By default is "continuous".
#' @return The rates of returns of the same class as prices.
#'
#' @examples
#' ## Download the historical prices for ten European insurance companies' stocks
#' # tickers <- c("ALV.DE", "AML.L", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA",
#' #              "HSX.L", "MUV2.DE", "RSA.L", "TOP.CO" )
#' # prices <- get_prices_from_tickers(tickers, start = as.Date("2000-01-01"),
#' #                                   end = as.Date("2002-01-01"),
#' #                                   quote = "Close", retclass = "list")
#' data(prices)
#' ## Estimate the rate of returns form prices
#' rates <- get_rates_from_prices(prices, quote = "Close", multi_day = TRUE,
#'                                compounding = "continuous")
#'
#' @export
get_rates_from_prices <- function(prices, quote = c("Open", "Close"),
                                  multi_day = TRUE,
                                  compounding = c("discrete", "continuous")) {
    # in plans to add an argument:
    #   -dividends: for taking into account dividends
    UseMethod("get_rates_from_prices")
}

#' @export
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
        prices_df_company <- data.frame(date = zoo::index(prices[[i]]),
                                        prices = zoo::coredata(prices[[i]]))
        colnames(prices_df_company) <- c("date", paste0("prices", i))
        if(is.null(prices_df)) {
            prices_df <- prices_df_company
        } else {
            prices_df <- merge(prices_df, prices_df_company, by = "date", all = TRUE)
        }
    }

    # calling C++ function to compute rate of return
    if(multi_day) {
        rates <- getMultiDayRates(as.matrix(prices_df[, -1]), continuous, open)
    } else {
        rates <- getSingleDayRates(as.matrix(prices_df[, -1]), continuous)
    }
    # variable for result list
    result <- list()
    for(i in 1:ncol(rates)) {
        if(open) {
            result[[i]] <- zoo::zoo(rates[, i], prices_df[1:(nrow(prices_df) - 1),
                                                     1])
        } else {
            result[[i]] <- zoo::zoo(rates[, i], prices_df[2:nrow(prices_df), 1])
        }
    }
    try(names(result) <- names(prices), TRUE)
    return(result)
}

#' @export
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
        rates <- getSingleDayRates(as.matrix(prices[, -1]), continuous)
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

#' @export
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
    if(!is.null(ncol(prices))) {
        for(i in 1:ncol(prices)) {
            prices_df_company <- data.frame(date = zoo::index(prices[, i]),
                                            prices = zoo::coredata(prices[, i]))
            colnames(prices_df_company) <- c("date", paste0("prices", i))
            if(is.null(prices_df)) {
                prices_df <- prices_df_company
            } else {
                prices_df <- merge(prices_df, prices_df_company, by = "date",
                                   all = TRUE)
            }
        }
    } else {
        prices_df <- data.frame(data = zoo::index(prices[, i]),
                                prices = zoo::coredata(prices))
    }
    # calling C++ function to compute rate of return
    if(multi_day) {
        rates <- getMultiDayRates(as.matrix(prices_df[, -1]), continuous, open)
    } else {
        rates <- getSingleDayRates(as.matrix(prices_df[, -1]), continuous)
    }

    if(open) {
        result <- zoo::zoo(rates, prices_df[1:(nrow(prices_df) - 1), 1])
    } else {
        result <- zoo::zoo(rates, prices_df[2:nrow(prices_df), 1])
    }
    if(!is.null(ncol(prices))) {
        colnames(result) <- colnames(prices)
    }
    return(result)
}
