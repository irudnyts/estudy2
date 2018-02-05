#' Calculate rates of return for given prices.
#'
#' \code{get_rates_from_prices} is used for computing rates of return from
#' prices for different classes.
#'
#' This is a generic function, dispatched for such classes as \code{list},
#' \code{data.frame}, and \code{zoo} that represent prices.
#'
#' The calculation is made in C++ (\code{Rcpp}) in favor of speed.
#'
#' If \code{prices} is a data frame, than the first column should be of the
#' class \code{Date} and contain ordered dates of prices.
#'
#' The correspondence between dates and values of the rates depends on the
#' quote, which can be either Open or Close. If the quote is Open, than the
#' value of rate belongs to the former date. Otherwise, to the latter one. This
#' is also applied for the algorithm, if multiday is allowed: the value of the
#' rate of return is assigned to the latter day in case of Close price, and to
#' the former day in in case of Open quote.
#'
#' The \code{multi_day} parameter specifies how to handle missing values and
#' weekends. If the value is TRUE, the function ignores missing values
#' and the rates are calculated between non-missing prices. If it is FALSE, then
#' only one-day period rates of return are computed (between two consecutive
#' calendar dates).
#'
#' The function uses either continuous (by default) or discrete (periodic)
#' compounding.
#'
#' @param prices an object containing prices of securities. Three classes are
#' allowed: \code{list}, \code{data.frame}, and \code{zoo}.
#' @param quote a character vector specifying the type of the quote:
#' \code{"Open"} (default) or \code{"Close"}.
#' @param multi_day logical, is a rate of return between more than 1 day is
#' allowed?
#' @param compounding a character vector defining the type of compounding:
#' \code{"continuous"} (default) or \code{"discrete"}.
#' @return Rates of returns of the same class as prices.
#'
#' @examples
#' ## Download historical prices of nine European insurance companies'
#' ## stocks and estimate rates of returns form prices:
#' \dontrun{
#' library("magrittr")
#' tickers <- c("ALV.DE", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA", "HSX.L",
#'              "MUV2.DE", "RSA.L", "TOP.CO")
#' rates <- tickers %>%
#'     get_prices_from_tickers(start = as.Date("2000-01-01"),
#'                             end = as.Date("2002-01-01"),
#'                             quote = "Close",
#'                             retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' }
#' ## The result of the above code is stored in:
#' data(rates)
#'
#' ## Download historical prices of ESTX50 EUR P index and estimate rates of
#' ## returns from prices:
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' }
#' ## The result of the above code is stored in:
#' data(rates_indx)
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
