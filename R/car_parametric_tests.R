#' Returns the result of given parametric CAR tests for event study.
#'
#' Performs given tests to examine whether cumulative abnormal return (CAR)
#' significantly differs from zero.
#'
#' \code{car_parametric_tests} performs specified tests among
#' \code{car_brown_warner_1985} and \code{lamb} and returns the list of these
#' tests' results. If \code{all = TRUE} (by default), the function ignores the
#' value of \code{tests}.
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param car_start the object of class \code{Date}, which represents the
#' first (starting) date of the time period, over which CAR will be estimated.
#' @param car_end the object of class \code{Date}, which represents the last
#' (ending) date of the time period, over which CAR will be estimated.
#' @param percentage the lowest allowed percentage of non-missing observation
#' for each day to be incorporated into CAR.
#' @param all a logical value indicating whether all tests should be performed.
#' The default value is \code{TRUE}.
#' @param tests the list of tests functions among \code{car_brown_warner_1985}
#' and \code{car_lamb}.
#' @return The list of results of given tests.
#'
#' @references \itemize{
#' \item Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case of
#' Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#' \item Lamb R.P. \emph{An Exposure-Based Analysis of Property-Liability
#' Insurer Stock Values around Hurricane Andrew}. Journal of Risk and Insurance,
#' 62(1):111-123, 1995.}
#'
#' @seealso \code{\link{car_brown_warner_1985}} and \code{\link{car_lamb}}.
#'
#' @examples
#' # Download the historical prices for ten European insurance companies' stocks
#' tickers <- c("ALV.DE", "AML.L", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA",
#'              "HSX.L", "MUV2.DE", "RSA.L", "TOP.CO" )
#' prices <- get_prices_from_tickers(tickers, start = as.Date("2000-01-01"),
#'                                   end = as.Date("2002-01-01"),
#'                                   quote = "Close", retclass = "list")
#' # Estimate the rate of returns form prices
#' rates <- get_rates_from_prices(prices, quote = "Close", multi_day = TRUE,
#'                                compounding = "continuous")
#' # Download the prices and estimate the rates of market proxy (index
#' # ESTX50 EUR P), which is regressor for the sim model
#' prices_indx <- get_prices_from_tickers("^STOXX50E",
#'                                        start = as.Date("2000-01-01"),
#'                                        end = as.Date("2002-01-01"),
#'                                        quote = "Close", retclass = "list")
#' rates_indx <- get_rates_from_prices(prices_indx, quote = "Close",
#'                                     multi_day = TRUE,
#'                                     compounding = "continuous")
#' # Make the length of regressor and stocks to be the same
#' rates_indx <- replicate(n = length(tickers), expr = rates_indx)
#' # Apply Single Index market model
#' returns <- apply_market_model(rates = rates, regressors = rates_indx,
#'                               market_model = "sim",
#'                               estimation_method = "ols",
#'                               estimation_start = as.Date("2001-03-26"),
#'                               estimation_end = as.Date("2001-09-10"))
#' car_parametric_tests(list_of_returns = returns,
#'                      car_start = as.Date("2001-09-11"),
#'                      car_end = as.Date("2001-09-28"),
#'                      percentage = 90)
#'
#' @export
car_parametric_tests <- function(list_of_returns, car_start, car_end,
                                 percentage, all = TRUE, tests) {
    if(all == TRUE) {
        tests <- list(car_lamb, car_brown_warner_1985)
    }
    result <- NULL
    for(i in seq_along(tests)) {
        tryCatch(
            result[[i]] <- tests[[i]](list_of_returns, car_start, car_end,
                                  percentage),
            error = function(x) warning(paste(x$message,
                                              "The test will be skip.")))
    }
    return(result)
}

#' Lamb's CAR test (1995).
#'
#' Parametric test, proposed by Lamb (1995), which examines whether or not
#' cumulative abnormal return (CAR) significantly differs from zero.
#'
#' This function performs the test, proposed by Lamb (1995) to investigate
#' whether CAR significantly differs from zero. This tests uses the variance,
#' specified by Lamb (1995). The advantage of this test is allowance for
#' correlated cross-sectional returns. Test statistic is close enough to statistic,
#' produced by \code{\link{car_brown_warner_1985}}. The critical
#' values are standard normal. The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param car_start the object of class \code{Date}, which represents the
#' first (starting) date of the time period, over which CAR will be estimated.
#' @param car_end the object of class \code{Date}, which represents the last
#' (ending) date of the time period, over which CAR will be estimated.
#' @param percentage the lowest allowed percentage of non-missing observation
#' for each day to be incorporated into CAR.
#' @return The list of starting date, ending date, average percentage of
#' non-missing observation over each day, the value of statistics, the length
#' of the time period, and significance.
#'
#' @references Lamb R.P. \emph{An Exposure-Based Analysis of Property-Liability
#' Insurer Stock Values around Hurricane Andrew}. Journal of Risk and Insurance,
#' 62(1):111-123, 1995.
#'
#' @seealso \code{\link{car_brown_warner_1985}} and
#' \code{\link{car_parametric_tests}}.
#'
#' @examples
#' # Download the historical prices for ten European insurance companies' stocks
#' tickers <- c("ALV.DE", "AML.L", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA",
#'              "HSX.L", "MUV2.DE", "RSA.L", "TOP.CO" )
#' prices <- get_prices_from_tickers(tickers, start = as.Date("2000-01-01"),
#'                                   end = as.Date("2002-01-01"),
#'                                   quote = "Close", retclass = "list")
#' # Estimate the rate of returns form prices
#' rates <- get_rates_from_prices(prices, quote = "Close", multi_day = TRUE,
#'                                compounding = "continuous")
#' # Download the prices and estimate the rates of market proxy (index
#' # ESTX50 EUR P), which is regressor for the sim model
#' prices_indx <- get_prices_from_tickers("^STOXX50E",
#'                                        start = as.Date("2000-01-01"),
#'                                        end = as.Date("2002-01-01"),
#'                                        quote = "Close", retclass = "list")
#' rates_indx <- get_rates_from_prices(prices_indx, quote = "Close",
#'                                     multi_day = TRUE,
#'                                     compounding = "continuous")
#' # Make the length of regressor and stocks to be the same
#' rates_indx <- replicate(n = length(tickers), expr = rates_indx)
#' # Apply Single Index market model
#' returns <- apply_market_model(rates = rates, regressors = rates_indx,
#'                               market_model = "sim",
#'                               estimation_method = "ols",
#'                               estimation_start = as.Date("2001-03-26"),
#'                               estimation_end = as.Date("2001-09-10"))
#' car_lamb(list_of_returns = returns, car_start = as.Date("2001-09-11"),
#'          car_end = as.Date("2001-09-28"), percentage = 90)
#'
#' @export
car_lamb <- function(list_of_returns, car_start, car_end, percentage) {
    daily_lamb_statistics <- lamb(list_of_returns, car_start, car_end)
    daily_lamb_statistics_tidy <- daily_lamb_statistics[daily_lamb_statistics[, 3] > percentage &
                                                        daily_lamb_statistics[, 3] > 0, ]
    average_percentage <- mean(daily_lamb_statistics_tidy[, 3])

    statistic <- sum(daily_lamb_statistics_tidy[, 5], na.rm = TRUE) /
        sqrt(nrow(daily_lamb_statistics_tidy))
    significance <- ""
    significance <- ifelse(test = abs(statistic) >= const_q1,
                           yes = "*", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q2,
                           yes = "**", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q3,
                           yes = "***", no = "")
    result <- list(car_start = car_start, car_end = car_end,
                   average_percentage = average_percentage,
                   statistic = statistic,
                   number_of_days = nrow(daily_lamb_statistics_tidy),
                   significance = significance)
    return(result)
}

#' Brown and Warner (1985) CAR test.
#'
#' Parametric test, proposed by Brown and Warner (1995), which examines whether
#' or not cumulative abnormal return (CAR) significantly differs from zero.
#'
#' This function performs the test, proposed by Brown and Warner (1985) to
#' investigate whether CAR significantly differs from zero. This tests uses the
#' variance, specified by Lamb (1995). The advantage of this test is allowance
#' for correlated cross-sectional returns. Test statistic is close enough to
#' statistic, produced by \code{\link{car_lamb}}. The critical values are
#' standard normal. The significance levels of \eqn{\alpha} are 0.1, 0.05, and
#' 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param car_start the object of class \code{Date}, which represents the
#' first (starting) date of the time period, over which CAR will be estimated.
#' @param car_end the object of class \code{Date}, which represents the last
#' (ending) date of the time period, over which CAR will be estimated.
#' @param percentage the lowest allowed percentage of non-missing observation
#' for each day to be incorporated into CAR.
#' @return The list of starting date, ending date, average percentage of
#' non-missing observation over each day, the value of statistics, the length
#' of the time period, and significance.
#'
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#' of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#'
#' @seealso \code{\link{car_lamb}} and
#' \code{\link{car_parametric_tests}}.
#'
#' @examples
#' # Download the historical prices for ten European insurance companies' stocks
#' tickers <- c("ALV.DE", "AML.L", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA",
#'              "HSX.L", "MUV2.DE", "RSA.L", "TOP.CO" )
#' prices <- get_prices_from_tickers(tickers, start = as.Date("2000-01-01"),
#'                                   end = as.Date("2002-01-01"),
#'                                   quote = "Close", retclass = "list")
#' # Estimate the rate of returns form prices
#' rates <- get_rates_from_prices(prices, quote = "Close", multi_day = TRUE,
#'                                compounding = "continuous")
#' # Download the prices and estimate the rates of market proxy (index
#' # ESTX50 EUR P), which is regressor for the sim model
#' prices_indx <- get_prices_from_tickers("^STOXX50E",
#'                                        start = as.Date("2000-01-01"),
#'                                        end = as.Date("2002-01-01"),
#'                                        quote = "Close", retclass = "list")
#' rates_indx <- get_rates_from_prices(prices_indx, quote = "Close",
#'                                     multi_day = TRUE,
#'                                     compounding = "continuous")
#' # Make the length of regressor and stocks to be the same
#' rates_indx <- replicate(n = length(tickers), expr = rates_indx)
#' # Apply Single Index market model
#' returns <- apply_market_model(rates = rates, regressors = rates_indx,
#'                               market_model = "sim",
#'                               estimation_method = "ols",
#'                               estimation_start = as.Date("2001-03-26"),
#'                               estimation_end = as.Date("2001-09-10"))
#' car_brown_warner_1985(list_of_returns = returns,
#'                       car_start = as.Date("2001-09-11"),
#'                       car_end = as.Date("2001-09-28"), percentage = 90)
#'
#' @export
car_brown_warner_1985 <- function(list_of_returns, car_start, car_end,
                                  percentage) {
    daily_lamb_statistics <- brown_warner_1985(list_of_returns, car_start, car_end)
    daily_lamb_statistics_tidy <- daily_lamb_statistics[daily_lamb_statistics[, 3] > percentage &
                                                            daily_lamb_statistics[, 3] > 0, ]
    average_percentage <- mean(daily_lamb_statistics_tidy[, 3])

    statistic <- sum(daily_lamb_statistics_tidy[, 5], na.rm = TRUE) /
        sqrt(nrow(daily_lamb_statistics_tidy))
    significance <- ""
    significance <- ifelse(test = abs(statistic) >= const_q1,
                           yes = "*", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q2,
                           yes = "**", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q3,
                           yes = "***", no = "")
    result <- list(car_start = car_start, car_end = car_end,
                   average_percentage = average_percentage,
                   statistic = statistic,
                   number_of_days = nrow(daily_lamb_statistics_tidy),
                   significance = significance)
    return(result)
}
