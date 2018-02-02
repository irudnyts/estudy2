#' Apply market model and return the list of objects \code{returns}
#'
#' The function applies given market model to securities' rates of returns and
#' returns the list of objects \code{returns} for each security, which can be
#' passed directly to the whole battery of tests.
#'
#' The generic function is dispatched for such classes as \code{list},
#' \code{data.frame}, and \code{zoo}. If \code{same_regressor_for_all} is TRUE,
#' and \code{regressors} has the length greater than one, the first element of
#' \code{regressors} will be applied for each sequrity in \code{rates}.
#'
#' @param rates \code{list}, \code{data.frame}, \code{zoo} object containing
#' rates of returns of securities.
#' @param regressors an object of the same class as \code{rates} containing
#' regressors. Can be omitted, if market model is \code{mean_adj}.
#' \code{regressors} must have the same number of components as \code{rates}
#' expept the cases when the same regressor used for all securities.
#' @param same_regressor_for_all logical. Should the same regressor be used for
#' each security? The default value is TRUE.
#' @param market_model a character indicating the market model among
#' \code{mean_adj}, \code{mrkt_adj}, and \code{sim}.
#' @param estimation_method a character, specifying the estimation method for
#' \code{sim} market model.
#' @param estimation_start an object of class Data, giving the start date of
#' estimation period.
#' @param estimation_end an object of class Data, giving the start date of
#' estimation period.
#' @return A list of objects \code{returns}
#'
#' @seealso \code{\link{returns}}
#'
#' @examples
#' ## Download the historical prices for ten European insurance companies' stocks
#' # tickers <- c("ALV.DE", "AML.L", "CS.PA", "ELE.PA", "G.MI", "HNR1.HA",
#' #              "HSX.L", "MUV2.DE", "RSA.L", "TOP.CO" )
#' # prices <- get_prices_from_tickers(tickers, start = as.Date("2000-01-01"),
#' #                                   end = as.Date("2002-01-01"),
#' #                                   quote = "Close", retclass = "list")
#' ## Estimate the rate of returns form prices
#' # rates <- get_rates_from_prices(prices, quote = "Close", multi_day = TRUE,
#' #                                compounding = "continuous")
#' ### Mean-adjusted returns market model
#' ## Apply mean-adjusted returns market model for each company
#' data(rates)
#' securities_returns <- apply_market_model(rates, market_model = "mean_adj",
#'                                          estimation_start =
#'                                                        as.Date("2001-03-26"),
#'                                          estimation_end =
#'                                                        as.Date("2001-09-10"))
#' ### Single Index market model
#' ## Download the prices and estimate the rates of market proxy (index
#' ## ESTX50 EUR P), which is regressor for the sim model
#' # prices_indx <- get_prices_from_tickers("^STOXX50E",
#' #                                        start = as.Date("2000-01-01"),
#' #                                        end = as.Date("2002-01-01"),
#' #                                        quote = "Close", retclass = "list")
#' # rates_indx <- get_rates_from_prices(prices_indx, quote = "Close",
#' #                                     multi_day = TRUE,
#' #                                     compounding = "continuous")
#' data(rates_indx)
#' ## Apply Single Index market model
#' securities_returns <- apply_market_model(rates = rates,
#'                                          regressors = rates_indx,
#'                                          same_regressor_for_all = TRUE,
#'                                          market_model = "sim",
#'                                          estimation_method = "ols",
#'                                          estimation_start =
#'                                                        as.Date("2001-03-26"),
#'                                          estimation_end =
#'                                                        as.Date("2001-09-10"))
#'
#' @export
apply_market_model <- function(rates, regressors, same_regressor_for_all = TRUE,
                               market_model = c("mean_adj", "mrkt_adj", "sim"),
                               estimation_method = c("ols"), estimation_start,
                               estimation_end) {
    UseMethod("apply_market_model")
}

#' @export
apply_market_model.list <- function(rates, regressors, same_regressor_for_all =
                                        TRUE,
                                    market_model =
                                        c("mean_adj", "mrkt_adj", "sim"),
                                    estimation_method = c("ols"),
                                    estimation_start, estimation_end) {
    # check args for validity
    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)

    if(market_model != "mean_adj") {
        if(missing(regressors)) {
            stop(paste("For market model", market_model, "specify the",
                       "regressors."))
        }
        if(!same_regressor_for_all && length(rates) != length(regressors)) {
            stop(paste("The number of regressors elements should be the same",
                       "as the number of rates elements"))

        }
        if(same_regressor_for_all && length(regressors) != 1) {
            message("Only first element of regressors will be used")
        }
    }

    if(estimation_start >= estimation_end) {
        stop("estimation_start should be earlier than estimation_end")
    }

    if(market_model != "mean_adj" && same_regressor_for_all) {
        first_element <- as.list(regressors[[1]])
        regressors <- rep(first_element, length(rates))
    }

    list_of_returns <- list()
    if(market_model == "mean_adj") {
        for(i in seq_along(rates)) {
            list_of_returns[[i]] <- returns(rates = rates[[i]],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "mrkt_adj") {
        for(i in seq_along(rates)) {
            list_of_returns[[i]] <- returns(rates = rates[[i]],
                                            regressor = regressors[[i]],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "sim"){
        for(i in seq_along(rates)) {
            list_of_returns[[i]] <- returns(rates = rates[[i]],
                                            regressor = regressors[[i]],
                                            market_model = market_model,
                                            estimation_method =
                                                estimation_method,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    }
    return(list_of_returns)
}

#' @export
apply_market_model.data.frame <- function(rates, regressors,
                                          same_regressor_for_all = TRUE,
                                          market_model =
                                              c("mean_adj", "mrkt_adj", "sim"),
                                          estimation_method = c("ols"),
                                          estimation_start, estimation_end) {
    # check args for validity
    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)

    if(market_model != "mean_adj") {
        if(missing(regressors)) {
            stop(paste("For market model", market_model, "specify the",
                       "regressors."))
        }
        if(!same_regressor_for_all && ncol(rates) != ncol(regressors)) {
            stop(paste("The number of regressors columns should be the same",
                       "as the number of rates columns"))
        }
        if(same_regressor_for_all && ncol(regressors) > 2) {
            message("Only second column of regressors will be used")
        }
    }

    if(estimation_start >= estimation_end) {
        stop("estimation_start should be earlier than estimation_end")
    }

    if(market_model != "mean_adj" && same_regressor_for_all) {
        first_element <- regressors[, c(1, 2)]
        regressors <- data.frame(date = first_element[, 1],
                                 first_element[, rep(2, ncol(rates) - 1)])
    }

    list_of_returns <- list()
    if(market_model == "mean_adj") {
        for(i in 2:ncol(rates)) {
            list_of_returns[[i - 1]] <- returns(rates = rates[, c(1, i)],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "mrkt_adj") {
        for(i in 2:ncol(rates)) {
            list_of_returns[[i - 1]] <- returns(rates = rates[, c(1, i)],
                                            regressor = regressors[, c(1, i)],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "sim"){
        for(i in 2:ncol(rates)) {
            list_of_returns[[i - 1]] <- returns(rates = rates[, c(1, i)],
                                            regressor = regressors[, c(1, i)],
                                            market_model = market_model,
                                            estimation_method =
                                                estimation_method,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    }
    return(list_of_returns)
}

#' @export
apply_market_model.zoo <- function(rates, regressors, same_regressor_for_all =
                                       TRUE,
                                   market_model =
                                       c("mean_adj", "mrkt_adj", "sim"),
                                   estimation_method = c("ols"),
                                   estimation_start, estimation_end) {
    # check args for validity
    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)
    if(market_model != "mean_adj") {
        if(missing(regressors)) {
            stop(paste("For market model", market_model,
                       "specify the regressors."))
        }
        if(!same_regressor_for_all && ncol(rates) != ncol(regressors)) {
            stop(paste("The number of regressors columns should be the same",
                       "as the number of rates columns"))
        }
        if(same_regressor_for_all && ncol(regressors) > 1) {
            message("Only first column of regressors will be used")
        }
    }

    if(estimation_start >= estimation_end) {
        stop("estimation_start should be earlier than estimation_end")
    }

    if(market_model != "mean_adj" && same_regressor_for_all) {
        first_element <- regressors[, 1]
        regressors <- first_element[, rep(1, ncol(rates))]
    }

    list_of_returns <- list()
    if(market_model == "mean_adj") {
        for(i in 1:ncol(rates)) {
            list_of_returns[[i]] <- returns(rates = rates[, i],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "mrkt_adj") {
        for(i in 1:ncol(rates)) {
            list_of_returns[[i]] <- returns(rates = rates[, i],
                                            regressor = regressors[, i],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "sim"){
        for(i in 1:ncol(rates)) {
            list_of_returns[[i]] <- returns(rates = rates[, i],
                                            regressor = regressors[, i],
                                            market_model = market_model,
                                            estimation_method =
                                                estimation_method,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    }
    return(list_of_returns)
}

#' Constructor of an object of S3 class \code{returns}
#'
#' Constructs an object of S3 class \code{returns}.
#'
#' The constructor is a generic function, dispatched for classes \code{zoo}
#' \code{data.frame}. Parameters \code{rates} and \code{regressor} should be
#' objects of the same class (\code{zoo} or \code{data.frame}). There are three
#' market model implemented. \code{mean_adj} stands for mean-adjusted-returns
#' model, which is the average of returns during the estimation period.
#' \code{mrkt_adj} represents market-adjusted-returns model: the securities'
#' rates of returns are simply market index rates of returns (in terms of
#' parameters - \code{regressor}). Finally, \code{sim} stands for single-index
#' market model For this model only Ordinary Least Squares
#' \code{estimation_method} is currently implemented. All models are described
#' in Brown and Warner (1985).
#'
#' @param rates an object of class either \code{zoo} or \code{data.frame}
#' giving observed rates of returns of security.
#' @param regressor an object of the same class as \code{rates} representing
#' rates of returns of the market model, if needed.
#' @param market_model a character indicating the market model among
#' \code{mean_adj}, \code{mrkt_adj}, and \code{sim}.
#' @param estimation_method a character specifying the estimation method for
#' \code{sim} market model.
#' @param estimation_start an object of \code{Date} class giving the first date
#' of the estimation period.
#' @param estimation_end an object of \code{Date} class giving the last date of
#' the estimation period.
#' @return An object of S3 class \code{returns}, which contains following
#' fields:
#' \itemize{
#' \item observed: an object of \code{zoo} class containing observed rates of
#' returns.
#' \item predicted: an object of \code{zoo} class containing predicted by a
#' market model rates of returns.
#' \item lower95CI: a lower bound of the 95\% Confidence Interval for
#' predicted rates of returns.
#' \item upper95CI: an upper bound of the 95\% Confidence Interval for
#' predicted rates of returns.
#' \item abnormal: an object of \code{zoo} class containing abnormal returns.
#' \item regressor: an object of \code{zoo} class containing rates of
#' regressor (typically market index).
#' \item market_model: a code name of the market model.
#' \item full_name_market_model: a full name of the market model.
#' \item estimation_method: a code name of the estimation method (applied only
#' for SIM).
#' \item full_name_estimation_method: a full name of the estimation method
#' (applied only for SIM).
#' \item coefficients: coefficients \eqn{\alpha} and \eqn{\beta} for SIM market
#' model (applied only for SIM).
#' \item estimation_start: a start date of the estimation period.
#' \item estimation_end: an end date of the estimation period.
#' \item estimation_length: a length of the estimation period.
#' }
#'
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#' of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#'
#' @seealso \code{\link{apply_market_model}}
#'
#' @examples
#' ## Mean-adjusted-returns model
#' \dontrun{
#' single_return <- get_prices_from_tickers("ALV.DE",
#'                                          start = as.Date("2000-01-01"),
#'                                          end = as.Date("2002-01-01"),
#'                                          quote = "Close",
#'                                          retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     returns(market_model = "mean_adj",
#'             estimation_start = as.Date("2001-03-26"),
#'             estimation_end = as.Date("2001-09-10"))
#' }
#' ## The code above is equivalent to:
#' data(rates)
#' single_return <- returns(rates[, "ALV.DE", drop = FALSE],
#'                          market_model = "mean_adj",
#'                          estimation_start = as.Date("2001-03-26"),
#'                          estimation_end = as.Date("2001-09-10"))
#'
#' ## Market-adjusted-returns model
#' \dontrun{
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#'
#' single_return <- get_prices_from_tickers("ALV.DE",
#'                                          start = as.Date("2000-01-01"),
#'                                          end = as.Date("2002-01-01"),
#'                                          quote = "Close",
#'                                          retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     returns(regressor = rates_indx,
#'             market_model = "mrkt_adj",
#'             estimation_start = as.Date("2001-03-26"),
#'             estimation_end = as.Date("2001-09-10"))
#' }
#' ## The code above is equivalent to:
#' data(rates, rates_indx)
#' single_return <- returns(rates = rates[, "ALV.DE", drop = FALSE],
#'                          regressor = rates_indx,
#'                          market_model = "mrkt_adj",
#'                          estimation_method = "ols",
#'                          estimation_start = as.Date("2001-03-26"),
#'                          estimation_end = as.Date("2001-09-10"))
#'
#' ## Single-index market model
#' \dontrun{
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#'
#' single_return <- get_prices_from_tickers("ALV.DE",
#'                                          start = as.Date("2000-01-01"),
#'                                          end = as.Date("2002-01-01"),
#'                                          quote = "Close",
#'                                          retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     returns(regressor = rates_indx,
#'             market_model = "sim",
#'             estimation_method = "ols",
#'             estimation_start = as.Date("2001-03-26"),
#'             estimation_end = as.Date("2001-09-10"))
#' }
#' ## The code above is equivalent to:
#' data(rates, rates_indx)
#' single_return <- returns(rates = rates[, "ALV.DE", drop = FALSE],
#'                          regressor = rates_indx,
#'                          market_model = "sim",
#'                          estimation_method = "ols",
#'                          estimation_start = as.Date("2001-03-26"),
#'                          estimation_end = as.Date("2001-09-10"))
#'
#' @export
returns <- function(rates, regressor,
                    market_model = c("mean_adj", "mrkt_adj", "sim"),
                    estimation_method = c("ols"), estimation_start,
                    estimation_end) {
    UseMethod("returns")
}

#' @export
returns.zoo <- function(rates, regressor, market_model = c("mean_adj",
                                                           "mrkt_adj", "sim"),
                        estimation_method = c("ols"), estimation_start,
                        estimation_end) {
    # check parameters
    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)

    if(market_model != "mean_adj" & missing(regressor)) {
        stop(paste("For market model", market_model, "specify the regressor."))
    }

    if(estimation_start >= estimation_end) {
        stop("estimation_start should be earlier than estimation_end")
    }
    # Mean Adjusted Market Model
    if(market_model == "mean_adj") {
        k_qnorm <- qnorm(1 - 0.05/2)
        estimation_data <- rates[!is.na(rates)]
        estimation_data <- estimation_data[
            zoo::index(estimation_data) >= estimation_start &
            zoo::index(estimation_data) <= estimation_end]
        delta <- length(estimation_data)
        estimation_mean <- mean(estimation_data)
        estimation_sd <- sd(estimation_data)

        result <- list(observed = rates,
                       predicted = zoo::zoo(estimation_mean, zoo::index(rates)),
                       lower95CI = zoo::zoo(estimation_mean - k_qnorm /
                                            sqrt(delta) * estimation_sd,
                                            zoo::index(rates)),
                       upper95CI = zoo::zoo(estimation_mean + k_qnorm /
                                            sqrt(delta) * estimation_sd,
                                            zoo::index(rates)),
                       abnormal = rates - estimation_mean,
                       market_model = market_model,
                       full_name_market_model = "Mean adjusted market model",
                       estimation_start = estimation_start,
                       estimation_end = estimation_end,
                       estimation_length = delta)
    } else if(market_model == "mrkt_adj") {
        data <- zoo::merge.zoo(rates, regressor, all = TRUE)
        estimation_data <- data[complete.cases(data), ]
        estimation_data <- estimation_data[
            zoo::index(estimation_data) >= estimation_start &
            zoo::index(estimation_data) <= estimation_end]
        delta <- nrow(estimation_data)
        # two variables created, because predict is looking for the same
        # as in lm variables names
        y <- zoo::coredata(estimation_data[, 1])
        x <- zoo::coredata(estimation_data[, 2])
        lm_fit <- lm(y ~ x)
        lm_fit$coefficients <- c(0, 1)
        predicted <- predict.lm(object = lm_fit,
                                newdata = data.frame(x = data[, 2]),
                                interval = c("confidence"), level = 0.95)
        rownames(predicted) <- NULL
        result <- list(observed = data[, 1],
                       predicted = zoo::zoo(predicted[, 1],
                                            zoo::index(data)),
                       lower95CI = zoo::zoo(predicted[, 2],
                                            zoo::index(data)),
                       upper95CI = zoo::zoo(predicted[, 3],
                                            zoo::index(data)),
                       abnormal = data[, 1] - zoo::zoo(predicted[, 1],
                                                       zoo::index(data)),
                       regressor = data[, 2],
                       market_model = market_model,
                       full_name_market_model = "Market Adjusted Market Model",
                       estimation_start = estimation_start,
                       estimation_end = estimation_end,
                       estimation_length = delta)

    } else if(market_model == "sim") {
        if(estimation_method == "ols") {
            data <- zoo::merge.zoo(rates, regressor, all = TRUE)
            estimation_data <- data[complete.cases(data), ]
            estimation_data <- estimation_data[
                zoo::index(estimation_data) >= estimation_start &
                zoo::index(estimation_data) <= estimation_end]
            delta <- nrow(estimation_data)
            # two variables created, because predict is looking for the same
            # as in lm variables names
            y <- zoo::coredata(estimation_data[, 1])
            x <- zoo::coredata(estimation_data[, 2])
            lm_fit <- lm(y ~ x)
            predicted <- predict.lm(object = lm_fit,
                                    newdata = data.frame(x = data[, 2]),
                                    interval = c("confidence"), level = 0.95)
            rownames(predicted) <- NULL
            result <- list(observed = data[, 1],
                           predicted = zoo::zoo(predicted[, 1],
                                                zoo::index(data)),
                           lower95CI = zoo::zoo(predicted[, 2],
                                                zoo::index(data)),
                           upper95CI = zoo::zoo(predicted[, 3],
                                                zoo::index(data)),
                           abnormal = data[, 1] - zoo::zoo(predicted[, 1],
                                                           zoo::index(data)),
                           regressor = data[, 2],
                           market_model = market_model,
                           full_name_market_model = "Single-Index Market Model",
                           estimation_method = estimation_method,
                           full_name_estimation_method =
                               "Ordinary Least Squares",
                           coefficients = c(alpha =
                                                unname(lm_fit$coefficients)[1],
                                            beta =
                                                unname(lm_fit$coefficients)[2]),
                           estimation_start = estimation_start,
                           estimation_end = estimation_end,
                           estimation_length = delta)
        }
    }
    class(result) <- "returns"
    return(result)
}


#' @export
returns.data.frame <- function(rates, regressor, market_model = c("mean_adj",
                                                           "mrkt_adj", "sim"),
                        estimation_method = c("ols"), estimation_start,
                        estimation_end) {
    # check parameters
    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)

    if(market_model != "mean_adj" & missing(regressor)) {
        stop(paste("For market model", market_model, "specify the regressor."))
    }

    if(estimation_start >= estimation_end) {
        stop("estimation_start should be earlier than estimation_end")
    }
    # Mean Adjusted Market Model
    if(market_model == "mean_adj") {
        k_qnorm <- qnorm(1 - 0.05/2)
        estimation_data <- rates[complete.cases(rates), ]
        estimation_data <- estimation_data[
            estimation_data[, 1] >= estimation_start &
            estimation_data[, 1] <= estimation_end, ]
        delta <- nrow(estimation_data)
        estimation_mean <- mean(estimation_data[, 2])
        estimation_sd <- sd(estimation_data[, 2])

        result <- list(observed = zoo::zoo(rates[, 2], rates[, 1]),
                       predicted = zoo::zoo(rep(estimation_mean, nrow(rates)),
                                            rates[, 1]),
                       lower95CI = zoo::zoo(rep(estimation_mean - k_qnorm /
                                                sqrt(delta) * estimation_sd,
                                                nrow(rates)),
                                            rates[, 1]),
                       upper95CI = zoo::zoo(rep(estimation_mean + k_qnorm /
                                                    sqrt(delta) * estimation_sd,
                                                nrow(rates)),
                                            rates[, 1]),
                       abnormal = zoo::zoo(rates[, 2], rates[, 1]) -
                           zoo::zoo(rep(estimation_mean, nrow(rates)),
                                    rates[, 1]),
                       market_model = market_model,
                       full_name_market_model = "Mean adjusted market model",
                       estimation_start = estimation_start,
                       estimation_end = estimation_end,
                       estimation_length = delta)
    } else if(market_model == "mrkt_adj") {
        data <- merge(rates, regressor, by = "date", all = TRUE)
        estimation_data <- data[complete.cases(data), ]
        estimation_data <- estimation_data[
            estimation_data[, 1] >= estimation_start &
            estimation_data[, 1] <= estimation_end, ]
        delta <- nrow(estimation_data)
        # two variables created, because predict is looking for the same
        # as in lm variables names
        y <- estimation_data[, 2]
        x <- estimation_data[, 3]
        lm_fit <- lm(y ~ x)
        lm_fit$coefficients <- c(0, 1)
        predicted <- predict.lm(object = lm_fit,
                                newdata = data.frame(x = data[, 3]),
                                interval = c("confidence"), level = 0.95)
        rownames(predicted) <- NULL
        result <- list(observed = zoo::zoo(data[, 2], data[, 1]),
                       predicted = zoo::zoo(predicted[, 1], data[, 1]),
                       lower95CI = zoo::zoo(predicted[, 2], data[, 1]),
                       upper95CI = zoo::zoo(predicted[, 3], data[, 1]),
                       abnormal = zoo::zoo(data[, 2], data[, 1]) -
                           zoo::zoo(predicted[, 1], data[, 1]),
                       regressor = zoo::zoo(data[, 3], data[, 1]),
                       market_model = market_model,
                       full_name_market_model = "Market Adjusted Market Model",
                       estimation_start = estimation_start,
                       estimation_end = estimation_end,
                       estimation_length = delta)

    } else if(market_model == "sim") {
        if(estimation_method == "ols") {
            data <- merge(rates, regressor, by = "date", all = TRUE)
            estimation_data <- data[complete.cases(data), ]
            estimation_data <- estimation_data[
                estimation_data[, 1] >= estimation_start &
                estimation_data[, 1] <= estimation_end, ]
            delta <- nrow(estimation_data)
            # two variables created, because predict is looking for the same
            # as in lm variables names
            y <- estimation_data[, 2]
            x <- estimation_data[, 3]
            lm_fit <- lm(y ~ x)
            predicted <- predict.lm(object = lm_fit,
                                    newdata = data.frame(x = data[, 3]),
                                    interval = c("confidence"), level = 0.95)
            rownames(predicted) <- NULL
            result <- list(observed = zoo::zoo(data[, 2], data[, 1]),
                           predicted = zoo::zoo(predicted[, 1], data[, 1]),
                           lower95CI = zoo::zoo(predicted[, 2], data[, 1]),
                           upper95CI = zoo::zoo(predicted[, 3], data[, 1]),
                           abnormal = zoo::zoo(data[, 2], data[, 1]) -
                               zoo::zoo(predicted[, 1], data[, 1]),
                           regressor = zoo::zoo(data[, 3], data[, 1]),
                           market_model = market_model,
                           full_name_market_model = "Single-Index Market Model",
                           estimation_method = estimation_method,
                           full_name_estimation_method =
                               "Ordinary Least Squares",
                           coefficients = c(alpha =
                                                unname(lm_fit$coefficients)[1],
                                            beta =
                                                unname(lm_fit$coefficients)[2]),
                           estimation_start = estimation_start,
                           estimation_end = estimation_end,
                           estimation_length = delta)
        }
    }
    class(result) <- "returns"
    return(result)
}
