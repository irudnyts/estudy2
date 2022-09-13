#' Apply a market model and return a list of \code{returns} objects.
#'
#' The function applies a given market model to securities' rates of returns and
#' returns a list of \code{returns} objects for each security, which can be
#' passed directly to a whole battery of tests.
#'
#' The generic function is dispatched for such classes as \code{list},
#' \code{data.frame}, and \code{zoo}. If \code{same_regressor_for_all} is TRUE,
#' and \code{regressors} has the length greater than one, the first element of
#' \code{regressors} will be applied for each security in \code{rates}.
#'
#' @param rates an object of \code{list}, \code{data.frame}, \code{zoo}
#' containing rates of returns of securities.
#' @param regressors an object of the same class as \code{rates} containing
#' regressors. The argument can be omitted, if market model is \code{mean_adj}.
#' \code{regressors} must have the same number of components as \code{rates}
#' except cases when the same regressor is used for all securities.
#' @param same_regressor_for_all logical. Should the same regressor be used for
#' each security? The default value is TRUE.
#' @param market_model a character indicating the market model among
#' \code{mean_adj}, \code{mrkt_adj}, and \code{sim}.
#' @param estimation_method a character specifying an estimation method for
#' \code{sim} model.
#' @param estimation_start an object of \code{Date} class giving the first date
#' of the estimation period.
#' @param estimation_end an object of \code{Date} class giving the last date of
#' the estimation period.
#' @return A list of \code{returns} objects.
#'
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#' of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#'
#' @seealso \code{\link{returns}}
#'
#' @examples
#' ## 1. Mean-adjusted-returns model
#' \dontrun{
#' library("magrittr")
#' tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")
#' securities_returns <- get_prices_from_tickers(tickers,
#'                                               start = as.Date("2019-04-01"),
#'                                               end = as.Date("2020-04-01"),
#'                                               quote = "Close",
#'                                               retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(market_model = "mean_adj",
#'                        estimation_start = as.Date("2019-04-01"),
#'                        estimation_end = as.Date("2020-03-13"))
#' }
#' ## The result of the code above is equivalent to:
#' data(rates)
#' securities_returns <- apply_market_model(
#'     rates,
#'     market_model = "mean_adj",
#'     estimation_start = as.Date("2019-04-01"),
#'     estimation_end = as.Date("2020-03-13")
#' )
#'
#' ## 2. Market-adjusted-returns model
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^GSPC",
#'                                       start = as.Date("2019-04-01"),
#'                                       end = as.Date("2020-04-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")
#' securities_returns <- get_prices_from_tickers(tickers,
#'                                               start = as.Date("2019-04-01"),
#'                                               end = as.Date("2020-04-01"),
#'                                               quote = "Close",
#'                                               retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "mrkt_adj",
#'                        estimation_start = as.Date("2019-04-01"),
#'                        estimation_end = as.Date("2020-03-13"))
#' }
#' ## The result of the code above is equivalent to:
#' data(rates, rates_indx)
#' securities_returns <- apply_market_model(
#'     rates = rates,
#'     regressor = rates_indx,
#'     same_regressor_for_all = TRUE,
#'     market_model = "mrkt_adj",
#'     estimation_start = as.Date("2019-04-01"),
#'     estimation_end = as.Date("2020-03-13")
#' )
#'
#' ## 3. Single-index market model
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^GSPC",
#'                                       start = as.Date("2019-04-01"),
#'                                       end = as.Date("2020-04-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")
#' securities_returns <- get_prices_from_tickers(tickers,
#'                                               start = as.Date("2019-04-01"),
#'                                               end = as.Date("2020-04-01"),
#'                                               quote = "Close",
#'                                               retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2019-04-01"),
#'                        estimation_end = as.Date("2020-03-13"))
#' }
#' ## The result of the code above is equivalent to:
#' data(rates, rates_indx)
#' securities_returns <- apply_market_model(
#'     rates = rates,
#'     regressor = rates_indx,
#'     same_regressor_for_all = TRUE,
#'     market_model = "sim",
#'     estimation_method = "ols",
#'     estimation_start = as.Date("2019-04-01"),
#'     estimation_end = as.Date("2020-03-13")
#' )
#'
#' @export
apply_market_model <- function(rates, regressors, same_regressor_for_all = TRUE,
                               market_model = c("mean_adj", "mrkt_adj", "sim"),
                               estimation_method = c("ols","gls"), estimation_start,
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
                                          estimation_method = c("ols","gls"),
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
        regressors <- zoo::zoo(x = regressors[, rep(1, ncol(rates))],
                               order.by = zoo::index(regressors))
        # first_element <- regressors[, 1]
        # regressors <- first_element[, rep(1, ncol(rates))]
    }

    list_of_returns <- list()
    if(market_model == "mean_adj") {
        for(i in 1:ncol(rates)) {
            current_rates <- zoo::zoo(x = rates[, i],
                                      order.by = zoo::index(rates))
            list_of_returns[[i]] <- returns(rates = current_rates,
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "mrkt_adj") {
        for(i in 1:ncol(rates)) {
            current_rates <- zoo::zoo(x = rates[, i],
                                      order.by = zoo::index(rates))
            list_of_returns[[i]] <- returns(rates = current_rates,
                                            regressor = regressors[, i],
                                            market_model = market_model,
                                            estimation_start = estimation_start,
                                            estimation_end = estimation_end)
        }
    } else if(market_model == "sim"){
        for(i in 1:ncol(rates)) {
            current_rates <- zoo::zoo(x = rates[, i],
                                      order.by = zoo::index(rates))
            list_of_returns[[i]] <- returns(rates = current_rates,
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

#' Constructor of an object of S3 class \code{returns}.
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
#' @param estimation_method a character specifying an estimation method for
#' \code{sim} model.
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
#' library("zoo")
#' ## 1. Mean-adjusted-returns model
#' \dontrun{
#' library("magrittr")
#' single_return <- get_prices_from_tickers("AMZN",
#'                                          start = as.Date("2019-04-01"),
#'                                          end = as.Date("2020-04-01"),
#'                                          quote = "Close",
#'                                          retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     returns(market_model = "mean_adj",
#'             estimation_start = as.Date("2019-04-01"),
#'             estimation_end = as.Date("2020-03-13"))
#' }
#' ## The result of the code above is equivalent to:
#' data(rates)
#' single_return <- returns(rates[, "AMZN"],
#'                          market_model = "mean_adj",
#'                          estimation_start = as.Date("2019-04-01"),
#'                          estimation_end = as.Date("2020-03-13"))
#'
#' ## 2. Market-adjusted-returns model
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^GSPC",
#'                                       start = as.Date("2019-04-01"),
#'                                       end = as.Date("2020-04-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#'
#' single_return <- get_prices_from_tickers("AMZN",
#'                                          start = as.Date("2019-04-01"),
#'                                          end = as.Date("2020-04-01"),
#'                                          quote = "Close",
#'                                          retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     returns(regressor = rates_indx,
#'             market_model = "mrkt_adj",
#'             estimation_start = as.Date("2019-04-01"),
#'             estimation_end = as.Date("2020-03-13"))
#' }
#' ## The result of the code above is equivalent to:
#' data(rates, rates_indx)
#' single_return <- returns(rates = rates[, "AMZN", drop = FALSE],
#'                          regressor = rates_indx,
#'                          market_model = "mrkt_adj",
#'                          estimation_method = "ols",
#'                          estimation_start = as.Date("2019-04-01"),
#'                          estimation_end = as.Date("2020-03-13"))
#'
#' ## 3. Single-index market model
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^GSPC",
#'                                       start = as.Date("2019-04-01"),
#'                                       end = as.Date("2020-04-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#'
#' single_return <- get_prices_from_tickers("AMZN",
#'                                          start = as.Date("2019-04-01"),
#'                                          end = as.Date("2020-04-01"),
#'                                          quote = "Close",
#'                                          retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     returns(regressor = rates_indx,
#'             market_model = "sim",
#'             estimation_method = "ols",
#'             estimation_start = as.Date("2019-04-01"),
#'             estimation_end = as.Date("2020-03-13"))
#' }
#' ## The result of the code above is equivalent to:
#' data(rates, rates_indx)
#' single_return <- returns(rates = rates[, "AMZN", drop = FALSE],
#'                          regressor = rates_indx,
#'                          market_model = "sim",
#'                          estimation_method = "ols",
#'                          estimation_start = as.Date("2019-04-01"),
#'                          estimation_end = as.Date("2020-03-13"))
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
        k_qnorm <- stats::qnorm(1 - 0.05/2)
        estimation_data <- rates[!is.na(rates)]
        estimation_data <- estimation_data[
            zoo::index(estimation_data) >= estimation_start &
            zoo::index(estimation_data) <= estimation_end]
        delta <- length(estimation_data)
        if(delta == 0) {
            stop(paste0("The estimation window contains no data. Check ",
                        paste(names(rates), collapse = " and "),
                        " for missing values."))
        }
        estimation_mean <- mean(estimation_data)
        estimation_sd <- stats::sd(estimation_data)

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
        estimation_data <- data[stats::complete.cases(data), ]
        estimation_data <- estimation_data[
            zoo::index(estimation_data) >= estimation_start &
            zoo::index(estimation_data) <= estimation_end]
        delta <- nrow(estimation_data)
        if(delta == 0) {
            stop(paste0("The estimation window contains no data. Check ",
                        paste(names(estimation_data), collapse = " and "),
                        " for missing values."))
        }
        # two variables created, because predict is looking for the same
        # as in lm variables names
        y <- zoo::coredata(estimation_data[, 1])
        x <- zoo::coredata(estimation_data[, 2])
        lm_fit <- stats::lm(y ~ x)
        lm_fit$coefficients <- c(0, 1)
        predicted <- stats::predict.lm(object = lm_fit,
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
            estimation_data <- data[stats::complete.cases(data), ]
            estimation_data <- estimation_data[
                zoo::index(estimation_data) >= estimation_start &
                zoo::index(estimation_data) <= estimation_end]
            delta <- nrow(estimation_data)
            if(delta == 0) {
                stop(paste0("The estimation window contains no data. Check ",
                            paste(names(estimation_data), collapse = " and "),
                            " for missing values."))
            }
            # two variables created, because predict is looking for the same
            # as in lm variables names
            y <- zoo::coredata(estimation_data[, 1])
            x <- zoo::coredata(estimation_data[, 2])
            lm_fit <- stats::lm(y ~ x)
            predicted <- stats::predict.lm(object = lm_fit,
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
                        estimation_method = c("ols","gls"), estimation_start,
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
        k_qnorm <- stats::qnorm(1 - 0.05/2)
        estimation_data <- rates[stats::complete.cases(rates), ]
        estimation_data <- estimation_data[
            estimation_data[, 1] >= estimation_start &
            estimation_data[, 1] <= estimation_end, ]
        delta <- nrow(estimation_data)
        if(delta == 0) {
            stop(paste0("The estimation window contains no data. Check ",
                        paste(names(estimation_data)[-1], collapse = " and "),
                        " for missing values."))
        }
        estimation_mean <- mean(estimation_data[, 2])
        estimation_sd <- stats::sd(estimation_data[, 2])

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
        estimation_data <- data[stats::complete.cases(data), ]
        estimation_data <- estimation_data[
            estimation_data[, 1] >= estimation_start &
            estimation_data[, 1] <= estimation_end, ]
        delta <- nrow(estimation_data)
        if(delta == 0) {
            stop(paste0("The estimation window contains no data. Check ",
                        paste(names(estimation_data)[-1], collapse = " and "),
                        " for missing values."))
        }
        # two variables created, because predict is looking for the same
        # as in lm variables names
        y <- estimation_data[, 2]
        x <- estimation_data[, 3]
        lm_fit <- stats::lm(y ~ x)
        lm_fit$coefficients <- c(0, 1)
        predicted <- stats::predict.lm(object = lm_fit,
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
            estimation_data <- data[stats::complete.cases(data), ]
            estimation_data <- estimation_data[
                estimation_data[, 1] >= estimation_start &
                estimation_data[, 1] <= estimation_end, ]
            delta <- nrow(estimation_data)
            if(delta == 0) {
                stop(paste0("The estimation window contains no data. Check ",
                            paste(names(estimation_data)[-1], collapse = " and "),
                            " for missing values."))
            }
            # two variables created, because predict is looking for the same
            # as in lm variables names
            y <- estimation_data[, 2]
            x <- estimation_data[, 3]
            lm_fit <- stats::lm(y ~ x)
            predicted <- stats::predict.lm(object = lm_fit,
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
        } else if (estimation_method == "gls") {
            data <- merge(rates, regressor, by = "date", all = TRUE)
            estimation_data <- data[stats::complete.cases(data), ]
            estimation_data <- estimation_data[
                estimation_data[, 1] >= estimation_start &
                estimation_data[, 1] <= estimation_end, ]
            delta <- nrow(estimation_data)
            if(delta == 0) {
                stop(paste0("The estimation window contains no data. Check ",
                            paste(names(estimation_data)[-1], collapse = " and "),
                            " for missing values."))
            }
            # two variables created, because predict is looking for the same
            # as in lm variables names
            y <- estimation_data[, 2]
            x <- estimation_data[, 3]
            lm_fit <- stats::lm(y ~ x)
            predicted <- stats::predict.lm(
                object = lm_fit,
                newdata = data.frame(x = data[, 3]),
                interval = c("confidence"),
                level = 0.95
            )
            # Function for correct gls model specification
            make_gls <- function(d = estimation_data, ar.res=NULL) {
                # add an extra feature that allows for different variance structures over time
                names(d) <- c("date","y","x")
                d['month'] <- format(d$date,"%m")

                # specify possible different variance structures
                vf1 <- nlme::varFixed(~x)
                vf2 <- nlme::varIdent(form = ~ x | month)
                vf3 <- nlme::varPower(form = ~ x)
                vf4 <- nlme::varPower(form = ~ x | month)
                vf5 <- nlme::varConstPower(form = ~ x)
                vf6 <- nlme::varConstPower(form = ~ x | month)
                vf7 <- nlme::varExp(form = ~x)

                # specify possible models
                lm_gls  <- try(nlme::gls(y ~ x, data = d), silent = TRUE)
                vf1_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf1), silent = TRUE)
                vf2_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf2), silent = TRUE)
                vf3_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf3), silent = TRUE)
                vf4_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf4), silent = TRUE)
                vf5_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf5), silent = TRUE)
                vf6_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf6), silent = TRUE)
                vf7_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf7), silent = TRUE)

                # Find & remove all models that run into errors
                reg_res <- list(lm_gls, vf1_gls, vf2_gls, vf3_gls, vf4_gls, vf5_gls, vf6_gls, vf7_gls)
                reg_res_logical <- lapply(reg_res, function (a) return(class(a) != "gls"))
                if (any(reg_res_logical == TRUE) == TRUE){
                    regs <- reg_res[-which((reg_res_logical == TRUE))]
                } else {
                    regs <- reg_res
                }

                # Select best fitting model according to AIC criterion
                aic <- sapply(regs, AIC)
                gls_model <- regs[-which((aic != min(aic)) == TRUE)]
                aic_logical <- aic == min(aic)

                return(gls_model)
            }
            # function for gls prediction
            predict.gls <- function(gls_model, newdata, level = 0.95) {
                # obtain the model's coefficients
                model_confint <- try(nlme::intervals(gls_model, level = level),
                                     silent = TRUE)

                if ((class(model_confint) == "try-error") == TRUE) {
                    print("An error occured in calculation of intervals")
                    problem <- model_confint
                    return(problem)

                } else if ((class(model_confint) == "intervals.gls") == TRUE) {
                    model_coeff <- model_confint[[1]]

                    lwr_intercept <- model_coeff[1, 1]
                    fit_intercept <- model_coeff[1, 2]
                    upr_intercept <- model_coeff[1, 3]

                    lwr_beta <- model_coeff[2, 1]
                    fit_beta <- model_coeff[2, 2]
                    upr_beta <- model_coeff[2, 3]

                    # calculate results, where 'newdata' is the independent variable
                    Y_lwr <- lwr_intercept + (lwr_beta * newdata)
                    Y_fit <- fit_intercept + (fit_beta * newdata)
                    Y_upr <- upr_intercept + (upr_beta * newdata)

                    res <- as.matrix(
                        as.data.frame(list(Y_fit, Y_lwr, Y_upr),
                                      col.names = c("fit", "lwr", "upr"))
                                     )
                    return(res)
                }
            }
            # Tests for appropriate model specification
            significance <- 0.01

            bp_test <- lmtest::bptest(lm_fit)
            bg_test1 <- lmtest::bgtest(lm_fit, order = 1)

            scedas <- lmtest::bptest(lm_fit)["p.value"] < significance
            autocorr <- lmtest::bgtest(lm_fit, order = 1)["p.value"] < significance

            # specify remedies
            if ((scedas == TRUE) & (autocorr == TRUE)) {
                # model has both heteroscedasticity and autocorrelation
                # Calc autocorrelation of residuals
                print("Correcting for autocorr & heteroscedasticity of residuals...")
                corr <- stats::acf(lm_fit$residuals,
                                   type = "correlation",
                                   plot = FALSE)
                ARcorr <- nlme::corAR1(value = corr$acf[[2]],
                                       form = ~ 1,
                                       fixed = FALSE)
                gls_fit <- make_gls(d = estimation_data,
                                    ar.res = ARcorr)

                predict_attempt <- predict.gls(gls_model = gls_fit[[1]],
                                               newdata = data[, 3],
                                               level = 0.95)
                if (all(class(predict_attempt) == "try-error") == TRUE) {
                    predicted <- predicted
                    print("Deffering to OLS model prediction.")
                } else if (all(class(predict_attempt) == c("matrix", "array")) == TRUE) {
                    predicted <- predict_attempt
                }

            } else if ((scedas == TRUE) & (autocorr == FALSE)) {
                # model has heteroscedasticity but no autocorrelation
                # Fit gls corrected for heteroscedasticity
                print("Correcting for heteroscedasticity of residuals. No autocorrelation found.")
                gls_fit <- make_gls(d = estimation_data,
                                    ar.res = NULL)

                predicted <- predict.gls(gls_model = gls_fit[[1]],
                                         newdata = data[, 3],
                                         level = 0.95)
                if (all(class(predict_attempt) == "try-error") == TRUE) {
                    print("Deffering to OLS model prediction.")
                    predicted <- predicted
                } else if (all(class(predict_attempt) == c("matrix", "array")) == TRUE) {
                    predicted <- predict_attempt
                }

            } else if ((scedas == FALSE) & (autocorr == TRUE)) {
                # model has autocorrelation but no heteroscedasticity
                # Calcl autocorrelation of residuals
                print("Correcting for autocorr of residuals. No heteroscedasticity found.")
                corr <- stats::acf(lm_fit$residuals,
                                   type = "correlation",
                                   plot = FALSE)
                ARcorr <- nlme::corAR1(value = corr$acf[[2]],
                                       form = ~ 1,
                                       fixed = FALSE)

                gls_fit <- nlme::gls(y ~ x,
                                     correlation = ARcorr,
                                     weights = NULL)

                predicted <- predict.gls(gls_model = gls_fit,
                                         newdata = data[, 3],
                                         level = 0.95)
                if (all(class(predict_attempt) == "try-error") == TRUE) {
                    print("Deffering to OLS model prediction.")
                    predicted <- predicted
                } else if (all(class(predict_attempt) == c("matrix", "array")) == TRUE) {
                    predicted <- predict_attempt
                }

            } else {
                # if no tests TRUE then OLS model remains OLS
                predicted <- predicted
            }
            # store results
            rownames(predicted) <- NULL
            result <- list(
                observed = zoo::zoo(data[, 2], data[, 1]),
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
                coefficients = c(
                    alpha =
                        unname(lm_fit$coefficients)[1],
                    beta =
                        unname(lm_fit$coefficients)[2]
                ),
                estimation_start = estimation_start,
                estimation_end = estimation_end,
                estimation_length = delta
            )
        }
    }
    class(result) <- "returns"
    return(result)
}
