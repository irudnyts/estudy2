apply_market_model <- function(rates, regressor, market_model = c("sim"),
                               estimation_method = c("ols"), estimation_start,
                               estimation_end) {

    #------------------------------------------
    # Extend if regressor is list of regressors
    #------------------------------------------

    #------------------------------------------------
    # Add Brown and Warner Market Adjusted model etc.
    #------------------------------------------------

    #-----------------------------------------------------------
    # Add Brown and Warner beta estimators from Brown and Warner
    #-----------------------------------------------------------

    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)
    if (market_model == "sim") {
        if (estimation_method == "ols") {
            data <- merge(rates, regressor)
            data <- data[complete.cases(data)]
            estimation_data <- data[time(data) >= estimation_start &
                             time(data) <= estimation_end]
            delta <- nrow(estimation_data)
            ols_coef <- lm(coredata(estimation_data[, 1]) ~
                               coredata(estimation_data[, 2]))$coefficients

            result <- list(observed = rates,
                           predicted = ols_coef[1] + ols_coef[2] * regressor,
                           abnormal = data[, 1] - ols_coef[1] -
                               ols_coef[2] * data[, 2],
                           regressor = regressor,
                           market_model = market_model,
                           full_name_market_model = "Single-Index Market Model",
                           estimation_method = estimation_method,
                           full_name_estimation_method =
                               "Ordinary Least Squares",
                           coefficients = c(alpha = unname(ols_coef)[1],
                                     beta = unname(ols_coef)[2]),
                           estimation_start = estimation_start,
                           estimation_end = estimation_end,
                           estimation_length = delta)
            class(result) <- "returns"
            return(result)

        }
    }
}

#' @export
returns <- function(rates, regressor, market_model = c("mean_adj", "mrkt_adj",
                                                       "sim"),
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
        data <- rates[!is.na(rates)]
        estimation_data <- data[time(data) >= estimation_start &
                                    time(data) <= estimation_end]
        delta <- length(estimation_data)
        estimation_mean <- mean(estimation_data)
        estimation_sd <- sd(estimation_data)

        result <- list(observed = rates,
                       predicted = zoo::zoo(estimation_mean, time(rates)),
                       lower95CI = zoo::zoo(estimation_mean - k_qnorm /
                                            sqrt(delta) * estimation_sd,
                                            time(rates)),
                       upper95CI = zoo::zoo(estimation_mean + k_qnorm /
                                            sqrt(delta) * estimation_sd,
                                            time(rates)),
                       abnormal = rates - estimation_mean,
                       market_model = market_model,
                       full_name_market_model = "Mean adjusted market model",
                       estimation_start = estimation_start,
                       estimation_end = estimation_end,
                       estimation_length = delta)
    } else if(market_model == "mrkt_adj") {
        data <- merge(rates, regressor)
        estimation_data <- data[complete.cases(data)]
        estimation_data <- estimation_data[
            time(estimation_data) >= estimation_start &
            time(estimation_data) <= estimation_end]
        delta <- nrow(estimation_data)
        # two variables created, because predict is looking for the same
        # as in lm variables names
        y <- coredata(estimation_data[, 1])
        x <- coredata(estimation_data[, 2])
        lm_fit <- lm(y ~ x)
        lm_fit$coefficients <- c(0, 1)
        predicted <- predict.lm(object = lm_fit,
                                newdata = data.frame(x = data[, 2]),
                                interval = c("confidence"), level = 0.95)
        result <- list(observed = data[, 1],
                       predicted = zoo::zoo(predicted[, 1],
                                            time(data)),
                       lower95CI = zoo::zoo(predicted[, 2],
                                            time(data)),
                       upper95CI = zoo::zoo(predicted[, 3],
                                            time(data)),
                       abnormal = data[, 1] - zoo::zoo(predicted[, 1],
                                                       time(data)),
                       regressor = data[, 2],
                       market_model = market_model,
                       full_name_market_model = "Market Adjusted Market Model",
                       estimation_start = estimation_start,
                       estimation_end = estimation_end,
                       estimation_length = delta)

    } else if(market_model == "sim") {
        if(estimation_method == "ols") {
            browser()
            data <- merge(rates, regressor)
            estimation_data <- data[complete.cases(data)]
            estimation_data <- estimation_data[
                time(estimation_data) >= estimation_start &
                time(estimation_data) <= estimation_end]
            delta <- nrow(estimation_data)
            # two variables created, because predict is looking for the same
            # as in lm variables names
            y <- coredata(estimation_data[, 1])
            x <- coredata(estimation_data[, 2])
            lm_fit <- lm(y ~ x)
            predicted <- predict.lm(object = lm_fit,
                                    newdata = data.frame(x = data[, 2]),
                                    interval = c("confidence"), level = 0.95)
            result <- list(observed = data[, 1],
                           predicted = zoo::zoo(predicted[, 1],
                                                time(data)),
                           lower95CI = zoo::zoo(predicted[, 2],
                                                time(data)),
                           upper95CI = zoo::zoo(predicted[, 3],
                                                time(data)),
                           abnormal = data[, 1] - zoo::zoo(predicted[, 1],
                                                           time(data)),
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
