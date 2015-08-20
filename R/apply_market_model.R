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
