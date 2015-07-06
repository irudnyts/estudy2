get_abnormal_return_parallel <- function(companies_rates, index_rates,
                                model =
                                    c("mean_adjusted", "market_adjusted", "OLS"),
                                estimation_start, estimation_end,
                                event_start, event_end,
                                system = c("win", "mac")) {

    if(is.null(attributes(companies_rates)$dim)) {
        attributes(companies_rates)$dim <- c(length(companies_rates), 1)
    }
    foreach() %dopar% {

    }
    # for(i in 1:ncol(companies_rates)) {
        # perform the linear regression on the estimation period
        # calculate abnormals
    # }

}
