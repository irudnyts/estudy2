get_abnormal_returns <- function(companies_rates, index_rates,
                               model =
                                   c("mean_adjusted", "market_adjusted", "OLS"),
                               estimation_start, estimation_end,
                               event_start, event_end) {

    if(is.null(attributes(companies_rates)$dim)) {
        attributes(companies_rates)$dim <- c(length(companies_rates), 1)
    }
    for(i in 1:ncol(companies_rates)) {
        # perform the linear regression on the estimation period
        # calculate abnormals
    }

}
