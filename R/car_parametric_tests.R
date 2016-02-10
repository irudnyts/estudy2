#' @export
car_parametric_tests <- function() {

}

#' @export
car_lamb <- function(list_of_returns, car_start, car_end) {
    daily_lamb_statistics <- lamb(list_of_returns, car_start, car_end)
    percentage <- mean(daily_lamb_statistics[, 3]) # exclude weekends




    statistic <- sum(daily_lamb_statistics[, 5], na.rm = T) /
        sqrt(nrow(daily_lamb_statistics))
    significance <- ""
    significance <- ifelse(test = abs(statistic) >= const_q1,
                           yes = "*", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q2,
                           yes = "**", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q3,
                           yes = "***", no = "")
    result <- list(car_start = car_start, car_end = car_end,
                   percentage = percentage, statistic = statistic,
                   number_of_days = length(daily_lamb_statistics), # must be nrow()
                   significance = significance)
    # return(result)
    return("DO NOT USE")
}
