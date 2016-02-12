#' @export
car_parametric_tests <- function() {

}

#' @export
car_lamb <- function(list_of_returns, car_start, car_end, percentage) {
    daily_lamb_statistics <- lamb(list_of_returns, car_start, car_end)
    daily_lamb_statistics_tidy <- daily_lamb_statistics[daily_lamb_statistics[, 3] > percentage &
                                                        daily_lamb_statistics[, 3] > 0, ]
    percentage <- mean(daily_lamb_statistics_tidy[, 3])

    browser()
    statistic <- sum(daily_lamb_statistics_tidy[, 5], na.rm = T) /
        sqrt(nrow(daily_lamb_statistics_tidy))
    significance <- ""
    significance <- ifelse(test = abs(statistic) >= const_q1,
                           yes = "*", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q2,
                           yes = "**", no = "")
    significance <- ifelse(test = abs(statistic) >= const_q3,
                           yes = "***", no = "")
    result <- list(car_start = car_start, car_end = car_end,
                   average_percentage = percentage, statistic = statistic,
                   number_of_days = nrow(daily_lamb_statistics_tidy),
                   significance = significance)
    return(result)
}
