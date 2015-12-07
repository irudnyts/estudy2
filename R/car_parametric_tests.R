#' @export
car_parametric_tests <- function() {

}

#' @export
car_lamb <- function(list_of_returns, car_start, car_end) {
    daily_lamb_statistics <- lamb(list_of_returns, car_start, car_end)
    percentage <- mean(daily_lamb_statistics[3])
    statistic <- sum(daily_lamb_statistics[5], na.rm = T) /
        sqrt(length(daily_lamb_statistics))
    significance <- ""
    significance <- ifelse(test = abs(statistics) >= const_q1,
                           yes = "*")
    significance <- ifelse(test = abs(statistics) >= const_q2,
                           yes = "**")
    significance <- ifelse(test = abs(statistics) >= const_q3,
                           yes = "***")
    result <- list(car_start = car_start, car_end = car_end,
                   percentage = percentage, statistic = statistic,
                   significance = significance)
}
