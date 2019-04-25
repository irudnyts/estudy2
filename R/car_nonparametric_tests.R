car_rank_test <- function(list_of_returns, car_start, car_end,
                          percentage = 90) {

    # check car_start and car_end for class and value validity
    if(!inherits(car_start, "Date")) {
        stop("car_start must be an object of class Date.")
    }
    if(!inherits(car_end, "Date")) {
        stop("car_end must be an object of class Date.")
    }
    if(car_start > car_end) {
        stop("car_start must be earlier than car_end.")
    }

    # zoo objects of abnormal returns
    full_rank <- NULL
    event_rank <- NULL
    delta_full <- numeric(length(list_of_returns))
    avg_rank <- numeric(length(list_of_returns))
    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= car_start) {
            stop(paste0("For ", as.character(i), "-th company estimation",
                        " period overlaps with event period."))
        }

        company_full_abnormal <- zoo::as.zoo(c(
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >=
                    list_of_returns[[i]]$estimation_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <=
                    list_of_returns[[i]]$estimation_end],
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >= car_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <= car_end]))

        company_full_rank <- zoo::zoo(rank(x = zoo::coredata(company_full_abnormal),
                                           na.last = "keep",
                                           ties.method = "average"),
                                      zoo::index(company_full_abnormal))
        company_event_rank <- zoo::as.zoo(company_full_rank[
            zoo::index(company_full_rank) >= car_start &
                zoo::index(company_full_rank) <= car_end])

        if(is.null(full_rank)) {
            full_rank <- company_full_rank
        } else {
            full_rank <- merge(full_rank, company_full_rank, all = TRUE)
        }

        if(is.null(event_rank)) {
            event_rank <- company_event_rank
        } else {
            event_rank <- merge(event_rank, company_event_rank, all = TRUE)
        }

        delta_full[i] <-
            length(company_full_abnormal[!is.na(company_full_abnormal)])
        avg_rank[i] <- mean(company_full_rank, na.rm = TRUE)
    }

    event_number_of_companies <- rowSums(!is.na(event_rank))
    event_number_of_companies[event_number_of_companies == 0] <- NA
    event_percentages <- event_number_of_companies / ncol(event_rank) * 100
    average_percentage <- mean(
        event_percentages[event_percentages > percentage],
        na.rm = TRUE
    )

    number_of_companies <- rowSums(!is.na(full_rank))
    number_of_companies[number_of_companies == 0] <- NA

    event_rank_tidy <- event_rank[event_percentages > percentage, ]

    full_rank <- as.matrix(full_rank)
    event_rank_tidy <- as.matrix(event_rank_tidy)

    mean_rank <- mean(avg_rank, na.rm = TRUE)

    statistic <- nrow(event_rank_tidy) ^ 0.5 *
        (mean(event_rank_tidy, na.rm = TRUE) - mean_rank) /
        (sum((rowMeans(full_rank, na.rm = TRUE) - mean_rank) ^ 2, na.rm = TRUE) /
             mean(delta_full, na.rm = TRUE)) ^ 0.5

    if(abs(statistic) >= const_q3) {
        significance <- "***"
    } else if(abs(statistic) >= const_q2) {
        significance <- "**"
    } else if(abs(statistic) >= const_q1) {
        significance <- "*"
    } else {
        significance <- ""
    }

    result <- data.frame(name = "car_rank_test",
                         car_start = car_start,
                         car_end = car_end,
                         average_percentage = average_percentage,
                         statistic = statistic,
                         number_of_days = nrow(event_rank_tidy),
                         significance = significance)
    return(result)
}
