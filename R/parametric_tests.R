# use colSums instead apply


parametric_tests <- function(list_of_returns, event_start, event_end, all = T,
                             tests = c("brown_warner_1980", "brown_warner_1985",
                                       "t_test", "patell", "boehmer")) {

    # for the reference see the paper of Boehmer 1991, Brown and Warner 1985
    # options for tests = c("brown_warner", "patell", "t-test", "hybrid")

    if(all == T) {
        tests <- c("brown_warner_1980", "brown_warner_1985",
                  "t_test", "patell", "boehmer")
    } else {
        tests <- match.arg(tests)
    }

    for(i in seq_along(tests)) {
        if(tests[i] == "brown_warner_1980") {
            current_table <- brown_warner_1980(list_of_returns, event_start,
                                               event_end)
            colnames(current_table)[c(5, 6)] <- c("bw1980_stat",
                                                     "bw1980_signif")
        }
        if(tests[i] == "brown_warner_1985") {
            current_table <- brown_warner_1985(list_of_returns, event_start,
                                               event_end)
            colnames(current_table)[c(5, 6)] <- c("bw1985_stat",
                                                     "bw1985_signif")
        }
        if(tests[i] == "t_test") {
            current_table <- t_test(list_of_returns, event_start, event_end)
            colnames(current_table)[c(5, 6)] <- c("t_test_stat",
                                                     "t_test_signif")
        }
        if(tests[i] == "patell") {
            current_table <- patell(list_of_returns, event_start, event_end)
            colnames(current_table)[c(5, 6)] <- c("pt_stat", "pt_signif")
        }
        if(tests[i] == "boehmer") {
            current_table <- boehmer(list_of_returns, event_start, event_end)
            colnames(current_table)[c(5, 6)] <- c("bh_stat", "bh_signif")
        }

        if(i == 1) {
            result <- current_table
        } else {
            result_column_names <- c(colnames(result),
                                     colnames(current_table[, c(5, 6)]))
            result <- merge(x = result, y = current_table[, c(1, 5, 6)],
                            by = "date", all = T)
            colnames(result) <- result_column_names
        }
    }
    return(result)

}

brown_warner_1980 <- function(list_of_returns, event_start, event_end) {
    # check event_start and event_end for class and value validity
    if(!inherits(event_start, "Date")) {
        stop("event_start must be an object of class Date.")
    }
    if(!inherits(event_end, "Date")) {
        stop("event_end must be an object of class Date.")
    }
    if(event_start > event_end) {
        stop("event_start must be earlier than event_end.")
    }


    # zoo objects of abnormal returns
    estimation_abnormal <- zoo()
    event_abnormal <- zoo()
    delta <- numeric(length(list_of_returns))

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.cahrachter(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_estimation_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >=
                list_of_returns[[i]]$estimation_start &
                time(list_of_returns[[i]]$abnormal) <=
                list_of_returns[[i]]$estimation_end]
        company_event_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >= event_start &
                time(list_of_returns[[i]]$abnormal) <= event_end]

        estimation_abnormal <- merge(company_estimation_abnormal,
                                     estimation_abnormal, all = TRUE)
        event_abnormal <- merge(company_event_abnormal, event_abnormal,
                                all = TRUE)

        delta[i] <- list_of_returns[[i]]$estimation_length
    }

    result <- data.frame(date = time(event_abnormal),
                         weekday = weekdays(time(event_abnormal)),
                         percentage = apply(!is.na(as.matrix(event_abnormal)),
                                        1, sum) / ncol(event_abnormal) * 100,
                         mean = apply(event_abnormal, 1, mean, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)

    mean_delta <- mean(delta)

    sd_estimation_period <- sqrt(sum(apply(estimation_abnormal, 2, var,
                                           na.rm = T))) /
        ncol(estimation_abnormal)
    statistics <- apply(event_abnormal, 1, mean, na.rm = T) /
        sd_estimation_period
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= qt(1 - 0.10/2, mean_delta)] <- "*"
    significance[abs(statistics) >= qt(1 - 0.05/2, mean_delta)] <- "**"
    significance[abs(statistics) >= qt(1 - 0.01/2, mean_delta)] <- "***"
    result <- cbind(result, data.frame(statistics = statistics,
                                       significance = significance))
    return(result)
}




brown_warner_1985 <- function(list_of_returns, event_start, event_end) {
    # check event_start and event_end for class and value validity
    if(!inherits(event_start, "Date")) {
        stop("event_start must be an object of class Date.")
    }
    if(!inherits(event_end, "Date")) {
        stop("event_end must be an object of class Date.")
    }
    if(event_start > event_end) {
        stop("event_start must be earlier than event_end.")
    }


    # zoo objects of abnormal returns
    estimation_abnormal <- zoo()
    event_abnormal <- zoo()
    delta <- numeric(length(list_of_returns))

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.cahrachter(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_estimation_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >=
                list_of_returns[[i]]$estimation_start &
                time(list_of_returns[[i]]$abnormal) <=
                list_of_returns[[i]]$estimation_end]
        company_event_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >= event_start &
                time(list_of_returns[[i]]$abnormal) <= event_end]

        estimation_abnormal <- merge(company_estimation_abnormal,
                                     estimation_abnormal, all = TRUE)
        event_abnormal <- merge(company_event_abnormal, event_abnormal,
                                all = TRUE)

        delta[i] <- list_of_returns[[i]]$estimation_length
    }

    result <- data.frame(date = time(event_abnormal),
                         weekday = weekdays(time(event_abnormal)),
                         percentage = apply(!is.na(as.matrix(event_abnormal)),
                         1, sum) / ncol(event_abnormal) * 100,
                         mean = apply(event_abnormal, 1, mean, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)

    mean_delta <- mean(delta)

    sd_estimation_period <- sqrt(var(apply(estimation_abnormal, 1, mean,
                                           na.rm = T)))
    statistics <- apply(event_abnormal, 1, mean, na.rm = T) /
        sd_estimation_period
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= qt(1 - 0.10/2, mean_delta)] <- "*"
    significance[abs(statistics) >= qt(1 - 0.05/2, mean_delta)] <- "**"
    significance[abs(statistics) >= qt(1 - 0.01/2, mean_delta)] <- "***"
    result <- cbind(result, data.frame(statistics = statistics,
                                       significance = significance))
    return(result)
}

t_test <- function(list_of_returns, event_start, event_end) {
    # check event_start and event_end for class and value validity
    if(!inherits(event_start, "Date")) {
        stop("event_start must be an object of class Date.")
    }
    if(!inherits(event_end, "Date")) {
        stop("event_end must be an object of class Date.")
    }
    if(event_start > event_end) {
        stop("event_start must be earlier than event_end.")
    }


    # zoo objects of abnormal returns
    event_abnormal <- zoo()

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.cahrachter(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_event_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >= event_start &
                time(list_of_returns[[i]]$abnormal) <= event_end]
        event_abnormal <- merge(company_event_abnormal, event_abnormal,
                                all = TRUE)
    }

    event_number_of_companies <- apply(!is.na(event_abnormal), 1, sum)

    result <- data.frame(date = time(event_abnormal),
                         weekday = weekdays(time(event_abnormal)),
                         percentage = event_number_of_companies /
                             ncol(event_abnormal) * 100,
                         mean = apply(event_abnormal, 1, mean, na.rm = T))

    event_abnormal <- as.matrix(event_abnormal)


    # browser()
    statistics <- apply(event_abnormal, 1, mean, na.rm = T) /
                  apply(event_abnormal, 1, sd, na.rm = T) *
                  sqrt(event_number_of_companies)


    significance <- rep("", length(statistics))
    significance[abs(statistics) >=
                     qt(1 - 0.10/2, event_number_of_companies)] <- "*"
    significance[abs(statistics) >=
                     qt(1 - 0.05/2, event_number_of_companies)] <- "**"
    significance[abs(statistics) >=
                     qt(1 - 0.01/2, event_number_of_companies)] <- "***"
    result <- cbind(result, data.frame(statistics = statistics,
                                       significance = significance))
    return(result)

}

patell <- function(list_of_returns, event_start, event_end) {
    # check event_start and event_end for class and value validity
    if(!inherits(event_start, "Date")) {
        stop("event_start must be an object of class Date.")
    }
    if(!inherits(event_end, "Date")) {
        stop("event_end must be an object of class Date.")
    }
    if(event_start > event_end) {
        stop("event_start must be earlier than event_end.")
    }


    # zoo objects of abnormal returns
    estimation_abnormal <- zoo()
    event_abnormal <- zoo()
    event_standardized_abnormal <- zoo()
    delta <- numeric(length(list_of_returns))

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.cahrachter(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_estimation_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >=
            list_of_returns[[i]]$estimation_start &
            time(list_of_returns[[i]]$abnormal) <=
            list_of_returns[[i]]$estimation_end]
        company_event_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >= event_start &
            time(list_of_returns[[i]]$abnormal) <= event_end]

        estimation_abnormal <- merge(company_estimation_abnormal,
                                     estimation_abnormal, all = TRUE)
        event_abnormal <- merge(company_event_abnormal, event_abnormal,
                                all = TRUE)


        market_event <- list_of_returns[[i]]$regressor[
            time(list_of_returns[[i]]$regressor) >= event_start &
            time(list_of_returns[[i]]$regressor) <= event_end]
        market_estimation <- list_of_returns[[i]]$regressor[
            time(list_of_returns[[i]]$regressor) >=
            list_of_returns[[i]]$estimation_start &
            time(list_of_returns[[i]]$regressor) <=
            list_of_returns[[i]]$estimation_end]
        mean_market_estimation <- mean(market_estimation)

        company_event_standardized <- company_event_abnormal /
            sd(company_estimation_abnormal) /
            sqrt(1 + 1 / list_of_returns[[i]]$estimation_length +
                 (market_event - mean_market_estimation) ^ 2 /
                 sum((market_estimation - mean_market_estimation) ^ 2))
        event_standardized_abnormal <- merge(company_event_standardized,
                                             event_standardized_abnormal,
                                             all = TRUE)
        delta[i] <- list_of_returns[[i]]$estimation_length
        #browser()
    }
    #browser()
    result <- data.frame(date = time(event_abnormal),
                         weekday = weekdays(time(event_abnormal)),
                         percentage = apply(!is.na(as.matrix(event_abnormal)),
                                        1, sum) / ncol(event_abnormal) * 100,
                         mean = apply(event_abnormal, 1, mean, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    event_standardized_abnormal <- as.matrix(event_standardized_abnormal)

    statistics <- apply(event_standardized_abnormal, 1, sum, na.rm = TRUE) /
        sqrt(sum((delta - 2) / (delta - 4)))
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= k_q1] <- "*"
    significance[abs(statistics) >= k_q2] <- "**"
    significance[abs(statistics) >= k_q3] <- "***"
    result <- cbind(result, data.frame(statistics = statistics,
                                       significance = significance))
    return(result)
}

boehmer <- function(list_of_returns, event_start, event_end) {
    # check event_start and event_end for class and value validity
    if(!inherits(event_start, "Date")) {
        stop("event_start must be an object of class Date.")
    }
    if(!inherits(event_end, "Date")) {
        stop("event_end must be an object of class Date.")
    }
    if(event_start > event_end) {
        stop("event_start must be earlier than event_end.")
    }


    # zoo objects of abnormal returns
    estimation_abnormal <- zoo()
    event_abnormal <- zoo()
    event_standardized_abnormal <- zoo()
    # delta <- numeric(length(list_of_returns))

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.cahrachter(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_estimation_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >=
                list_of_returns[[i]]$estimation_start &
                time(list_of_returns[[i]]$abnormal) <=
                list_of_returns[[i]]$estimation_end]
        company_event_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >= event_start &
                time(list_of_returns[[i]]$abnormal) <= event_end]

        estimation_abnormal <- merge(company_estimation_abnormal,
                                     estimation_abnormal, all = TRUE)
        event_abnormal <- merge(company_event_abnormal, event_abnormal,
                                all = TRUE)


        market_event <- list_of_returns[[i]]$regressor[
            time(list_of_returns[[i]]$regressor) >= event_start &
                time(list_of_returns[[i]]$regressor) <= event_end]
        market_estimation <- list_of_returns[[i]]$regressor[
            time(list_of_returns[[i]]$regressor) >=
                list_of_returns[[i]]$estimation_start &
                time(list_of_returns[[i]]$regressor) <=
                list_of_returns[[i]]$estimation_end]
        mean_market_estimation <- mean(market_estimation)

        company_event_standardized <- company_event_abnormal /
            sd(company_estimation_abnormal) /
            sqrt(1 + 1 / list_of_returns[[i]]$estimation_length +
                     (market_event - mean_market_estimation) ^ 2 /
                     sum((market_estimation - mean_market_estimation) ^ 2))
        event_standardized_abnormal <- merge(company_event_standardized,
                                             event_standardized_abnormal,
                                             all = TRUE)
        # delta[i] <- list_of_returns[[i]]$estimation_length
        #browser()
    }

    event_number_of_companies <- apply(!is.na(event_abnormal), 1, sum)
    #browser()
    result <- data.frame(date = time(event_abnormal),
                         weekday = weekdays(time(event_abnormal)),
                         percentage = apply(!is.na(as.matrix(event_abnormal)),
                                        1, sum) / ncol(event_abnormal) * 100,
                         mean = apply(event_abnormal, 1, mean, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    event_standardized_abnormal <- as.matrix(event_standardized_abnormal)

    statistics <- apply(event_standardized_abnormal, 1, mean, na.rm = T) /
                  apply(event_standardized_abnormal, 1, sd, na.rm = T) *
                  sqrt(event_number_of_companies)
    significance <- rep("", length(statistics))
    significance[abs(statistics) >=
                     qt(1 - 0.10/2, event_number_of_companies)] <- "*"
    significance[abs(statistics) >=
                     qt(1 - 0.05/2, event_number_of_companies)] <- "**"
    significance[abs(statistics) >=
                     qt(1 - 0.01/2, event_number_of_companies)] <- "***"
    result <- cbind(result, data.frame(statistics = statistics,
                                       significance = significance))
    return(result)
}

