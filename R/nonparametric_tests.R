nonparametric_tests <- function(list_of_returns, event_start, event_end, all = T,
                             tests) {
    if(all == T) {
        tests <- list(sign_test, generalized_sign_test, rank_test,
                      generalized_rank_test)
    }
    result <- NULL
    for(test in tests) {
        if(is.null(result)) {
            result <- test(list_of_returns, event_start, event_end)
        } else {
            result <- merge(x = result, y = test(list_of_returns, event_start,
                                                 event_end)[, c(1, 5, 6)],
                            by = "date", all = T)
        }
    }
    return(result)
}


#' @export
corrado_sign_test <- function(list_of_returns, event_start, event_end) {
    # Corrado Zivney 1992
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


    # zoo objects of signs
    event_sign <- NULL
    full_sign <- NULL
    delta_full <- numeric(length(list_of_returns))
    browser()
    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            stop(paste0("For ", as.character(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_full_abnormal <- c(
            list_of_returns[[i]]$abnormal[
                time(list_of_returns[[i]]$abnormal) >=
                list_of_returns[[i]]$estimation_start &
                time(list_of_returns[[i]]$abnormal) <=
                list_of_returns[[i]]$estimation_end],
            list_of_returns[[i]]$abnormal[
                time(list_of_returns[[i]]$abnormal) >= event_start &
                time(list_of_returns[[i]]$abnormal) <= event_end])

        company_event_abnormal <- list_of_returns[[i]]$abnormal[
            time(list_of_returns[[i]]$abnormal) >= event_start &
                time(list_of_returns[[i]]$abnormal) <= event_end]

        company_median <- median(zoo::coredata(company_full_abnormal))
        company_full_sign <- sign(company_full_abnormal - company_median)
        company_event_sign <- sign(company_event_abnormal - company_median)

        if(is.null(full_sign)){
            full_sign <- company_full_sign
        } else {
            full_sign <- merge(full_sign, company_full_sign, all = T)
        }

        if(is.null(event_sign)){
            event_sign <- company_event_sign
        } else {
            event_sign <- merge(event_sign, company_event_sign, all = T)
        }

        delta_full[i] <-
            length(company_full_abnormal[!is.na(company_full_abnormal)])

    }

    result <- data.frame(date = time(event_sign),
                         weekday = weekdays(time(event_sign)),
                         percentage = rowSums(!is.na(as.matrix(event_sign)),
                                              na.rm = T) /
                             ncol(event_sign) * 100)

    full_sign <- as.matrix(full_sign)
    event_sign <- as.matrix(event_sign)
    number_of_companies <- rowSums(!is.na(full_sign), na.rm = T)
    sd_full <- sqrt(1 / mean(delta_full) *
            sum((1 / sqrt(number_of_companies) * rowSums(full_sign, na.rm = T))^2))


    statistics <- 1 / sqrt(rowSums(!is.na(event_sign), na.rm = T)) *
        rowSums(event_sign, na.rm = T) / sd_full

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= k_q1] <- "*"
    significance[abs(statistics) >= k_q2] <- "**"
    significance[abs(statistics) >= k_q3] <- "***"
    result <- cbind(result, data.frame(cs_stat = statistics,
                                       cs_signif = significance))
    return(result)
}


#' @export
binomial_sign_test <- function(list_of_returns, event_start, event_end) {
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
    estimation_binary <- NULL
    event_binary <- NULL
    browser()
    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.character(i), "-th company estimation",
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

        company_estimation_binary <- zoo::zoo(
            as.numeric(company_estimation_abnormal > 0),
            time(company_estimation_abnormal))

        company_event_binary <- zoo::zoo(as.numeric(company_event_abnormal > 0),
                                         time(company_event_abnormal))

        if(is.null(estimation_binary)){
            estimation_binary <- company_estimation_binary
        } else {
            estimation_binary <- merge(estimation_binary,
                                       company_estimation_binary, all = T)
        }
        if(is.null(event_binary)){
            event_binary <- company_event_binary
        } else {
            event_binary <- merge(event_binary, company_event_binary,
                                    all = T)
        }
    }

    p_hat <- mean(as.matrix(estimation_binary), na.rm = F)

    result <- data.frame(date = time(event_binary),
                         weekday = weekdays(time(event_binary)),
                         percentage = rowSums(!is.na(as.matrix(event_binary)),
                                              na.rm = T) /
                             ncol(event_binary) * 100)

    estimation_binary <- as.matrix(estimation_binary)
    event_binary <- as.matrix(event_binary)

    statistics <- (rowSums(event_binary, na.rm = T) -
                       rowSums(!is.na(event_binary), na.rm = T) * p_hat) /
        sqrt(rowSums(!is.na(event_binary), na.rm = T) * p_hat * (1-p_hat))

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= k_q1] <- "*"
    significance[abs(statistics) >= k_q2] <- "**"
    significance[abs(statistics) >= k_q3] <- "***"
    result <- cbind(result, data.frame(bs_stat = statistics,
                                       bs_signif = significance))
    return(result)
}
