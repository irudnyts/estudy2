nonparametric_tests <- function(list_of_returns, event_start, event_end, all = T,
                             tests) {
    if(all == T) {
        tests <- list(corrado_sign_test, binomial_sign_test, rank_test,
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


#' Corrado's sign test (1992).
#'
#' The implementation of nonparametric test, described in Corrado and Zivney's
#' 1992 paper.
#'
#' Performs the nonparametric test for event study, which is descibed in Corrado
#' and Zivney's 1992 paper. This test is similar to procedure, described in
#' Brown and Warner's paper 1985 (t-ratio), but instead of using abnormal
#' returns, the test uses \eqn{G_{i,t} = sign(A_{i,t} - median(A_i))}.
#' \code{sign} and \code{median} are ones, which have the same definition as R
#' functions. For this test the estimation period and event period must not
#' overlap, otherwise an error occurrs. The sign test procedure avoids the
#' misspecification of tests, which assume symmetry around zero of abnormal
#' returns (the median equals to zero). For a single day the performance of this
#' test is proven to be better than classical Brown and Warner's test (without
#' event-induced variance). This test is dominated by rank test. The
#' significance levels of \eqn{\alpha} are 0.1, 0.05, and 0.01 (marked
#' respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each elemnt
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references Corrado C.J., Zivney T.L. \emph{The Specification and Power of
#' the Sign Test in Event Study Hypothesis Tests Using Daily Stock Returns}.
#' Journal of Financial and Quantitative Analysis, 27(3):465-478, 1992.
#'
#' @seealso \code{\link{nonparametric_tests}}, \code{\link{binomial_sign_test}},
#' \code{\link{rank_test}}, and \code{\link{modified_rank_test}}
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

        if(is.null(full_sign)) {
            full_sign <- company_full_sign
        } else {
            full_sign <- merge(full_sign, company_full_sign, all = T)
        }

        if(is.null(event_sign)) {
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
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(cs_stat = statistics,
                                       cs_signif = significance))
    return(result)
}


#' @export
binomial_sign_test <- function(list_of_returns, event_start, event_end) {
    # Cowan 1992
    # Boehmer 1991
    # McConnell Muscarella 1985 or 1989
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
    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            stop(paste0("For ", as.character(i), "-th company estimation",
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

        if(is.null(estimation_binary)) {
            estimation_binary <- company_estimation_binary
        } else {
            estimation_binary <- merge(estimation_binary,
                                       company_estimation_binary, all = T)
        }
        if(is.null(event_binary)) {
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
        sqrt(rowSums(!is.na(event_binary), na.rm = T) * p_hat * (1 - p_hat))

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(bs_stat = statistics,
                                       bs_signif = significance))
    return(result)
}

#' @export
rank_test <- function(list_of_returns, event_start, event_end) {
    # Corrado 1989
    # Cowan 1992
    # Campbell Wasley 1992
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
    browser()
    full_rank <- NULL
    event_rank <- NULL
    delta_full <- numeric(length(list_of_returns))
    avg_rank <- numeric(length(list_of_returns))
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

        company_full_rank <- zoo::zoo(rank(x = coredata(company_full_abnormal),
                                           na.last = "keep",
                                           ties.method = "average"),
                                      time(company_full_abnormal))
        company_event_rank <- company_full_rank[time(company_full_rank) >= event_start & time(company_full_rank) <= event_end]

        if(is.null(full_rank)) {
            full_rank <- company_full_rank
        } else {
            full_rank <- merge(full_rank, company_full_rank, all = T)
        }

        if(is.null(event_rank)) {
            event_rank <- company_event_rank
        } else {
            event_rank <- merge(event_rank, company_event_rank, all = T)
        }

        delta_full[i] <-
            length(company_full_abnormal[!is.na(company_full_abnormal)])
        avg_rank[i] <- mean(company_full_rank)
    }


    result <- data.frame(date = time(event_rank),
                         weekday = weekdays(time(event_rank)),
                         percentage = rowSums(!is.na(as.matrix(event_rank)),
                                              na.rm = T) /
                             ncol(event_rank) * 100)

    full_rank <- as.matrix(full_rank)
    event_rank <- as.matrix(event_rank)
    number_of_companies <- rowSums(!is.na(full_rank), na.rm = T)
    avg_rank_full <- matrix(rep(avg_rank, nrow(full_rank)),
                              nrow = nrow(full_rank), ncol = ncol(full_rank),
                              byrow = T)
    avg_rank_event <- matrix(rep(avg_rank, nrow(event_rank)),
                             nrow = nrow(event_rank), ncol = ncol(event_rank),
                             byrow = T)

    sd_full <- sqrt(1 / mean(delta_full) * sum((1 / number_of_companies * rowSums(full_rank - avg_rank_full, na.rm = T))^2))


    statistics <- 1 / rowSums(!is.na(as.matrix(event_rank)), na.rm = T) * rowSums(event_rank - avg_rank_event, na.rm = T) / sd_full

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(rk_stat = statistics,
                                       rk_signif = significance))
    return(result)
}



#' @export
modified_rank_test <- function(list_of_returns, event_start, event_end) {
    # Corrado Zivney 1992
    # Kolari Pynnonen 2010

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
    browser()
    full_rank_modif <- NULL
    event_rank_modif <- NULL
    delta_full <- numeric(length(list_of_returns))
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

        company_full_rank_modif <- zoo::zoo(rank(x = coredata(company_full_abnormal),
                                                 na.last = "keep",
                                                 ties.method = "average") /
                                                (1 + sum(!is.na(company_full_abnormal))),
                                            time(company_full_abnormal))
        company_event_rank_modif <- company_full_rank_modif[time(company_full_rank_modif) >= event_start & time(company_full_rank_modif) <= event_end]

        if(is.null(full_rank_modif)) {
            full_rank_modif <- company_full_rank_modif
        } else {
            full_rank_modif <- merge(full_rank_modif, company_full_rank_modif, all = T)
        }

        if(is.null(event_rank_modif)) {
            event_rank_modif <- company_event_rank_modif
        } else {
            event_rank_modif <- merge(event_rank_modif, company_event_rank_modif, all = T)
        }

        delta_full[i] <-
            length(company_full_abnormal[!is.na(company_full_abnormal)])
    }


    result <- data.frame(date = time(event_rank_modif),
                         weekday = weekdays(time(event_rank_modif)),
                         percentage = rowSums(!is.na(as.matrix(event_rank_modif)),
                                              na.rm = T) /
                             ncol(event_rank_modif) * 100)

    full_rank_modif <- as.matrix(full_rank_modif)
    event_rank_modif <- as.matrix(event_rank_modif)
    number_of_companies <- rowSums(!is.na(full_rank_modif), na.rm = T)

    sd_full <- sqrt(1 / mean(delta_full) * sum((1 / sqrt(number_of_companies) * rowSums(full_rank_modif - 0.5, na.rm = T))^2))


    statistics <- 1 / sqrt(rowSums(!is.na(as.matrix(event_rank_modif)), na.rm = T)) * rowSums(event_rank_modif - 0.5, na.rm = T) / sd_full

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(rm_stat = statistics,
                                       rm_signif = significance))
    return(result)
}
