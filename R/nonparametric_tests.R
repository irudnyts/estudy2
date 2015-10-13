#' Returns the result for all nonparametric tests for event study.
#'
#' Performs main nonparametric tests for each date in the event window and
#' returns the table of statistics and significance of these tests.
#'
#' \code{nonparametric_tests} performs given tests among \code{\link{sign_test}},
#' \code{\link{generalized_sign_test}}, \code{\link{corrado_sign_test}},
#' \code{\link{rank_test}}, \code{\link{modified_rank_test}},
#' \code{\link{generalized_rank_test}}, \code{\link{wilcoxon_text}}, and merge
#' result to single table. If \code{all = T} (by default), the function ignores
#' the value of \code{tests}.
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each elemnt
#' of which is treated as a company (security).
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @param all a logical value indicating whether all tests should be performed.
#' The default value is TRUE.
#' @param tests the list of tests functions among \code{corrado_sign_test},
#' \code{binomial_sign_test}, \code{rank_test}, and \code{modified_rank_test}.
#' @return The single table of statistics and significances of all tests.
#'
#' @references \itemize{
#' \item Corrado C.J., Zivney T.L. \emph{The Specification and Power of
#' the Sign Test in Event Study Hypothesis Tests Using Daily Stock Returns}.
#' Journal of Financial and Quantitative Analysis, 27(3):465-478, 1992.
#' \item McConnell J.J., Muscarella C.J. \emph{Capital expenditure plans and
#' firm value} Journal of Financial Economics, 14:399-422, 1985.
#' \item Boehmer E., Musumeci J., Poulsen A.B. \emph{Event-study methodology
#' under conditions of event-induced variance}. Journal of Financial Economics,
#' 30(2):253-272, 1991.
#' \item Cowan A.R. \emph{Nonparametric Event Study Tests}. Review of
#' Quantitative Finance and Accounting, 2:343-358, 1992.
#' \item Corrado C.J. \emph{A Nonparametric Test аor Abnormal Security-Price
#' Performance in Event Studeis}. Journal of Financial Economics 23:385-395,
#' 1989.
#' \item Campbell C.J., Wasley C.E. \emph{Measuring Security Price Performance
#' Using Daily NASDAQ Retrurns}. Journal of Financial Economics 33:73-92, 1993.
#' \item Savickas R. \emph{Event-Induced Volatility and Tests for Abnormal
#' Performance}. The Journal of Financial Research, 26(2):156-178, 2003.
#' \item Kolari J.W., Pynnonen S. \emph{Event Study Testing with Cross-sectional
#' Correlation of Abnormal Returns}. The Review of Financial Studies,
#' 23(11):3996-4025, 2010.
#' \item Wilcoxon F. \emph{Individual Comparisons by Ranking Mathods}.
#' Biometrics Bulletin 1(6):80-83, 1945
#' \item Lehmann E.L, \emph{Nonparametrics: Statistical Methods Based on Ranks},
#' San Francisco: Holden-Day, 1975.
#' \item Hollander M., Wolfe D.A. \emph{Nonparametric Statistical Methods},
#' New York: John Wiley & Sons, 1973.
#' }
#'
#' @seealso \code{\link{sign_test}}, \code{\link{generalized_sign_test}},
#' \code{\link{corrado_sign_test}}, \code{\link{rank_test}},
#' \code{\link{modified_rank_test}}, \code{\link{generalized_rank_test}}, and
#' \code{\link{wilcoxon_text}}.
#' @export
nonparametric_tests <- function(list_of_returns, event_start, event_end,
                                all = T, tests) {
    if(all == T) {
        tests <- list(sign_test, generalized_sign_test, corrado_sign_test,
                      rank_test, modified_rank_test)
    }
    result <- NULL
    for(test in tests) {
        if(is.null(result)) {
            result <- test(list_of_returns, event_start, event_end)
        } else {
            result <- merge(x = result, y = test(list_of_returns, event_start,
                                                 event_end)[, c(1, 4, 5)],
                            by = "date", all = T)
        }
    }
    return(result)
}

#' Simple binomial sign test for event study.
#'
#' The binomial sign test which determines whether the frequency of positive
#' abnormal returns in the event period is significantly different from one-half.
#'
#' This test is application of the simple binomial test to the event study,
#' which indicates whether the cross-sectional frequency of positive abnormal
#' returns is significantly different from 0.5. This test is stable
#' to outliers, in other words allows to check if the result is driven by few
#' companies with extremly large abnomal performance. For this test the
#' estimation period and event period must not overlap, otherwise an error
#' occurrs. The test statistic is assumed to have a normal distribution in
#' approximation under null hypothesis, if the number of securities is large.
#' Typically is used together with parametric tests. The test is well-specified
#' for the case, when cross-sectional abnoral returns are not symmetric.
#' Also this procedure is less sensative to extreme returns than the rank test.
#' The significance levels of \eqn{\alpha} are 0.1, 0.05, and 0.01
#' (marked respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each elemnt
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references Boehmer E., Musumeci J., Poulsen A.B. \emph{Event-study
#' methodology under conditions of event-induced variance}. Journal of Financial
#' Economics, 30(2):253-272, 1991.
#'
#' @seealso \code{\link{nonparametric_tests}}, \code{\link{generalized_sign_test}},
#' \code{\link{corrado_sign_test}}, \code{\link{rank_test}},
#' \code{\link{modified_rank_test}}, \code{\link{generalized_rank_test}}, and
#' \code{\link{wilcoxon_test}}.
#' @export
sign_test <- function(list_of_returns, event_start, event_end) {
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

        company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                zoo::index(list_of_returns[[i]]$abnormal) <= event_end])

        company_event_binary <- zoo::zoo(as.numeric(company_event_abnormal > 0),
                                         zoo::index(company_event_abnormal))

        if(is.null(event_binary)) {
            event_binary <- company_event_binary
        } else {
            event_binary <- merge(event_binary, company_event_binary,
                                  all = T)
        }
    }

    result <- data.frame(date = zoo::index(event_binary),
                         weekday = weekdays(zoo::index(event_binary)),
                         percentage = rowSums(!is.na(as.matrix(event_binary)),
                                              na.rm = T) /
                             ncol(event_binary) * 100)

    event_binary <- as.matrix(event_binary)

    statistics <- (rowSums(event_binary, na.rm = T) -
                       rowSums(!is.na(event_binary), na.rm = T) * 0.5) /
        sqrt(rowSums(!is.na(event_binary), na.rm = T) * 0.25)

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(sign_stat = statistics,
                                       sign_signif = significance))
    return(result)
}

#' Binomial sign test for event study.
#'
#' The binomial sign test which determines whether the frequency of positive
#' abnormal returns in the event period is significantly different from one in
#' the estimation period.
#'
#' This test is application of the simple binomial test to the event study,
#' which indicates whether the cross-sectional frequency of positive abnormal
#' returns is significantly different from the expected. This test is stable
#' to outliers, in other words allows to check if the result is driven by few
#' companies with extremly large abnomal performance. For this test the
#' estimation period and event period must not overlap, otherwise an error
#' occurrs. This test instead of using naive value of expected frequency 0.5
#' uses an estimate from the estimation period. The test statistic is assumed to
#' have a normal distribution. Typically is used together with parametric tests.
#' The test is well-specified for the case, when cross-sectional abnoral returns
#' are not symmetric. Also this procedure is less sensative to extreme returns
#' than the rank test. The significance levels of \eqn{\alpha} are 0.1, 0.05,
#' and 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each elemnt
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references \itemize{
#' \item McConnell J.J., Muscarella C.J. \emph{Capital expenditure plans and
#' firm value} Journal of Financial Economics, 14:399-422, 1985.
#' \item Cowan A.R. \emph{Nonparametric Event Study Tests}. Review of
#' Quantitative Finance and Accounting, 2:343-358, 1992.
#' }
#'
#' @seealso \code{\link{nonparametric_tests}}, \code{\link{sign_test}},
#' \code{\link{corrado_sign_test}}, \code{\link{rank_test}},
#' \code{\link{modified_rank_test}}, \code{\link{generalized_rank_test}}, and
#' \code{\link{wilcoxon_test}}.
#' @export
generalized_sign_test <- function(list_of_returns, event_start, event_end) {
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

        company_estimation_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >=
                list_of_returns[[i]]$estimation_start &
                zoo::index(list_of_returns[[i]]$abnormal) <=
                list_of_returns[[i]]$estimation_end])
        company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                zoo::index(list_of_returns[[i]]$abnormal) <= event_end])

        company_estimation_binary <- zoo::zoo(
            as.numeric(company_estimation_abnormal > 0),
            zoo::index(company_estimation_abnormal))

        company_event_binary <- zoo::zoo(as.numeric(company_event_abnormal > 0),
                                         zoo::index(company_event_abnormal))

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

    result <- data.frame(date = zoo::index(event_binary),
                         weekday = weekdays(zoo::index(event_binary)),
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
    result <- cbind(result, data.frame(gsign_stat = statistics,
                                       gsign_signif = significance))
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
#' @seealso \code{\link{nonparametric_tests}}, \code{\link{sign_test}},
#' \code{\link{generalized_sign_test}}, \code{\link{rank_test}},
#' \code{\link{modified_rank_test}}, \code{\link{generalized_rank_test}}, and
#' \code{\link{wilcoxon_test}}.
#' @export
corrado_sign_test <- function(list_of_returns, event_start, event_end) {
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
        company_full_abnormal <- zoo::as.zoo(c(
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >=
                    list_of_returns[[i]]$estimation_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <=
                    list_of_returns[[i]]$estimation_end],
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <= event_end]))

        company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                zoo::index(list_of_returns[[i]]$abnormal) <= event_end])

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

    result <- data.frame(date = zoo::index(event_sign),
                         weekday = weekdays(zoo::index(event_sign)),
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
    result <- cbind(result, data.frame(csign_stat = statistics,
                                       csign_signif = significance))
    return(result)
}


#' Rank test for event study.
#'
#' The original rank test for event study, which is based on Wilcoxon (1945)
#' rank test.
#'
#' This procedure uses ranks of abnormal returns to examine significance of each
#' day in event window. In order to get ranks of correspoding abnormal returns,
#' the procedure uses regular R function \code{\link{rank}} with parameter
#' \code{ties.method = "average"} and \code{na.last = "keep"}. For this test the
#' estimation period and event period must not overlap, otherwise an error
#' occurrs. The test statistic is assumed to have a normal distribution (as an
#' approximation). The test is well-specified for the case, when cross-sectional
#' abnoral returns are not symmetric. The test is stable to variance increase
#' during event window. This test is more sensative to extreme values than sign
#' test. For data with missed data see the \code{\link{modified_rank_test}}. The
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
#' @references \itemize{
#' \item Corrado C.J. \emph{A Nonparametric Test аor Abnormal Security-Price
#' Performance in Event Studeis}. Journal of Financial Economics 23:385-395,
#' 1989.
#' \item Cowan A.R. \emph{Nonparametric Event Study Tests}. Review of
#' Quantitative Finance and Accounting, 2:343-358, 1992.
#' \item Campbell C.J., Wasley C.E. \emph{Measuring Security Price Performance
#' Using Daily NASDAQ Retrurns}. Journal of Financial Economics 33:73-92, 1993.
#' \item Savickas R. \emph{Event-Induced Volatility and Tests for Abnormal
#' Performance}. The Journal of Financial Research, 26(2):156-178, 2003.
#' }
#'
#' @seealso \code{\link{nonparametric_tests}},\code{\link{sign_test}},
#' \code{\link{generalized_sign_test}}, \code{\link{corrado_sign_test}},
#' \code{\link{modified_rank_test}}, \code{\link{generalized_rank_test}}, and
#' \code{\link{wilcoxon_test}}.
#' @export
rank_test <- function(list_of_returns, event_start, event_end) {

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

        company_full_abnormal <- zoo::as.zoo(c(
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >=
                    list_of_returns[[i]]$estimation_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <=
                    list_of_returns[[i]]$estimation_end],
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                zoo::index(list_of_returns[[i]]$abnormal) <= event_end]))

        company_full_rank <- zoo::zoo(rank(x = zoo::coredata(company_full_abnormal),
                                           na.last = "keep",
                                           ties.method = "average"),
                                      zoo::index(company_full_abnormal))
        company_event_rank <- zoo::as.zoo(company_full_rank[
            zoo::index(company_full_rank) >= event_start &
            zoo::index(company_full_rank) <= event_end])

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


    result <- data.frame(date = zoo::index(event_rank),
                         weekday = weekdays(zoo::index(event_rank)),
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

    sd_full <- sqrt(1 / mean(delta_full) * sum((1 / number_of_companies *
                            rowSums(full_rank - avg_rank_full, na.rm = T))^2))


    statistics <- 1 / rowSums(!is.na(as.matrix(event_rank)), na.rm = T) *
        rowSums(event_rank - avg_rank_event, na.rm = T) / sd_full

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(rank_stat = statistics,
                                       rank_signif = significance))
    return(result)
}



#' Modified rank test for event study.
#'
#' This test, the modification of the original rank test, proposed by Corrado
#' (1989), is abapted to missing values in abnormal returns.
#'
#' In addition to original rank test, the procedure devides corresponding ranks
#' on number of nonmissing returns plus one for each security. This leads to
#' order statistics with uniform destribution. In limit overall statistics under
#' null hypothesis is aproximately noramly distributed. For this test the
#' estimation period and event period must not overlap, otherwise an error
#' occurrs. The test is well-specified for the case, when cross-sectional
#' abnoral returns are not symmetric. The test is stable to variance increase
#' during event window. This test is more sensative to extreme values than sign
#' test. The significance levels of \eqn{\alpha} are 0.1, 0.05, and 0.01 (marked
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
#' @references \itemize{
#' \item Corrado C.J., Zivney T.L. \emph{The Specification and Power of
#' the Sign Test in Event Study Hypothesis Tests Using Daily Stock Returns}.
#' Journal of Financial and Quantitative Analysis, 27(3):465-478, 1992.
#' \item Kolari J.W., Pynnonen S. \emph{Event Study Testing with Cross-sectional
#' Correlation of Abnormal Returns}. The Review of Financial Studies,
#' 23(11):3996-4025, 2010.
#' }
#'
#' @seealso \code{\link{nonparametric_tests}},\code{\link{sign_test}},
#' \code{\link{generalized_sign_test}}, \code{\link{corrado_sign_test}},
#' \code{\link{rank_test}}, \code{\link{generalized_rank_test}}, and
#' \code{\link{wilcoxon_test}}.
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

        company_full_abnormal <- zoo::as.zoo(c(
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >=
                    list_of_returns[[i]]$estimation_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <=
                    list_of_returns[[i]]$estimation_end],
            list_of_returns[[i]]$abnormal[
                zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                    zoo::index(list_of_returns[[i]]$abnormal) <= event_end]))

        company_full_rank_modif <- zoo::zoo(rank(x =
                                                zoo::coredata(company_full_abnormal),
                                                na.last = "keep",
                                                ties.method = "average") /
                                    (1 + sum(!is.na(company_full_abnormal))),
                                            zoo::index(company_full_abnormal))
        company_event_rank_modif <- zoo::as.zoo(company_full_rank_modif[
            zoo::index(company_full_rank_modif) >= event_start &
                zoo::index(company_full_rank_modif) <= event_end])

        if(is.null(full_rank_modif)) {
            full_rank_modif <- company_full_rank_modif
        } else {
            full_rank_modif <- merge(full_rank_modif, company_full_rank_modif,
                                     all = T)
        }

        if(is.null(event_rank_modif)) {
            event_rank_modif <- company_event_rank_modif
        } else {
            event_rank_modif <- merge(event_rank_modif,
                                      company_event_rank_modif, all = T)
        }

        delta_full[i] <-
            length(company_full_abnormal[!is.na(company_full_abnormal)])
    }


    result <- data.frame(date = zoo::index(event_rank_modif),
                         weekday = weekdays(zoo::index(event_rank_modif)),
                         percentage = rowSums(
                             !is.na(as.matrix(event_rank_modif)), na.rm = T) /
                             ncol(event_rank_modif) * 100)

    full_rank_modif <- as.matrix(full_rank_modif)
    event_rank_modif <- as.matrix(event_rank_modif)
    number_of_companies <- rowSums(!is.na(full_rank_modif), na.rm = T)

    sd_full <- sqrt(1 / mean(delta_full) * sum((1 / sqrt(number_of_companies) *
                                rowSums(full_rank_modif - 0.5, na.rm = T))^2))


    statistics <- 1 / sqrt(rowSums(!is.na(as.matrix(event_rank_modif)),
                                   na.rm = T)) *
        rowSums(event_rank_modif - 0.5, na.rm = T) / sd_full

    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(mrank_stat = statistics,
                                       mrank_signif = significance))
    return(result)
}


generalized_rank_test <- function() { }

#' Wilcoxon signed rank test for event study.
#'
#' Performs Wilcoxon test on event period for abnormal returns (the latter are
#' treted as differences).
#'
#' The estimation periods can overlap with event windows, because the procedure
#' takes into account only abnormal returns from event windows. The test has the
#' same algorithm as bulit-in \code{R} \code{\link{wilcox.test}}. The critical
#' values are exact values, which are obtained from \code{\link{qsignrank}}. The
#' algorithm is the following: for each day in event window the cross sectional
#' abnormal returns treated as sample of differences (which actually is true).
#' Firstly the absolute value of these differences are computed, and
#' corresponding ranks of non-zero values are calculated. The test statistic is
#' the sum of ranks, corresponoding to positive abnormal returns. The
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
#' @references \itemize{
#' \item Wilcoxon F. \emph{Individual Comparisons by Ranking Mathods}.
#' Biometrics Bulletin 1(6):80-83, 1945
#' \item Kolari J.W., Pynnonen S. \emph{Event Study Testing with Cross-sectional
#' Correlation of Abnormal Returns}. The Review of Financial Studies,
#' 23(11):3996-4025, 2010.
#' \item Lehmann E.L, \emph{Nonparametrics: Statistical Methods Based on Ranks},
#' San Francisco: Holden-Day, 1975.
#' \item Hollander M., Wolfe D.A. \emph{Nonparametric Statistical Methods},
#' New York: John Wiley & Sons, 1973.
#' }
#'
#' @seealso \code{\link{nonparametric_tests}}, \code{\link{sign_test}},
#' \code{\link{generalized_sign_test}}, \code{\link{corrado_sign_test}},
#' \code{\link{rank_test}}, \code{\link{modified_rank_test}}, and
#' \code{\link{generalized_rank_test}}
#' @export
wilcoxon_test <- function(list_of_returns, event_start, event_end) {
    # event and estimation period could overlap
    # should be applied only for large N
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
    event_abnormal <- NULL

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.character(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
                zoo::index(list_of_returns[[i]]$abnormal) <= event_end])

        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = T)
        }
    }
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = T) /
                             ncol(event_abnormal) * 100)

    browser()
    event_abnormal <- as.matrix(event_abnormal)
    event_abs <- abs(event_abnormal)
    event_abs[event_abs == 0] <- NA
    event_rank <- t(apply(event_abs, 1, rank, na.last = "keep",
                          ties.method = "average"))
    event_rank[event_abnormal < 0] <- 0
    N <- rowSums(!is.na(event_abs))
    statistics <- rowSums(event_rank, na.rm = T)



    significance <- rep("", length(statistics))
    significance[statistics >= qsignrank(1 - 0.1, n = N) |
            statistics <= N / (N + 1) - qsignrank(1 - 0.1 / 2, n = N)] <- "*"
    significance[statistics >= qsignrank(1 - 0.05, n = N) |
            statistics <= N / (N + 1) - qsignrank(1 - 0.05 / 2, n = N)] <- "**"
    significance[statistics >= qsignrank(1 - 0.01, n = N) |
            statistics <= N / (N + 1) - qsignrank(1 - 0.01 / 2, n = N)] <- "***"

    result <- cbind(result, data.frame(wlcx_stat = statistics,
                                       wlcx_signif = significance))
    return(result)
}
