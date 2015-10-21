#' Returns the result for all parametric tests for event study.
#'
#' Performs main parametric tests for each date in the event window and returns
#' the table of statistics and significance.
#'
#' \code{parametric_tests} performs given tests among \code{brown_warner_1980},
#' \code{brown_warner_1985}, \code{t_test}, \code{patell}, \code{boehmer} and
#' merge result to single table. If \code{all = T} (by default), the function
#' ignores the value of \code{tests}.
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @param all a logical value indicating whether all tests should be performed.
#' The default value is \code{TRUE}.
#' @param tests the list of tests functions among \code{brown_warner_1980},
#' \code{brown_warner_1985}, \code{t_test}, \code{patell}, and \code{boehmer}.
#' @return The single table of statistics and significances of all tests.
#'
#' @references \itemize{
#' \item Brown S.J., Warner J.B. \emph{Measuring security price performance}.
#' Journal of Financial Economics, 8:205-258, 1980.
#' \item Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case of
#' Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#' \item Boehmer, E. \emph{Event-study methodology under conditions of event-
#' induced variance}. Journal of Financial Economics, 30(2):253-272, 1991.
#' \item Patell J.M. \emph{Corporate forecasts of earnings per share and stock
#' price behavior: empirical tests}. Journal of Accounting Research, 14(2):246-
#' 276, 1976.}
#'
#' @seealso \code{\link{brown_warner_1980}}, \code{\link{brown_warner_1985}},
#' \code{\link{t_test}}, \code{\link{patell}}, and \code{\link{boehmer}}
#' @export
parametric_tests <- function(list_of_returns, event_start, event_end, all = T,
                             tests) {
    if(all == T) {
        tests <- list(brown_warner_1980, brown_warner_1985, t_test, patell,
                      boehmer)
    }
    result <- NULL
    for(test in tests) {
        # browser()
        if(is.null(result)) {
            result <- test(list_of_returns, event_start, event_end)
        } else {
            tryCatch(
                result <- merge(x = result, y = test(list_of_returns,
                                event_start, event_end)[, c(1, 5, 6)],
                                by = "date", all = T),
                error = function(x) warning(paste(x$message,
                                                  "The test will be skip.")))
        }
    }
    return(result)
}

#' Brown and Warner parametric test (1980).
#'
#' Parametric test for event study, which is descibed in Brown and Warner 1980
#' paper.
#'
#' Performs the parametric test for event study, which is descibed in Brown and
#' Warner 1980 paper. The test assumes cross-sectional independence and
#' insignificance of event-induced variance. The test examines the hypothesis
#' whether the theoretical cross-sectional expected value for a given day is
#' equal to zero. The standard deviation in statistics is calculated as the
#' cross-sectional mean of companies' variances, estimated on estimation period.
#' It calculates statistics even if event window and estimation period are
#' overlapped (intersect). The critical values are Student's t-destributed (no
#' approximation in limit). The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***). It was designed to
#' measure monthly data: for daily data look at Brown and Warner 1985 paper and
#' \code{brown_warner_1985}.
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references Brown S.J., Warner J.B. \emph{Measuring security price
#' performance}. Journal of Financial Economics, 8:205-258, 1980.
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1985}},
#' \code{\link{t_test}}, \code{\link{patell}}, and \code{\link{boehmer}}
#' @export
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
    estimation_abnormal <- NULL
    event_abnormal <- NULL
    delta <- numeric(length(list_of_returns))
    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.character(i), "-th company estimation",
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

        if(is.null(estimation_abnormal)) {
            estimation_abnormal <- company_estimation_abnormal
        } else {
            estimation_abnormal <- merge(estimation_abnormal,
                                         company_estimation_abnormal, all = T)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = T)
        }

        delta[i] <- list_of_returns[[i]]$estimation_length
    }
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = T) /
                                      ncol(event_abnormal) * 100,
                         mean = rowMeans(event_abnormal, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)

    mean_delta <- mean(delta)

    sd_estimation_period <- sqrt(sum(matrixStats::colVars(estimation_abnormal,
                                                          na.rm = T), na.rm = T)) /
        ncol(estimation_abnormal)
    statistics <- rowMeans(event_abnormal, na.rm = T) /
        sd_estimation_period
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= qt(1 - 0.10/2, mean_delta)] <- "*"
    significance[abs(statistics) >= qt(1 - 0.05/2, mean_delta)] <- "**"
    significance[abs(statistics) >= qt(1 - 0.01/2, mean_delta)] <- "***"
    result <- cbind(result, data.frame(bw_1980_stat = statistics,
                                       bw_1980_signif = significance))
    return(result)
}

#' Brown and Warner parametric test (1985).
#'
#' Parametric test for event study, which is descibed in Brown and Warner 1985
#' paper.
#'
#' Performs the parametric test for event study, which is descibed in Brown and
#' Warner 1985 paper, which is traditional event study approach. Assumes
#' cross-sectional independence and non-robust to event-induced variance. The
#' test examines the hypothesis whether the theoretical cross-sectional expected
#' value for a given day is equal to zero. The standard deviation in statistics
#' is estimated as the cross-sectional standard deviation of companies' means,
#' estimated on estimation period. It calculates statistics even if event window
#' and estimation period are overlapped (intersect). The critical values are
#' Student's t-destributed (no approximation in limit). The significance levels
#' of \eqn{\alpha} are 0.1, 0.05, and 0.01 (marked respectively by *, **, and
#' ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#'  of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{t_test}}, \code{\link{patell}}, and \code{\link{boehmer}}
#' @export
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
    estimation_abnormal <- NULL
    event_abnormal <- NULL
    delta <- numeric(length(list_of_returns))

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.character(i), "-th company estimation",
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

        if(is.null(estimation_abnormal)) {
            estimation_abnormal <- company_estimation_abnormal
        } else {
            estimation_abnormal <- merge(estimation_abnormal,
                                         company_estimation_abnormal, all = T)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = T)
        }

        delta[i] <- list_of_returns[[i]]$estimation_length
    }

    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = T) /
                             ncol(event_abnormal) * 100,
                         mean = rowMeans(event_abnormal, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)

    mean_delta <- mean(delta)

    sd_estimation_period <- sqrt(var(rowMeans(estimation_abnormal, na.rm = T),
                                     na.rm = T))
    statistics <- rowMeans(event_abnormal, na.rm = T) / sd_estimation_period
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= qt(1 - 0.10/2, mean_delta)] <- "*"
    significance[abs(statistics) >= qt(1 - 0.05/2, mean_delta)] <- "**"
    significance[abs(statistics) >= qt(1 - 0.01/2, mean_delta)] <- "***"
    result <- cbind(result, data.frame(bw_1985_stat = statistics,
                                       bw_1985_signif = significance))
    return(result)
}

#' t-test for event study.
#'
#' Classical t-test, which examine each date in event window.
#'
#' Performs the t-test for event study. The procedure of this test is described
#' in Boehmer 1991, sometimes is called cross-sectional test. Assumes
#' independens of securities, however is stable to event-induced variance. This
#' test examines the equality of cross-sectional expected value to zero. The
#' standard deviation, which is used in this test, is simply cross-section
#' standard deviation for given day in the event window. It calculates
#' statistics even if event window and estimation period are overlapped
#' (intersect). The critical values are Student's t-destributed (no
#' approximation in limit). The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @section Warning: This test strongly requires cross-sectional independence
#' and sensative to the size of the sample.
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
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
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{brown_warner_1985}}, \code{\link{patell}}, and
#' \code{\link{boehmer}}
#' @export
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

    event_number_of_companies <- rowSums(!is.na(event_abnormal), na.rm = T)

    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = event_number_of_companies /
                             ncol(event_abnormal) * 100,
                         mean = rowMeans(event_abnormal, na.rm = T))

    event_abnormal <- as.matrix(event_abnormal)


    statistics <- rowMeans(event_abnormal, na.rm = T) /
                  matrixStats::rowSds(event_abnormal, na.rm = T) *
                  sqrt(event_number_of_companies)


    significance <- rep("", length(statistics))
    significance[abs(statistics) >=
                     qt(1 - 0.10/2, event_number_of_companies)] <- "*"
    significance[abs(statistics) >=
                     qt(1 - 0.05/2, event_number_of_companies)] <- "**"
    significance[abs(statistics) >=
                     qt(1 - 0.01/2, event_number_of_companies)] <- "***"
    result <- cbind(result, data.frame(t_test_stat = statistics,
                                       t_test_signif = significance))
    return(result)

}

#' Patell's parametric test (1976).
#'
#' Parametric test for event study, which is descibed in Patell's 1976
#' paper.
#'
#' Performs the parametric test for event study, which is descibed in Patell's
#' 1976 paper, which is called standardized-residuals method in Boehmer's 1991
#' paper. The test assumptions are cross-sectional independence and
#' insignificance of event-induced variance. The standardization smooths the
#' effect of event-induced variance comparing to Brown and Warner tests. Also
#' standardization incorporates the situation, when high volatility secturity
#' dominates the test. The test examines the hypothesis whether the theoretical
#' cross-sectional expected value for a given day is equal to zero. It
#' calculates statistics even if event window and estimation period are
#' overlapped (intersect). The critical values are standard normal. The
#' significance levels of \eqn{\alpha} are 0.1, 0.05, and 0.01 (marked
#' respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references \itemize{
#' \item Patell J.M. \emph{Corporate forecasts of earnings per share and stock
#' price behavior: empirical tests}. Journal of Accounting Research, 14(2):246-
#' 276, 1976.
#' \item Boehmer E., Musumeci J., Poulsen A.B. \emph{ Event-study methodology
#' under conditions of event-induced variance}. Journal of Financial Economics,
#' 30(2):253-272, 1991.}
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{brown_warner_1985}}, \code{\link{t_test}}, and
#' \code{\link{boehmer}}
#' @export
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
    estimation_abnormal <- NULL
    event_abnormal <- NULL
    event_standardized_abnormal <- NULL
    delta <- numeric(length(list_of_returns))
    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.character(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        if(list_of_returns[[i]]$market_model != "sim") {
            stop("Patell's test is applicable only for Single-Index market model.")
        }

        company_estimation_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >=
            list_of_returns[[i]]$estimation_start &
            zoo::index(list_of_returns[[i]]$abnormal) <=
            list_of_returns[[i]]$estimation_end])
        company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
            zoo::index(list_of_returns[[i]]$abnormal) <= event_end])

        if(is.null(estimation_abnormal)) {
            estimation_abnormal <- company_estimation_abnormal
        } else {
            estimation_abnormal <- merge(estimation_abnormal,
                                         company_estimation_abnormal, all = T)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = T)
        }


        market_event <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >= event_start &
            zoo::index(list_of_returns[[i]]$regressor) <= event_end])
        market_estimation <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >=
            list_of_returns[[i]]$estimation_start &
            zoo::index(list_of_returns[[i]]$regressor) <=
            list_of_returns[[i]]$estimation_end])
        mean_market_estimation <- mean(market_estimation, na.rm = T)

        company_event_standardized <- company_event_abnormal /
            sd(company_estimation_abnormal, na.rm = T) /
            sqrt(1 + 1 / list_of_returns[[i]]$estimation_length +
                 (market_event - mean_market_estimation) ^ 2 /
                 sum((market_estimation - mean_market_estimation) ^ 2, na.rm = T))

        if(is.null(event_standardized_abnormal)) {
            event_standardized_abnormal <- company_event_standardized
        } else {
            event_standardized_abnormal <- merge(event_standardized_abnormal,
                                                 company_event_standardized,
                                                 all = T)
        }
        delta[i] <- list_of_returns[[i]]$estimation_length

    }

    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = T) /
                             ncol(event_abnormal) * 100,
                         mean = rowMeans(event_abnormal, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    event_standardized_abnormal <- as.matrix(event_standardized_abnormal)

    statistics <- rowSums(event_standardized_abnormal, na.rm = T) /
        sqrt(sum((delta - 2) / (delta - 4)))
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(pt_stat = statistics,
                                       pt_signif = significance))
    return(result)
}

#' Boehmer's parametric test (1991).
#'
#' Parametric test for event study, which is descibed in Boehmer's 1991
#' paper.
#'
#' Performs the parametric test for event study, which is descibed in Boehmer's
#' 1991 paper, also called hybrid test or standardized cross-sectional test.
#' This test performs t-test based on Patell's standardized resuduals. By
#' combining Patell's and t- tests this test allows event-induced variance
#' changes, but still assumes cross-sectional independence. The test examines
#' the hypothesis whether the theoretical cross-sectional expected value for a
#' given day is equal to zero. It calculates statistics even if event window and
#' estimation period are overlapped (intersect). The critical values has
#' Student's t-distribution. The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns list of objects of S3 class \code{return}, each element
#' of which is treated as a company.
#' @param event_start the object of class \code{Date}, which represents the
#' first (starting) date of the event window.
#' @param event_end the object of class \code{Date}, which represents the last
#' (ending) date in the event window.
#' @return The table of statistics and significances of the test.
#'
#' @references \itemize{
#' \item Patell J.M. \emph{Corporate forecasts of earnings per share and stock
#' price behavior: empirical tests}. Journal of Accounting Research, 14(2):246-
#' 276, 1976.
#' \item Boehmer E., Musumeci J., Poulsen A.B. \emph{Event-study methodology
#' under conditions of event-induced variance}. Journal of Financial Economics,
#' 30(2):253-272, 1991.}
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{brown_warner_1985}}, \code{\link{t_test}}, and
#' \code{\link{patell}}
#' @export
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
    estimation_abnormal <- NULL
    event_abnormal <- NULL
    event_standardized_abnormal <- NULL
    # delta <- numeric(length(list_of_returns))

    for(i in seq_along(list_of_returns)) {

        # check whether each element of list_of_returns is returns
        if(!inherits(list_of_returns[[i]], "returns")) {
            stop("Each element of list_of_rates must have class returns.")
        }

        if(list_of_returns[[i]]$estimation_end >= event_start) {
            message(paste0("For ", as.character(i), "-th company estimation",
                           " period overlaps with event period."))
        }

        if(list_of_returns[[i]]$market_model != "sim") {
            stop("Boehmer's test is applicable only for Single-Index market model.")
        }

        company_estimation_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >=
                list_of_returns[[i]]$estimation_start &
            zoo::index(list_of_returns[[i]]$abnormal) <=
                list_of_returns[[i]]$estimation_end])
        company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[
            zoo::index(list_of_returns[[i]]$abnormal) >= event_start &
            zoo::index(list_of_returns[[i]]$abnormal) <= event_end])

        if(is.null(estimation_abnormal)) {
            estimation_abnormal <- company_estimation_abnormal
        } else {
            estimation_abnormal <- merge(estimation_abnormal,
                                         company_estimation_abnormal, all = T)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = T)
        }


        market_event <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >= event_start &
            zoo::index(list_of_returns[[i]]$regressor) <= event_end])
        market_estimation <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >=
                list_of_returns[[i]]$estimation_start &
            zoo::index(list_of_returns[[i]]$regressor) <=
                list_of_returns[[i]]$estimation_end])
        mean_market_estimation <- mean(market_estimation, na.rm = T)

        company_event_standardized <- company_event_abnormal /
            sd(company_estimation_abnormal, na.rm = T) /
            sqrt(1 + 1 / list_of_returns[[i]]$estimation_length +
                     (market_event - mean_market_estimation) ^ 2 /
                     sum((market_estimation - mean_market_estimation) ^ 2, na.rm = T))

        if(is.null(event_standardized_abnormal)) {
            event_standardized_abnormal <- company_event_standardized
        } else {
            event_standardized_abnormal <- merge(event_standardized_abnormal,
                                                 company_event_standardized,
                                                 all = T)
        }
        # delta[i] <- list_of_returns[[i]]$estimation_length
    }

    event_number_of_companies <- rowSums(!is.na(event_abnormal), na.rm = T)
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = T) /
                             ncol(event_abnormal) * 100,
                         mean = rowMeans(event_abnormal, na.rm = T))

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    event_standardized_abnormal <- as.matrix(event_standardized_abnormal)

    statistics <- rowMeans(event_standardized_abnormal, na.rm = T) /
                  matrixStats::rowSds(event_standardized_abnormal, na.rm = T) *
                  sqrt(event_number_of_companies)
    significance <- rep("", length(statistics))
    significance[abs(statistics) >=
                     qt(1 - 0.10/2, event_number_of_companies)] <- "*"
    significance[abs(statistics) >=
                     qt(1 - 0.05/2, event_number_of_companies)] <- "**"
    significance[abs(statistics) >=
                     qt(1 - 0.01/2, event_number_of_companies)] <- "***"
    result <- cbind(result, data.frame(bh_stat = statistics,
                                       bh_signif = significance))
    return(result)
}

