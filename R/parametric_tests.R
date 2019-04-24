#' Returns the result of given event study parametric tests.
#'
#' Performs main parametric tests for each date in the event window and returns
#' a data frame of their statistics and significance.
#'
#' \code{parametric_tests} performs given tests among \code{brown_warner_1980},
#' \code{brown_warner_1985}, \code{t_test}, \code{patell}, \code{boehmer},
#' \code{lamb} and merge result to a single data frame. If \code{all = TRUE}
#' (the default value), the function ignores the value of \code{tests}.
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @param all a logical vector of length one indicating whether all tests should
#' be performed. The default value is \code{TRUE}.
#' @param tests a list of tests' functions among \code{brown_warner_1980},
#' \code{brown_warner_1985}, \code{t_test}, \code{patell}, \code{boehmer}, and
#' \code{lamb}.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item Various tests' statistics and significances
#' }
#'
#' @references \itemize{
#' \item Brown S.J., Warner J.B. \emph{Measuring security price performance}.
#' Journal of Financial Economics, 8:205-258, 1980.
#' \item Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case of
#' Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#' \item Boehmer E., Musumeci J., Poulsen A.B. \emph{Event-study methodology
#' under conditions of event-induced variance}. Journal of Financial Economics,
#' 30(2):253-272, 1991.
#' \item Patell J.M. \emph{Corporate forecasts of earnings per share and stock
#' price behavior: empirical tests}. Journal of Accounting Research, 14(2):246-
#' 276, 1976.
#' \item Lamb R.P. \emph{An Exposure-Based Analysis of Property-Liability
#' Insurer Stock Values around Hurricane Andrew}. Journal of Risk and Insurance,
#' 62(1):111-123, 1995.}
#'
#' @seealso \code{\link{brown_warner_1980}}, \code{\link{brown_warner_1985}},
#' \code{\link{t_test}}, \code{\link{patell}}, \code{\link{boehmer}}, and
#' \code{\link{lamb}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' nine_eleven_param <- get_prices_from_tickers(tickers,
#'                                              start = as.Date("2000-01-01"),
#'                                              end = as.Date("2002-01-01"),
#'                                              quote = "Close",
#'                                              retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     parametric_tests(event_start = as.Date("2001-09-11"),
#'                      event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' nine_eleven_param <- parametric_tests(list_of_returns = securities_returns,
#'                                       event_start = as.Date("2001-09-11"),
#'                                       event_end = as.Date("2001-09-28"))
#'
#' @export
parametric_tests <- function(list_of_returns, event_start, event_end,
                             all = TRUE, tests) {
    if(missing(tests)) {
        if(all) {
            tests <- list(brown_warner_1980, brown_warner_1985, t_test, patell,
                          boehmer, lamb)
        } else {
            stop("Specify at least one test.")
        }
    } else {
        message("Argument all will be ignored.")
        for(i in seq_along(tests)) {
            tests[[i]] <- match.fun(tests[[i]])
        }
    }
    result <- NULL
    for(test in tests) {
        if(is.null(result)) {
            result <- test(list_of_returns, event_start, event_end)
        } else {
            tryCatch(
                result <- merge(x = result, y = test(list_of_returns,
                                event_start, event_end)[, c(1, 5, 6)],
                                by = "date", all = TRUE),
                error = function(x) warning(paste(x$message,
                                                  "The test will be skip.")))
        }
    }
    return(result)
}

#' Brown and Warner parametric test (1980).
#'
#' An event study parametric test described in Brown and Warner 1980.
#'
#' Performs a parametric test for the event study, which is described in
#' Brown and Warner 1980. The test assumes a cross-sectional independence
#' and an insignificance of event-induced variance. The test examines the
#' hypothesis whether the theoretical cross-sectional expected value for a given
#' day is equal to zero. The standard deviation in statistics is calculated as
#' the cross-sectional mean of companies' variances, estimated on the estimation
#' period. It calculates statistics even if the event window and the estimation
#' period are overlapped (intersect). The critical values are Student's
#' t-distributed (no approximation in limit). The significance levels of
#' \eqn{\alpha} are 0.1, 0.05, and 0.01 (marked respectively by *, **, and ***).
#' It was designed to measure monthly data: for daily data look at Brown and
#' Warner 1985 and \code{brown_warner_1985}.
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item \code{bw_1980_stat}: a Brown and Warner (1980) test statistic
#'     \item \code{bw_1980_signif}: a significance of the statistic
#' }
#'
#' @references Brown S.J., Warner J.B. \emph{Measuring security price
#' performance}. Journal of Financial Economics, 8:205-258, 1980.
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1985}},
#' \code{\link{t_test}}, \code{\link{patell}}, \code{\link{boehmer}}, and
#' \code{\link{lamb}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' get_prices_from_tickers(tickers,
#'                         start = as.Date("2000-01-01"),
#'                         end = as.Date("2002-01-01"),
#'                         quote = "Close",
#'                         retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     brown_warner_1980(event_start = as.Date("2001-09-11"),
#'                       event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' brown_warner_1980(list_of_returns = securities_returns,
#'                   event_start = as.Date("2001-09-11"),
#'                   event_end = as.Date("2001-09-28"))
#'
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
                                         company_estimation_abnormal, all = TRUE)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = TRUE)
        }

        delta[i] <- list_of_returns[[i]]$estimation_length
    }
    event_means <- rowMeans(event_abnormal, na.rm = TRUE)
    event_means[is.nan(event_means)] <- NA
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = TRUE) /
                                      ncol(event_abnormal) * 100,
                         mean = event_means)

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    mean_delta <- mean(delta)

    sd_estimation_period <- sqrt(sum(matrixStats::colVars(estimation_abnormal,
                                                          na.rm = TRUE), na.rm = TRUE)) /
        ncol(estimation_abnormal)
    statistics <- event_means / sd_estimation_period
    statistics[is.nan(statistics)] <- NA
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= stats::qt(1 - 0.10/2, mean_delta - 1)] <- "*"
    significance[abs(statistics) >= stats::qt(1 - 0.05/2, mean_delta - 1)] <- "**"
    significance[abs(statistics) >= stats::qt(1 - 0.01/2, mean_delta - 1)] <- "***"
    result <- cbind(result, data.frame(bw_1980_stat = statistics,
                                       bw_1980_signif = significance))
    rownames(result) <- NULL
    return(result)
}

#' Brown and Warner parametric test (1985).
#'
#' An event study parametric test described in Brown and Warner 1985.
#'
#' Performs a parametric test for event study, which is described in Brown and
#' Warner 1985, which is a traditional event study approach. This test does not
#' require cross-sectional independence but is non-robust to an event-induced
#' variance. The test examines the hypothesis whether the theoretical
#' cross-sectional expected value for a given day is equal to zero. The standard
#'  deviation in statistics is estimated as the cross-sectional standard
#' deviation of companies' means, estimated on the estimation period. It
#' calculates statistics even if event window and estimation period are
#' overlapped (intersect). The critical values are Student's t-distributed (no
#' approximation in limit). The significance levels of \eqn{\alpha} are 0.1,
#'  0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item \code{bw_1985_stat}: a Brown and Warner (1985) test statistic
#'     \item \code{bw_1985_signif}: a significance of the statistic
#' }
#'
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#'  of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{t_test}}, \code{\link{patell}}, \code{\link{boehmer}}, and
#' \code{\link{lamb}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' get_prices_from_tickers(tickers,
#'                         start = as.Date("2000-01-01"),
#'                         end = as.Date("2002-01-01"),
#'                         quote = "Close",
#'                         retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     brown_warner_1985(event_start = as.Date("2001-09-11"),
#'                       event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' brown_warner_1985(list_of_returns = securities_returns,
#'                   event_start = as.Date("2001-09-11"),
#'                   event_end = as.Date("2001-09-28"))
#'
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
                                         company_estimation_abnormal, all = TRUE)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = TRUE)
        }

        delta[i] <- list_of_returns[[i]]$estimation_length
    }
    event_means <- rowMeans(event_abnormal, na.rm = TRUE)
    event_means[is.nan(event_means)] <- NA
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = TRUE) /
                             ncol(event_abnormal) * 100,
                         mean = event_means)

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    mean_delta <- mean(delta)

    sd_estimation_period <- sqrt(stats::var(rowMeans(estimation_abnormal, na.rm = TRUE),
                                     na.rm = TRUE))
    statistics <- event_means / sd_estimation_period
    statistics[is.nan(statistics)] <- NA
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= stats::qt(1 - 0.10/2, mean_delta - 1)] <- "*"
    significance[abs(statistics) >= stats::qt(1 - 0.05/2, mean_delta - 1)] <- "**"
    significance[abs(statistics) >= stats::qt(1 - 0.01/2, mean_delta - 1)] <- "***"
    result <- cbind(result, data.frame(bw_1985_stat = statistics,
                                       bw_1985_signif = significance))
    rownames(result) <- NULL
    return(result)
}

#' An event study t-test.
#'
#' A classical t-test that examines each date in the event window.
#'
#' Performs a t-test for the event study. The procedure of this test is
#' described in Boehmer et al. 1991, sometimes is called a cross-sectional test.
#' Assumes independence of securities, however is stable to event-induced
#' variance. This test examines the equality of the cross-sectional expected
#' value to zero. The standard deviation, which is used in this test, is simply
#' a cross-sectional standard deviation for a given day in the event window. It
#' calculates statistics even if event window and estimation period are
#' overlapped (intersect). The critical values are Student's t-distributed (no
#' approximation in limit). The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @section Warning: This test strongly requires cross-sectional independence
#' and sensitive to the size of the sample.
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item \code{t_test_stat}: a t-test statistic
#'     \item \code{t_test_signif}: a significance of the statistic
#' }
#'
#' @references Boehmer E., Musumeci J., Poulsen A.B. \emph{Event-study
#' methodology under conditions of event-induced variance}. Journal of Financial
#' Economics, 30(2):253-272, 1991.
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{brown_warner_1985}}, \code{\link{patell}}, \code{\link{boehmer}},
#' and \code{\link{lamb}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' get_prices_from_tickers(tickers,
#'                         start = as.Date("2000-01-01"),
#'                         end = as.Date("2002-01-01"),
#'                         quote = "Close",
#'                         retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     t_test(event_start = as.Date("2001-09-11"),
#'            event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' t_test(list_of_returns = securities_returns,
#'        event_start = as.Date("2001-09-11"),
#'        event_end = as.Date("2001-09-28"))
#'
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
                                    all = TRUE)
        }
    }
    event_number_of_companies <- rowSums(!is.na(event_abnormal), na.rm = TRUE)
    event_means <- rowMeans(event_abnormal, na.rm = TRUE)
    event_means[is.nan(event_means)] <- NA
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = event_number_of_companies /
                             ncol(event_abnormal) * 100,
                         mean = event_means)

    event_abnormal <- as.matrix(event_abnormal)


    statistics <- event_means /
                  matrixStats::rowSds(event_abnormal, na.rm = TRUE) *
                  sqrt(event_number_of_companies)
    statistics[is.nan(statistics)] <- NA

    df <- event_number_of_companies - 1
    df[df <= 0] <- NA
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= stats::qt(1 - 0.10/2, df)] <- "*"
    significance[abs(statistics) >= stats::qt(1 - 0.05/2, df)] <- "**"
    significance[abs(statistics) >= stats::qt(1 - 0.01/2, df)] <- "***"
    result <- cbind(result, data.frame(t_test_stat = statistics,
                                       t_test_signif = significance))
    rownames(result) <- NULL
    return(result)

}

#' Patell's parametric test (1976).
#'
#' An event study parametric test described in Patell 1976.
#'
#' Performs a parametric test for event study, which is described in Patell
#' 1976, which is called standardized-residuals method in Boehmer 1991.
#' Test's assumptions are a cross-sectional independence and an
#' insignificance of an event-induced variance. The standardization smooths the
#' effect of the event-induced variance comparing to Brown and Warner tests.
#' Also standardization incorporates the situation, when a highly volatile
#' security dominates the test. The test examines the hypothesis whether the
#' theoretical cross-sectional expected value for a given day is equal to zero.
#' It calculates statistics even if event window and estimation period are
#' overlapped (intersect). The critical values are standard normal. The
#' significance levels of \eqn{\alpha} are 0.1, 0.05, and 0.01 (marked
#' respectively by *, **, and ***).
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item \code{pt_stat}: a Patell's test statistic
#'     \item \code{pt_signif}: a significance of the statistic
#' }
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
#' \code{\link{boehmer}}, and \code{\link{lamb}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' get_prices_from_tickers(tickers,
#'                         start = as.Date("2000-01-01"),
#'                         end = as.Date("2002-01-01"),
#'                         quote = "Close",
#'                         retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     patell(event_start = as.Date("2001-09-11"),
#'            event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' patell(list_of_returns = securities_returns,
#'        event_start =  as.Date("2001-09-11"),
#'        event_end = as.Date("2001-09-28"))
#'
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

        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = TRUE)
        }


        market_event <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >= event_start &
            zoo::index(list_of_returns[[i]]$regressor) <= event_end])
        market_estimation <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >=
            list_of_returns[[i]]$estimation_start &
            zoo::index(list_of_returns[[i]]$regressor) <=
            list_of_returns[[i]]$estimation_end])
        mean_market_estimation <- mean(market_estimation, na.rm = TRUE)

        company_event_standardized <- company_event_abnormal /
            stats::sd(company_estimation_abnormal, na.rm = TRUE) /
            sqrt(1 + 1 / list_of_returns[[i]]$estimation_length +
                 (market_event - mean_market_estimation) ^ 2 /
                 sum((market_estimation - mean_market_estimation) ^ 2, na.rm = TRUE))

        if(is.null(event_standardized_abnormal)) {
            event_standardized_abnormal <- company_event_standardized
        } else {
            event_standardized_abnormal <- merge(event_standardized_abnormal,
                                                 company_event_standardized,
                                                 all = TRUE)
        }
        delta[i] <- list_of_returns[[i]]$estimation_length

    }
    event_means <- rowMeans(event_abnormal, na.rm = TRUE)
    event_means[is.nan(event_means)] <- NA
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = TRUE) /
                             ncol(event_abnormal) * 100,
                         mean = event_means)

    event_abnormal <- as.matrix(event_abnormal)
    event_standardized_abnormal <- as.matrix(event_standardized_abnormal)

    # because by definition the sum of empty set is zero, which in the estudy
    # case should be represented as NA, we use trick: mean * n = sum

    statistics <- rowMeans(event_standardized_abnormal, na.rm = TRUE) *
        ncol(event_standardized_abnormal) /
        sqrt(sum((delta - 2) / (delta - 4)))
    statistics[is.nan(statistics)] <- NA
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(pt_stat = statistics,
                                       pt_signif = significance))
    rownames(result) <- NULL
    return(result)
}

#' Boehmer's parametric test (1991).
#'
#' An event study parametric test described in Boehmer 1991.
#'
#' Performs a parametric test for event study, which is described in Boehmer
#' 1991. Also called hybrid test or standardized cross-sectional test.
#' This test performs t-test based on Patell's standardized residuals. By
#' combining Patell's and t-tests, this test allows for event-induced variance
#' changes, but still assumes cross-sectional independence. The test examines
#' the hypothesis whether the theoretical cross-sectional expected value for a
#' given day is equal to zero. It calculates statistics even if event window and
#' estimation period are overlapped (intersect). The critical values has
#' Student's t-distribution. The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item \code{bh_stat}: a Boehmer's test statistic
#'     \item \code{bh_signif}: a significance of the statistic
#' }
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
#' \code{\link{brown_warner_1985}}, \code{\link{t_test}}, \code{\link{patell}},
#' and \code{\link{lamb}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' get_prices_from_tickers(tickers,
#'                         start = as.Date("2000-01-01"),
#'                         end = as.Date("2002-01-01"),
#'                         quote = "Close",
#'                         retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     boehmer(event_start = as.Date("2001-09-11"),
#'             event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' boehmer(list_of_returns = securities_returns,
#'         event_start =  as.Date("2001-09-11"),
#'         event_end = as.Date("2001-09-28"))
#'
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

        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = TRUE)
        }


        market_event <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >= event_start &
            zoo::index(list_of_returns[[i]]$regressor) <= event_end])
        market_estimation <- zoo::as.zoo(list_of_returns[[i]]$regressor[
            zoo::index(list_of_returns[[i]]$regressor) >=
                list_of_returns[[i]]$estimation_start &
            zoo::index(list_of_returns[[i]]$regressor) <=
                list_of_returns[[i]]$estimation_end])
        mean_market_estimation <- mean(market_estimation, na.rm = TRUE)

        company_event_standardized <- company_event_abnormal /
            stats::sd(company_estimation_abnormal, na.rm = TRUE) /
            sqrt(1 + 1 / list_of_returns[[i]]$estimation_length +
                     (market_event - mean_market_estimation) ^ 2 /
                     sum((market_estimation - mean_market_estimation) ^ 2, na.rm = TRUE))

        if(is.null(event_standardized_abnormal)) {
            event_standardized_abnormal <- company_event_standardized
        } else {
            event_standardized_abnormal <- merge(event_standardized_abnormal,
                                                 company_event_standardized,
                                                 all = TRUE)
        }
        # delta[i] <- list_of_returns[[i]]$estimation_length
    }
    event_means <- rowMeans(event_abnormal, na.rm = TRUE)
    event_means[is.nan(event_means)] <- NA
    event_number_of_companies <- rowSums(!is.na(event_abnormal), na.rm = TRUE)
    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = event_number_of_companies /
                             ncol(event_abnormal) * 100,
                         mean = event_means)

    event_abnormal <- as.matrix(event_abnormal)
    event_standardized_abnormal <- as.matrix(event_standardized_abnormal)

    statistics <- rowMeans(event_standardized_abnormal, na.rm = TRUE) /
                  matrixStats::rowSds(event_standardized_abnormal, na.rm = TRUE) *
                  sqrt(event_number_of_companies)
    statistics[is.nan(statistics)] <- NA

    df <- event_number_of_companies - 1
    df[df <= 0] <- NA
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= stats::qt(1 - 0.10/2, df)] <- "*"
    significance[abs(statistics) >= stats::qt(1 - 0.05/2, df)] <- "**"
    significance[abs(statistics) >= stats::qt(1 - 0.01/2, df)] <- "***"
    result <- cbind(result, data.frame(bh_stat = statistics,
                                       bh_signif = significance))
    rownames(result) <- NULL
    return(result)
}

#' Lamb's parametric test (1995).
#'
#' An event study parametric test described in Lamb 1995.
#'
#' Performs a parametric test for the event study, which is described in Lamb
#' 1995. The author refers to Warner and Brown 1985 and Henderson
#' 1990. However, this test was not observed in neither papers. The test
#' statistics are very close to the statistics produced by
#' \code{brown_warner_1985} and typically has the same significance. The test
#' examines the hypothesis whether the theoretical cross-sectional expected
#' value for a given day is equal to zero. It calculates statistics even if
#' event window and estimation period are overlapped (intersect). The critical
#' values are standard normal. The significance levels of \eqn{\alpha} are 0.1,
#' 0.05, and 0.01 (marked respectively by *, **, and ***).
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param event_start an object of \code{Date} class giving the first date of
#' the event period.
#' @param event_end an object of \code{Date} class giving the last date of the
#' event period.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{date}: a calendar date
#'     \item \code{weekday}: a day of the week
#'     \item \code{percentage}: a share of non-missing observations for a given
#'           day
#'     \item \code{mean}: an average abnormal return
#'     \item \code{lmb_stat}: a Lamb's test statistic
#'     \item \code{lmb_signif}: a significance of the statistic
#' }
#'
#' @references Lamb R.P. \emph{An Exposure-Based Analysis of Property-Liability
#' Insurer Stock Values around Hurricane Andrew}. Journal of Risk and Insurance,
#' 62(1):111-123, 1995.
#'
#' @seealso \code{\link{parametric_tests}}, \code{\link{brown_warner_1980}},
#' \code{\link{brown_warner_1985}}, \code{\link{t_test}},\code{\link{patell}}
#' and \code{\link{boehmer}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^STOXX50E",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "RSA.L", "TOP.CO")
#' get_prices_from_tickers(tickers,
#'                         start = as.Date("2000-01-01"),
#'                         end = as.Date("2002-01-01"),
#'                         quote = "Close",
#'                         retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     lamb(event_start = as.Date("2001-09-11"),
#'          event_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' lamb(list_of_returns = securities_returns,
#'      event_start =  as.Date("2001-09-11"),
#'      event_end = as.Date("2001-09-28"))
#'
#' @export
lamb <- function(list_of_returns, event_start, event_end) {
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
    estimation_market <- NULL
    event_market <- NULL
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

        if(is.null(estimation_market)) {
            estimation_market <- zoo::as.zoo(list_of_returns[[i]]$regressor[
                             zoo::index(list_of_returns[[i]]$regressor) >=
                                 list_of_returns[[i]]$estimation_start &
                                 zoo::index(list_of_returns[[i]]$regressor) <=
                                 list_of_returns[[i]]$estimation_end])
        } else if (identical(estimation_market, list_of_returns[[i]]$regressor)) {
            stop("regressor must be the same for all companies.")
        }

        if(is.null(event_market)) {
            event_market <- zoo::as.zoo(list_of_returns[[i]]$regressor[
                zoo::index(list_of_returns[[i]]$regressor) >= event_start &
                zoo::index(list_of_returns[[i]]$regressor) <= event_end])
        } else if (identical(event_market, list_of_returns[[i]]$regressor)) {
            stop("regressor must be the same for all companies.")
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
                                         company_estimation_abnormal,
                                         all = TRUE)
        }
        if(is.null(event_abnormal)) {
            event_abnormal <- company_event_abnormal
        } else {
            event_abnormal <- merge(event_abnormal, company_event_abnormal,
                                    all = TRUE)
        }
        delta[i] <- list_of_returns[[i]]$estimation_length
    }

    event_means <- rowMeans(event_abnormal, na.rm = TRUE)
    event_means[is.nan(event_means)] <- NA

    estimation_means <- rowMeans(estimation_abnormal, na.rm = TRUE)
    estimation_means[is.nan(estimation_means)] <- NA

    estimation_market_mean <- mean(estimation_market, na.rm = TRUE)

    sum_estimation_market <- sum((estimation_market - estimation_market_mean)^2,
                                 na.rm = TRUE)

    result <- data.frame(date = zoo::index(event_abnormal),
                         weekday = weekdays(zoo::index(event_abnormal)),
                         percentage = rowSums(!is.na(as.matrix(event_abnormal)),
                                              na.rm = TRUE) /
                             ncol(event_abnormal) * 100,
                         mean = event_means)

    estimation_abnormal <- as.matrix(estimation_abnormal)
    event_abnormal <- as.matrix(event_abnormal)
    mean_delta <- mean(delta)



    sd <- stats::sd(estimation_means, na.rm = TRUE) * sqrt(1 + 1 / mean_delta +
            (event_market - estimation_market_mean)^2 / sum_estimation_market)
    statistics <- event_means / sd
    statistics[is.nan(statistics)] <- NA
    significance <- rep("", length(statistics))
    significance[abs(statistics) >= const_q1] <- "*"
    significance[abs(statistics) >= const_q2] <- "**"
    significance[abs(statistics) >= const_q3] <- "***"
    result <- cbind(result, data.frame(lmb_stat = statistics,
                                       lmb_signif = significance))
    rownames(result) <- NULL
    return(result)
}
