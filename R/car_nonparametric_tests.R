#' Returns the result of given event study nonparametric CAR tests.
#'
#' Performs given tests to examine the statistical significance of the CAR of a
#' given period.
#'
#' Currently, \code{car_nonparametric_tests} performs only \code{car_rank_test}
#' test. This function was developed for the sake of completeness and can be
#' used for future extensions of the package.
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param car_start an object of \code{Date} class giving the first date of
#' the CAR period.
#' @param car_end an object of \code{Date} class giving the last date of the
#' CAR period.
#' @param percentage a lowest allowed percentage of non-missing observation
#' for each day to be incorporated into CAR. The default value is 90 percent.
#' @param all a logical value indicating whether all tests should be performed.
#' The default value is \code{TRUE}. Note, only \code{car_rank_test} will be
#' performed.
#' @param tests a list of tests' functions. Currently, only \code{car_rank_test}
#'  is allowed.
#' @return A data frame of the following columns:
#' \itemize{
#'     \item \code{name}: a name of the test
#'     \item \code{car_start}: the first date of the CAR period
#'     \item \code{car_end}: the last date of the CAR period
#'     \item \code{average_percentage}: an average share of non-missing
#'           observations over the CAR period
#'     \item \code{statistic}: a test's statistic
#'     \item \code{number_of_days}: the number of days in the CAR period
#'     \item \code{significance}: a significance of the statistic
#' }
#'
#' @references \itemize{
#' \item Corrado C.J. \emph{A Nonparametric Test for Abnormal Security-Price
#' Performance in Event Studies}. Journal of Financial Economics 23:385-395,
#' 1989.
#' \item Cowan A.R. \emph{Nonparametric Event Study Tests}. Review of
#' Quantitative Finance and Accounting, 2:343-358, 1992.
#' }
#'
#' @seealso \code{\link{car_rank_test}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^N100",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "TOP.CO")
#' nine_eleven_car_param <- get_prices_from_tickers(tickers,
#'                                                  start = as.Date("2000-01-01"),
#'                                                  end = as.Date("2002-01-01"),
#'                                                  quote = "Close",
#'                                                  retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous") %>%
#'     apply_market_model(regressor = rates_indx,
#'                        same_regressor_for_all = TRUE,
#'                        market_model = "sim",
#'                        estimation_method = "ols",
#'                        estimation_start = as.Date("2001-03-26"),
#'                        estimation_end = as.Date("2001-09-10")) %>%
#'     car_nonparametric_tests(car_start = as.Date("2001-09-11"),
#'                             car_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' nine_eleven_car_param <- car_nonparametric_tests(
#'     list_of_returns = securities_returns,
#'     car_start = as.Date("2001-09-11"),
#'     car_end = as.Date("2001-09-28")
#' )
#'
#' @export
car_nonparametric_tests <- function(list_of_returns, car_start, car_end,
                                    percentage = 90, all = TRUE, tests) {
    if(missing(tests)) {
        if(all) {
            tests <- list(car_rank_test)
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
    for(i in seq_along(tests)) {
        if(is.null(result)) {
            result <- tests[[i]](list_of_returns, car_start, car_end,
                                 percentage)
        } else {
            tryCatch(
                result <- rbind(
                    result,
                    tests[[i]](list_of_returns, car_start, car_end,
                               percentage)
                ),
                error = function(x) warning(paste(x$message,
                                                  "The test will be skip.")))
        }
    }
    return(result)
}

#' Cowan's CAR test.
#'
#' A nonparametric test proposed by Cowan 1992 as an extension of the rank test
#'  proposed by Corrado 1989.
#'
#' This function performs a test proposed by Cowan 1992 to investigate the
#' significance of the CAR for a given period. In order to get ranks of
#' corresponding abnormal returns, the procedure uses regular R function
#' \code{\link{rank}} with parameter \code{ties.method = "average"} and
#' \code{na.last = "keep"}. For this test the estimation period and the event
#' period must not overlap, otherwise an error will be thrown. The test
#' statistic is assumed to have a normal distribution (as an approximation). The
#'  test is well-specified for the case, when cross-sectional abnormal returns
#' are not symmetric. The test is stable to variance increase during given
#' period. This test ignores the dependence of abnormal returns' ranks of
#' different days (i.e., a serial dependence). The critical values are standard
#' normal. The significance levels of \eqn{\alpha} are 0.1, 0.05, and 0.01
#' (marked respectively by *, **, and ***).
#'
#' @param list_of_returns a list of objects of S3 class \code{returns}, each
#' element of which is treated as a security.
#' @param car_start an object of \code{Date} class giving the first date of
#' the CAR period.
#' @param car_end an object of \code{Date} class giving the last date of the
#' CAR period.
#' @param percentage a lowest allowed percentage of non-missing observation
#' for each day to be incorporated into CAR. The default value is 90 percent.
#' \itemize{
#'     \item \code{name}: a name of the test, i.e.
#'     \code{"car_brown_warner_1985"}
#'     \item \code{car_start}: the first date of the CAR period
#'     \item \code{car_end}: the last date of the CAR period
#'     \item \code{average_percentage}: an average share of non-missing
#'           observations over the CAR period
#'     \item \code{statistic}: a test's statistic
#'     \item \code{number_of_days}: the number of days in the CAR period
#'     \item \code{significance}: a significance of the statistic
#' }
#'
#' @references \itemize{
#' \item Corrado C.J. \emph{A Nonparametric Test for Abnormal Security-Price
#' Performance in Event Studies}. Journal of Financial Economics 23:385-395,
#' 1989.
#' \item Cowan A.R. \emph{Nonparametric Event Study Tests}. Review of
#' Quantitative Finance and Accounting, 2:343-358, 1992.
#' }
#'
#' @seealso \code{\link{car_nonparametric_tests}}.
#'
#' @examples
#' \dontrun{
#' library("magrittr")
#' rates_indx <- get_prices_from_tickers("^N100",
#'                                       start = as.Date("2000-01-01"),
#'                                       end = as.Date("2002-01-01"),
#'                                       quote = "Close",
#'                                       retclass = "zoo") %>%
#'     get_rates_from_prices(quote = "Close",
#'                           multi_day = TRUE,
#'                           compounding = "continuous")
#' tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE",
#'              "TOP.CO")
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
#'     car_rank_test(car_start = as.Date("2001-09-11"),
#'                   car_end = as.Date("2001-09-28"))
#' }
#' ## The result of the code above is equivalent to:
#' data(securities_returns)
#' car_rank_test(
#'     list_of_returns = securities_returns,
#'     car_start = as.Date("2001-09-11"),
#'     car_end = as.Date("2001-09-28")
#' )
#'
#' @export
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
