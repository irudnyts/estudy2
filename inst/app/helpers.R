#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' Brutally imported from \code{magrittr} package, since otherwise
#' would require \code{library(magrittr)} when the app is deployed as-is.
`%>%` <- magrittr::`%>%`

#' Download prices given a ticker
#'
#' The function uses \code{get_prices_from_tickers()} from the package to
#' download prices. However, instead of throwing an error if the ticker is
#' not valid, it returns \code{NULL}.
#'
#' The function does not return \code{NULL} in case of the warining.
download_prices <- function(ticker, start, end, quote, retclass) {
    tryCatch(
        expr = {
            estudy2::get_prices_from_tickers(
                ticker,
                start = start,
                end = end,
                quote = quote,
                retclass = retclass
            )
        },
        error = function(cond) {
            return(NULL)
        }
    )
}

#' Named vector with nice column names
#'
#' Each element of the vector is a nice column name that has a corresponding
#' name from the data frames that returned by \code{parametric_tests()} and
#' \code{nonparametric_tests()}.
#'
#' All labels are in title case.
nice_colnames <- c(
    "date" = "Date",
    "weekday" = "Weekday",
    "percentage" = "Percent",
    "mean" = "Mean",
    "bw_1980_stat" = "BW1980 Stat.",
    "bw_1980_signif" = "BW1980 Signif.",
    "bw_1985_stat" = "BW1985 Stat.",
    "bw_1985_signif" = "BW1985 Signif.",
    "t_test_stat" = "t-test Stat.",
    "t_test_signif" = "t-test Signif.",
    "pt_stat" = "Patell's Stat.",
    "pt_signif" = "Patell's Signif.",
    "bh_stat" = "BMP Stat.",
    "bh_signif" = "BMP Signif.",
    "lmb_stat" = "Lamb's Stat.",
    "lmb_signif" = "Lamb's Signif.",
    "sign_stat" = "Sign Stat.",
    "sign_signif" = "Sign Signif.",
    "gsign_stat" = "G. Sign Stat.",
    "gsign_signif" = "G. Sign Signif.",
    "csign_stat" = "C. Sign Stat.",
    "csign_signif" = "C. Sign Signif.",
    "rank_stat" = "Rank Stat.",
    "rank_signif" = "Rank Signif.",
    "mrank_stat" = "M. Rank Stat.",
    "mrank_signif" = "M. Rank Signif.",
    "wlcx_stat" = "Wilcoxon's Stat.",
    "wlcx_signif" = "Wilcoxon's Signif."
)

#' Highlight values depending on their sign
#'
#' An auxiliary function that highlights values below zero into red, above --
#' into green, and zero values into grey (default from \code{bslib}'s
#'  \code{solar} theme).
#'
#'  The function heavily relies to \code{formattable}.
sign_formatter <- formattable::formatter(
    "span",
    style = x ~ formattable::style(
        color = dplyr::if_else(
            x > 0, "green", dplyr::if_else(x < 0, "red", "#869496")
        )
    )
)

#' Beautify the result of \code{parametric_tests()} and
#' \code{nonparametric_tests()}
#'
#' The function beautifies the column names, formats percent columns, rounds
#' to the second digit, and replaces asterisks by emojis' stars.
beautify <- function(tests_table) {

    # beautify column names
    names(tests_table) <- nice_colnames[names(tests_table)]

    # Define formatters for various columns
    column_formatters <- list(
        Percent = formattable::color_bar(
            "lightgreen", fun = formattable::proportion
        )
    )

    if (any(names(tests_table) == "Mean"))
        column_formatters[["Mean"]] <- sign_formatter

    tests_table %>%
        dplyr::mutate(
            dplyr::across(
                dplyr::contains("Stat") | dplyr::contains("Mean"),
                round,
                2
            )
        ) %>%
        dplyr::mutate(
            dplyr::across(
                dplyr::contains("Signif"),
                stringr::str_replace_all, stringr::fixed("*"), emo::ji("star")
            )
        ) %>%
        dplyr::mutate(
            Percent = formattable::percent(Percent / 100, digits = 0),
            Date = format(Date, "%b %d")
        ) %>%
        dplyr::mutate(
            dplyr::across(dplyr::contains("Mean"), formattable::percent)
        ) %>%
        formattable::formattable(
            column_formatters
        )
}
