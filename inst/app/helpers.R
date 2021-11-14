download_prices <- function(ticker, start, end, quote, retclass) {
    tryCatch(
        expr = {
            get_prices_from_tickers(
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

sign_formatter <- formattable::formatter(
    "span",
    style = x ~ formattable::style(
        color = dplyr::if_else(
            x > 0, "green", dplyr::if_else(x < 0, "red", "#869496")
        )
    )
)

beautify <- function(tests_table) {

    # beautify column names
    names(tests_table) <- nice_colnames[names(tests_table)]

    column_formatters <- list(
        Percent = formattable::color_bar("lightgreen")
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
