library("magrittr")

rates_indx <- get_prices_from_tickers("^GSPC",
                                      start = as.Date("2019-04-01"),
                                      end = as.Date("2020-04-01"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous")


usethis::use_data(rates_indx, overwrite = TRUE)
