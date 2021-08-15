library("magrittr")

rates_indx <- get_prices_from_tickers("^N100",
                                      start = as.Date("2000-01-01"),
                                      end = as.Date("2002-01-01"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous")


usethis::use_data(rates_indx, overwrite = TRUE)
