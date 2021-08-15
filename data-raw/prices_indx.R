library("magrittr")

prices_indx <- get_prices_from_tickers("^N100",
                                       start = as.Date("2000-01-01"),
                                       end = as.Date("2002-01-01"),
                                       quote = "Close",
                                       retclass = "zoo")

usethis::use_data(prices_indx, overwrite = TRUE)
