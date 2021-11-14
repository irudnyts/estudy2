library("magrittr")

prices_indx <- get_prices_from_tickers("^GSPC",
                                       start = as.Date("2019-04-01"),
                                       end = as.Date("2020-04-01"),
                                       quote = "Close",
                                       retclass = "zoo")

usethis::use_data(prices_indx, overwrite = TRUE)
