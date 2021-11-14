library("magrittr")

tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")

prices <- tickers %>%
    get_prices_from_tickers(start = as.Date("2019-04-01"),
                            end = as.Date("2020-04-01"),
                            quote = "Close",
                            retclass = "zoo")

usethis::use_data(prices, overwrite = TRUE)
