library("magrittr")

tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")

rates <- tickers %>%
    get_prices_from_tickers(start = as.Date("2019-04-01"),
                            end = as.Date("2020-04-01"),
                            quote = "Close",
                            retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous")

usethis::use_data(rates, overwrite = TRUE)
