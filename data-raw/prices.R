library("magrittr")

tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE", "TOP.CO")

prices <- tickers %>%
    get_prices_from_tickers(start = as.Date("2000-01-01"),
                            end = as.Date("2002-01-01"),
                            quote = "Close",
                            retclass = "zoo")

usethis::use_data(prices, overwrite = TRUE)
