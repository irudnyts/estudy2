library("magrittr")

tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE", "TOP.CO")

rates <- tickers %>%
    get_prices_from_tickers(start = as.Date("2000-01-01"),
                            end = as.Date("2002-01-01"),
                            quote = "Close",
                            retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous")

usethis::use_data(rates, overwrite = TRUE)
