library("magrittr")

rates_indx <- get_prices_from_tickers("^N100",
                                      start = as.Date("2000-01-01"),
                                      end = as.Date("2002-01-01"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous")

tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE", "TOP.CO")

securities_returns <- get_prices_from_tickers(tickers,
                                              start = as.Date("2000-01-01"),
                                              end = as.Date("2002-01-01"),
                                              quote = "Close",
                                              retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous") %>%
    apply_market_model(regressor = rates_indx,
                       same_regressor_for_all = TRUE,
                       market_model = "sim",
                       estimation_method = "ols",
                       estimation_start = as.Date("2001-03-26"),
                       estimation_end = as.Date("2001-09-10"))

usethis::use_data(securities_returns, overwrite = TRUE)
