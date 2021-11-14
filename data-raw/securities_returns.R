library("magrittr")

rates_indx <- get_prices_from_tickers("^GSPC",
                                      start = as.Date("2019-04-01"),
                                      end = as.Date("2020-04-01"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous")

tickers <- c("AMZN", "ZM", "UBER", "NFLX", "SHOP", "FB", "UPWK")

securities_returns <- get_prices_from_tickers(tickers,
                                              start = as.Date("2019-04-01"),
                                              end = as.Date("2020-04-01"),
                                              quote = "Close",
                                              retclass = "zoo") %>%
    get_rates_from_prices(quote = "Close",
                          multi_day = TRUE,
                          compounding = "continuous") %>%
    apply_market_model(regressor = rates_indx,
                       same_regressor_for_all = TRUE,
                       market_model = "sim",
                       estimation_method = "ols",
                       estimation_start = as.Date("2019-04-01"),
                       estimation_end = as.Date("2020-03-13"))

usethis::use_data(securities_returns, overwrite = TRUE)
