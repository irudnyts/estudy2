# rm(list = ls())
library("devtools")
# devtools::install_github("irudnyts/estudy")
library("estudy")
library("zoo")
library("tseries")
library("estudy2")
source("/Users/yegor/Documents/Projects/estudy2/R/apply_market_model.R")
source("/Users/yegor/Documents/Projects/estudy2/R/constants.R")
source("/Users/yegor/Documents/Projects/estudy2/R/get_rates_from_prices.R")
source("/Users/yegor/Documents/Projects/estudy2/R/parametric_tests.R")


event.date <- as.Date("11.09.2001","%d.%m.%Y")

# reading prices of companies
setwd(paste("/Users/yegor/Documents/Education/UNIL/UNIL, 5.2/Master thesis/",
            "sources/data/eu_companies", sep = ""))

for(i in seq_along(list.files())) {
    current.df <- read.csv(list.files()[i])[, c("Date", "Open")]
    current.df[, "Date"] <- as.Date(as.character(current.df[, "Date"]))
    colnames(current.df)[2] <- gsub(".csv", "", list.files()[i])
    if(i == 1) {
        companies.prices <- current.df
    } else {
        companies.prices <- merge(x = companies.prices, y = current.df,
                                  by.x = "Date", by.y = "Date",all = TRUE,
                                  sort = FALSE)
    }
}

# reading indecis
setwd(paste("/Users/yegor/Documents/Education/UNIL/UNIL, 5.2/Master thesis/",
            "sources/data/indecies/", sep = ""))
index.name <- "SXW1INSE"
index.price <- read.table(paste(index.name, ".txt", sep = ""),
                          header = T, sep = ";")[, c("Date", "Indexvalue")]
index.price[, "Date"] <- as.Date(as.character(index.price[, "Date"]),
                                 "%d.%m.%Y")
colnames(index.price)[2] <- index.name

prices <- merge(x = companies.prices, y = index.price, by.x = "Date",
                by.y = "Date", all.x = T, all.y = F, sort = F)
prices <- prices[order(prices[, "Date"]), ]
prices <- CleanSeries.data.frame(prices)
colnames(prices)[1] <- "date"
rates <- GetRatesOpenPrices(prices = prices)
colnames(rates)[1] <- "date"

result.table <- CheckEventDaily.data.frame(rates = rates,
                                           index.ticker = index.name,
                                           event.date = event.date,
                                           w.b = 1, w.a = 8, delta = 120)

rates.period <- rates[(334 - 1 - 120):(334 + 8), ]
data <-
    GetOLSAbnormals(rates = rates.period, index.ticker = index.name,
                    delta = 120)


rates_zoo <- zoo(rates[, -c(1, 12)], rates[, 1])
index_zoo <- zoo(rates[, 12], rates[, 1])

returns <- list()
for(i in 1:ncol(rates_zoo)) {
    returns[[i]] <- apply_market_model(rates = rates_zoo[, i], regressor = index_zoo, market_model = "sim", estimation_method = "ols", estimation_start = as.Date("2001-03-05"), estimation_end = as.Date("2001-09-07"))
}


#-------------------------------------------------------------------------------
prices_list <- list()
for(i in 2:ncol(prices)) {
    prices_list[[i - 1]] <- zoo(prices[, i], prices[, 1])
}
index_prices <- zoo(index.price$SXW1INSE, index.price$Date)


rates_list <- list()
for(i in seq_along(prices_list)) {
    rates_list[[i]] <- get_rates_from_prices(prices = prices_list[[i]], quote = "Open", compounding = "discrete")
}
index_rates <- get_rates_from_prices(prices = index_prices, quote = "Open", compounding = "discrete")

list_of_returns <- list()
for(i in seq_along(rates_list)) {
    list_of_returns[[i]] <- apply_market_model(rates = rates_list[[i]], regressor = index_rates, market_model = "sim", estimation_method = "ols", estimation_start = as.Date("2001-03-05"), estimation_end = as.Date("2001-09-21"))
}



prices1 <- zoo(prices[, 2], prices[, 1])
rates1 <- get_rates_from_prices(prices = prices1, quote = "Open", compounding = "discrete")


allianz <- apply_market_model(rates = zoo(rates[, 2], rates[, 1]),
                   regressor = zoo(rates[, 12], rates[, 1]), market_model = "sim", estimation_method = "ols",
                   estimation_start = as.Date("2001-03-05"), estimation_end = as.Date("2001-09-09"))
axa <-  apply_market_model(rates = zoo(rates[, 3], rates[, 1]),
                           regressor = zoo(rates[, 12], rates[, 1]), market_model = "sim", estimation_method = "ols",
                           estimation_start = as.Date("2001-03-05"), estimation_end = as.Date("2001-09-09"))
generali <-  apply_market_model(rates = zoo(rates[, 6], rates[, 1]),
                           regressor = zoo(rates[, 12], rates[, 1]), market_model = "sim", estimation_method = "ols",
                           estimation_start = as.Date("2001-03-05"), estimation_end = as.Date("2001-09-09"))






# axa <-  apply_market_model(rates = zoo(rates[, 3], rates[, 1]),
#                            regressor = zoo(rates[, 12], rates[, 1]), market_model = "sim", estimation_method = "ols",
#                            estimation_start = as.Date("2001-03-05"), estimation_end = as.Date("2001-09-09"))

brown_warner_1980(list_of_returns = list(allianz, axa), event_start = as.Date("2001-09-10"), event_end = as.Date("2001-09-21"))
brown_warner_1985(list_of_returns = list(allianz, axa), event_start = as.Date("2001-09-10"), event_end = as.Date("2001-09-21"))
t_test(list_of_returns = list(allianz, axa), event_start = as.Date("2001-09-10"), event_end = as.Date("2001-09-21"))
patell(list_of_returns = list(allianz, axa), event_start = as.Date("2001-09-10"), event_end = as.Date("2001-09-21"))
boehmer(list_of_returns = list(allianz, axa), event_start = as.Date("2001-09-10"), event_end = as.Date("2001-09-21"))

parametric_tests(list_of_returns = list(allianz, axa), event_start = as.Date("2001-09-10"), event_end = as.Date("2001-09-21"), tests = list(boehmer, brown_warner_1980, brown_warner_1985), all = F)

CheckEventDaily.data.frame(rates = rates[,c(1,2,3,12)], index.ticker = index.name, event.date = as.Date("2001-09-11"), w.b = 1, w.a = 8, delta = 120)


#a <- merge(rates1[time(rates1) >= as.Date("2001-03-05") & time(rates1) <= as.Date("2001-09-09")],
#       index_rates[time(index_rates) >= as.Date("2001-03-05") & time(index_rates) <= as.Date("2001-09-09")])
#names(a) <- c("a", "b")


