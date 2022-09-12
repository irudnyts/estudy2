# SCRIPT OUTCOMES: ####
# A. Create tests in generalized format
# B. Create GLS pipeline in generalized format

# DATA WRANGLING: CLEANING ####
# 1. User defined functions and package (library) imports ####
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

library(estudy2)
library(magrittr)

fun_insert <- function(x, pos, insert)
{
    # Create own function
    gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x)
}

fix_digit_names <- function(x, insertion, pos_idx = 0)
{
    # NOTE: Need to assign it to the x variable e.g. x <- tf_fixer(x, insertion)
    fun_insert <- function(x, pos, insert)
    {
        # Function inserts 'insertion' argument at the 0 position
        gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x)
    }

    test_vec <- grepl("^[[:digit:]+]", x)
    for (i in 1:length(x))
    {
        if (test_vec[i] == TRUE)
        {
            x[i] <- fun_insert(x[i], pos_idx, as.character(insertion))
        }
    }
    return(x)
}

name_as_string <- function(x)
{
    # Returns the name of whatever you pass as a character string
    # Primary purpose is to get string representation of function and variable names
    # Added benefit is anything passed as x will come out as 'x'
    deparse(substitute(x))
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")
# 2. Data Loading #####
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/"
files <- c("stock_data_column-wise.csv", "market-index_data_column-wise.csv")

# Join the strings together (static = simpler in R, but wanted to practice looping over txt)
cds <- list()

for (i in files)
{
    string <- paste(cd, i, sep = "")
    # print(string)
    cds <- append(cds, string, )
}

# Read in data No df1 because that would be the 3rd file containing everything in df2 and df3
df2 <- read.csv(as.character(cds[1]))
df3 <- read.csv(as.character(cds[2]))

# Remove duplicate index
df2 = subset(df2, select = -c(X))
df3 = subset(df3, select = -c(X))

# Rename 'Date' var into 'date'
names(df2)[names(df2) == "Date"] <- "date"
names(df3)[names(df3) == "Date"] <- "date"

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 3. Basic wrangling of data (slicing data-set out of bulk, reformatting of dtypes) ####

print("Reformatting data.shape...")
start_time <- Sys.time()

# Reformatting 'date's into Date-class
df2$date <- as.Date(df2$date, format = "%Y-%m-%d")
df3$date <- as.Date(df3$date, format = "%Y-%m-%d")

# Index by dates
rownames(df2) <- as.Date(df2$date, format = "%Y-%m-%d")
rownames(df3) <- as.Date(df3$date, format = "%Y-%m-%d")

# Subset data.frames to the right size of usable data
df2 <- subset(df2, rownames(df2) > as.Date("2019-03-29"))
df3 <- subset(df3, rownames(df3) > as.Date("2019-03-29"))

df2 <- subset(df2, rownames(df2) < as.Date("2020-05-01"))
df3 <- subset(df3, rownames(df3) < as.Date("2020-05-01"))

# Store 'date' column
dates2 <- c(as.Date(df2$date))
dates3 <- c(as.Date(df3$date))

# Create new data.frames without 'date' cols since index by 'date'
df2 <- subset(df2, select = -date)
df3 <- subset(df3, select = -date)

# Remove remaining rows where all data points = NaN
df2 <- df2[rowSums(is.na(df2)) != ncol(df2), ]
df3 <- df3[rowSums(is.na(df3)) != ncol(df3), ]
# NOTE: Does not remove all remaining NaNs because of 'date' col have not tested if the above two
# lines can be removed without effects

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 4. Wrangling of Identification (strings) data ##################
cat("Fetching variable identification data.", "\n", "Creating variables. Reformatting for r syntax...")
start_time <- Sys.time()

cd_dict = "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/index_member_dict_fixed.xlsx"
cd_problem_stocks = "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/problem_id_s.txt"
name_dict = readxl::read_xlsx(cd_dict)
# read in fully formatted names of problematic stock series
problem_names = read.delim(cd_problem_stocks, header = FALSE)
problem_names <- unlist(problem_names, use.names = FALSE)

# Get colnames
keys <- colnames(name_dict)
# Replace spaces with '.' to be able to select columns from data.frame
keys <- as.vector(gsub(" ", ".", keys))

# Initialise empty lists with required length indice_list <- vector(mode = 'list', length = n)
stock_list <- vector(mode = "list", length = length(df3))
market_list <- vector(mode = "list", length = length(df3))

# Swap numerical index for named index --> provides similarity to a python dictionary
names(market_list) <- keys
names(stock_list) <- keys

# Specify patterns
pattern_list <- c(" ", "/", "-", "\\*", "&", ",")

# Break ID data.frame into list to remove NANs
name_list <- as.list(name_dict)
for (i in 1:3)
{
    name_list <- lapply(name_list, stringr::str_replace_all, pattern = paste0(pattern_list, collapse = "|"),
                        replacement = ".")
    name_list <- lapply(name_list, na.omit)
    name_list <- lapply(name_list, fix_digit_names, "X")

    # Make syntactically compatible
    names(name_list) <- lapply(names(name_list), gsub, pattern = " ", replacement = ".")
}

original_number_of_stocks <- length(unlist(name_list))
long_name_list <- unlist(name_list)
long_name_list_mindu <- name_list[(names(name_list) != "INDU.Index") & (names(name_list) !="CAC.Index")] %>% unlist

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# Check for duplicates
if (anyDuplicated(long_name_list_mindu) != 0) {
    dupe_vec <- long_name_list_mindu %>% duplicated
    cat("There are", sum(as.numeric(dupe_vec)), "duplicate names.")
    dupe_locs <- which(dupe_vec == TRUE)
    ori_vec <- long_name_list_mindu %>% duplicated(fromLast = TRUE)
    ori_locs <- which(ori_vec == TRUE)
    cat("\n", dupe_locs)
} else {
    cat("No duplicates found in", length(long_name_list_mindu), "items")
}
# long_name_list[dupe_locs]
# long_name_list[ori_locs]
# 5. Creation of list of market-index data.frames ##############################

print("Making list of market data.frame")
start_time <- Sys.time()

for (i in 1:length(keys))
{
    # Select as.data.frame the market index
    temp_df <- as.data.frame(df3[, keys[[i]], drop = FALSE])
    temp_df <- cbind(date = dates2, temp_df)

    market_list[[keys[[i]]]] <- temp_df

    rm(temp_df)
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")
# 6. Creation of list of stock-index data.frames consisting of constituent-shares ####
print("Begining 'data.frame 2' (df2) slicing process.")
start_time <- Sys.time()

for (i in 1:length(keys))
{
    tryCatch({
        print(i)
        print(keys[[i]])
        # gets names corresponding to key: gets constituents of index
        str_vec <- unlist(name_list[[keys[[i]]]], use.names = FALSE)

        # selects index by their names
        stock_series <- df2[str_vec]
        stock_series <- cbind(date = dates2, stock_series)
        # stock_series <- subset(df2, select=str_vec) #df2 = subset(df2, select = -c(X))

        # remove NA rows from data.frame
        stock_series <- stock_series[rowSums(is.na(stock_series)) != ncol(stock_series), ]

        # stores data in dictionary of index constituent pd.DataFrame --> Index name is key
        stock_list[[keys[[i]]]] <- stock_series

        # explicit memory clean-up
        rm(stock_series)
    }, error = function(e)
    {
        message(cat("ERROR: ", conditionMessage(e), "\n"))
    })
}
# NOTE: THERE ARE STILL NANs PRESENT IN THE ORGANISED DATASET
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 7. Removal of remaining NAs #####
# Process first drops rows where all observations are NAs (public holidays, etc.)
# Process then drops remaining columns that still include AT LEAST 1 NA
cat("Removing NANs for data.frames in lists:", "\n", "1. ", name_as_string(stock_list), "\n", "2. ",
    name_as_string(market_list), "\n")
start_time <- Sys.time()

# Copy lists of data.frames for verification purposes
market_list_copy <- market_list
stock_list_copy <- stock_list

# 'NA' CLEANING LOOPS for the MARKET-data
for (i in 1:length(market_list))
{
    # selects data.frame in list that is to be manipulated on this iter INPUT
    selector <- market_list[[i]]
    # Make copy of data column in order to handle N*2 data.frames
    selector$copy = selector[[ncol(selector)]]
    # drop the 'date' column
    selector <- subset(selector, select = -c(date))
    # remove rows where all observations = 'NA'
    selector <- selector[rowSums(is.na(selector)) != ncol(selector), ]
    # remove columns where at least 1 observation = 'NA'
    selector <- selector[, colSums(is.na(selector)) == 0]
    # add back the date column
    selector <- tibble::rownames_to_column(selector)
    selector <- dplyr::rename(selector, date = rowname)
    rownames(selector) <- selector$date
    # remove the 'copy' column as it is no longer needed
    selector <- subset(selector, select = -c(copy))
    # return cleaned data.frame to list of data.frames OUTPUT
    market_list[[i]] <- selector
    rm(selector)
}
# for the STOCKS (market-constituient) data
for (i in 1:length(stock_list))
{
    # selects data.frame in list that is to be manipulated on this iter INPUT
    selector <- stock_list[[i]]
    # drop the 'date' column
    selector <- subset(selector, select = -c(date))
    # remove rows where all observations = 'NA'
    selector <- selector[rowSums(is.na(selector)) != ncol(selector), ]
    # remove columns where at least 1 observation = 'NA'
    selector <- selector[, colSums(is.na(selector)) == 0]
    # add back the date column
    selector <- tibble::rownames_to_column(selector)
    selector <- dplyr::rename(selector, date = rowname)
    rownames(selector) <- selector$date
    # return cleaned data.frame to list of data.frames OUTPUT
    stock_list[[i]] <- selector
    rm(selector)
}

# CHECK CLEANING HAPPENED
if ((identical(market_list, market_list_copy) == TRUE) & (identical(stock_list, stock_list_copy) == TRUE))
{
    message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(market_list)),
            "\n", "2. ", name_as_string(stock_list))
} else if (identical(market_list, market_list_copy) == TRUE)
{
    message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(market_list)))
} else if (identical(stock_list, stock_list_copy) == TRUE)
{
    message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(stock_list)))
} else
{
    message(cat("NANs have been removed from the following lists", "\n", "1. ", name_as_string(market_list),
                "\n", "2. ", name_as_string(stock_list)))
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")




# A. Create tests in generalized format ####
# 1. Setup for dev ####
# REMOVE UNNCESSARY ITEMS FROM ENVIRONMENT
rm(cds,df2,df3,market_list_copy,stock_list_copy,name_dict)

# useful dev tool:
# CHECK TYPES OF OBJECT
check_types <- function(object) {
    cat("OBJECT NAME:", deparse(substitute(object)), "\n")
    cat("typeof:", typeof(object), "\n")
    cat("class:", class(object), "\n")
    cat("mode:", mode(object), "\n")
    cat("length:", length(object), "\n")
    cat("number of dimensions:", length(dim(object)), "\n")
    cat("dimensions:", dim(object),  "\n")
}

# load libraries
library(estudy2)
library(lmtest)
library(nlme)


# cut out sample from data and allocate to correct variables
all_dependents <- stock_list$MEXBOL.Index
dependent <- all_dependents$AMXL.MF.Equity %>%
    as.data.frame(
        row.names = all_dependents$date,
        col.names = "AMXL.MF.Equity"
        )
names(dependent) <- "AMXL.MF.Equity"
dependent <- cbind(date = as.Date(rownames(dependent)), dependent)
rates <- estudy2::get_rates_from_prices(dependent,
                                      quote = "Close",
                                      multi_day = TRUE,
                                      compounding = "continuous")

independent <- market_list$MEXBOL.Index
independent$date <- independent$date %>% as.Date
regressor <- estudy2::get_rates_from_prices(independent,
                                        quote = "Close",
                                        multi_day = TRUE,
                                        compounding = "continuous")

# arguments that are usually specified elsewhere
same_regressor_for_all = TRUE
market_model = "sim"
estimation_method = "ols"
estimation_start = as.Date("2019-04-01")
estimation_end = as.Date("2020-03-13")


data <- merge(rates, regressor, by = "date", all = TRUE)
estimation_data <- data[stats::complete.cases(data), ]
estimation_data <- estimation_data[
    estimation_data[, 1] >= estimation_start &
    estimation_data[, 1] <= estimation_end, ]
delta <- nrow(estimation_data)

# two variables created, because predict is looking for the same
# as in lm variables names
y <- estimation_data[, 2]
x <- estimation_data[, 3]
lm_fit <- stats::lm(y ~ x)
predicted <- stats::predict.lm(
    object = lm_fit,
    newdata = data.frame(x = data[, 3]),
    interval = c("confidence"),
    level = 0.95
)

# 2. Creation of "predict.gls()" function ####
# Prediction with GLS
predict.gls <- function(gls_model, newdata, level = 0.95) {
    # obtain the model's coefficients
    model_coeff <- nlme::intervals(gls_model[[1]], level = level)[[1]]

    lwr_intercept <- model_coeff[1,1]
    fit_intercept <- model_coeff[1,2]
    upr_intercept <- model_coeff[1,3]

    lwr_beta <- model_coeff[2,1]
    fit_beta <- model_coeff[2,2]
    upr_beta <- model_coeff[2,3]

    # calculate results, where 'newdata' is the independent variable
    Y_lwr <- lwr_intercept + (lwr_beta * newdata)
    Y_fit <- fit_intercept + (fit_beta * newdata)
    Y_upr <- upr_intercept + (upr_beta * newdata)

    res <- as.matrix(
        as.data.frame(list(Y_fit,Y_lwr,Y_upr),
                      col.names=c("fit","lwr","upr")
        )
    )
    return(res)
}

# 3. Tests for GLS ####

# Run tests
significance <- 0.01

bp_test <- lmtest::bptest(lm_fit)
bg_test1 <- lmtest::bgtest(lm_fit, order = 1)

scedas <- lmtest::bptest(lm_fit)["p.value"] < significance
autocorr <- lmtest::bgtest(lm_fit, order = 1)["p.value"] < significance

# 4. Creation of GLS model specifying function ####

make_gls <- function(d = estimation_data, ar.res=NULL) {
    # copy for heteroscedasticity correction in the gls models
    d <- estimation_data
    # add an extra feature that allows for different variance structures over time
    names(d) <- c("date","y","x")
    d['month'] <- format(d$date,"%m")

    # specify possible different variance structures
    vf1 <- nlme::varFixed(~x)
    vf2 <- nlme::varIdent(form = ~ x | month)
    vf3 <- nlme::varPower(form = ~ x)
    vf4 <- nlme::varPower(form = ~ x | month)
    vf5 <- nlme::varConstPower(form = ~ x)
    vf6 <- nlme::varConstPower(form = ~ x | month)
    vf7 <- nlme::varExp(form = ~x)

    # specify possible models
    lm_gls <- try(nlme::gls(y ~ x, data = d), silent = TRUE)
    vf1_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf1), silent = TRUE)
    vf2_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf2), silent = TRUE)
    vf3_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf3), silent = TRUE)
    vf4_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf4), silent = TRUE)
    vf5_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf5), silent = TRUE)
    vf6_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf6), silent = TRUE)
    vf7_gls <- try(nlme::gls(y ~ x, data = d, correlation = ar.res, weights = vf7), silent = TRUE)

    # Find & remove all models that run into errors
    reg_res <- list(lm_gls, vf1_gls, vf2_gls, vf3_gls, vf4_gls, vf5_gls, vf6_gls, vf7_gls)
    reg_res_logical <- lapply(reg_res, function (a) return(class(a) != "gls"))
    if (any(reg_res_logical == TRUE) == TRUE){
        regs <- reg_res[-which((reg_res_logical == TRUE))]
    }

    # Select best fitting model according to AIC criterion
    aic <- sapply(regs, AIC)
    gls_model <- regs[-which((aic != min(aic))==TRUE)]
    aic_logical <- aic == min(aic)

    return(gls_model)
}

# 5. Statement ensures correct model specification of GLS ####

scedas <- TRUE
autocorr <- TRUE

# specify remedies
if ((scedas == TRUE) & (autocorr == TRUE)) {
    # model has both heteroscedasticity and autocorrelation
    # Calc autocorrelation of residuals
    corr <- stats::acf(lm_fit$residuals,
                       type = "correlation",
                       plot = FALSE)
    ARcorr <- nlme::corAR1(value = corr$acf[[2]],
                         form = ~1,
                         fixed = FALSE)
    print("correcting for autocorr & heteroscedas...")
    gls_fit <- make_gls(ar.res = ARcorr)

    predicted <- predict.gls(
        gls_model = gls_fit,
        newdata = data[,3],
        level = 0.95
    )
} else if ((scedas == TRUE) & (autocorr == FALSE)) {
    # model has heteroscedasticity but no autocorrelation
    # Fit gls corrected for heteroscedasticity
    gls_fit <- make_gls(ar.res = NULL)

    predicted <- predict.gls(
        gls_model = gls_fit,
        newdata = data[,3],
        level = 0.95
    )
} else if ((scedas == FALSE) & (autocorr == TRUE)) {
    # model has autocorrelation but no heteroscedasticity
    # Calcl autocorrelation of residuals
    corr <- stats::acf(lm_fit$residuals,
                       type = "correlation",
                       plot = FALSE)
    ARcorr <- nlme::corAR1(value = corr$acf[[2]],
                           form = ~1,
                           fixed = FALSE)

    gls_fit <- nlme::gls(y ~ x,
                         correlation = ARcorr,
                         weights = NULL)

    predicted <- predict.gls(
        gls_model = gls_fit,
        newdata = data[,3],
        level = 0.95
    )
} else {
    # if no tests TRUE then OLS model remains OLS
    predicted <- predicted
}

# Attempt to strip out info of GLS and put it in an LM object
lm_gls <- lm_fit

lm_gls$coefficients <- gls_fit[[1]]$coefficients
lm_gls$residuals <- gls_fit[[1]]$residuals
lm_gls$effects <- NULL # want to empty unused args to test if they are used in tests later

# lm_gls$rank # not relevant to change
lm_gls$fitted.values <- gls_fit[[1]]$fitted
# lm_gls$assign # not relevant to change

# lm_gls$qr <-  NULL # want to empty unused args to test if they are used in tests later
# lm_gls$df.residual # not relevant to replace
# lm_gls$xlevels # not relevant to replace

# lm_gls$call # not relevant to replace
lm_gls$terms <- gls_fit[[1]]$terms
lm_gls$model <- NULL # want to empty unused args to test if they are used in tests later

#nlme::predict.gls
# predicted_gls <- predict.lm(
#     object = lm_gls,
#     newdata = data.frame(x = data[, 3]),
#     interval = c("confidence"),
#     level = 0.95
# )


rownames(predicted) <- NULL
result <- list(
    observed = zoo::zoo(data[, 2], data[, 1]),
    predicted = zoo::zoo(predicted[, 1], data[, 1]),
    lower95CI = zoo::zoo(predicted[, 2], data[, 1]),
    upper95CI = zoo::zoo(predicted[, 3], data[, 1]),
    abnormal = zoo::zoo(data[, 2], data[, 1]) -
        zoo::zoo(predicted[, 1], data[, 1]),
    regressor = zoo::zoo(data[, 3], data[, 1]),
    market_model = market_model,
    full_name_market_model = "Single-Index Market Model",
    estimation_method = estimation_method,
    full_name_estimation_method =
        "Ordinary Least Squares",
    coefficients = c(
        alpha =
            unname(lm_fit$coefficients)[1],
        beta =
            unname(lm_fit$coefficients)[2]
    ),
    estimation_start = estimation_start,
    estimation_end = estimation_end,
    estimation_length = delta
)

