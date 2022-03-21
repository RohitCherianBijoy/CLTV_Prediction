install.packages('devtools')
library(fpp3)
library(readxl)
library(devtools)
library(sm)
library(varhandle)
library(lubridate)
library(forecast)

getwd()

ol_df_0910 <-
  read_excel("online_retail_II.xlsx", sheet = "Year 2009-2010") %>%
  rename(id = `Customer ID`)

df_clean <- ol_df_0910 %>%
  filter((is.na(id) == FALSE) &  # Keep not null id
           (Quantity > 0) &  # Keep Valid Quantity
           (Price > 0) & # Keep valid price
           (substr(
             toupper(Invoice), nchar(Invoice), nchar(Invoice)
           ) != 'C') & # Keep invoice without C as last character
           (check.numeric(StockCode) == TRUE)) %>% mutate(clean_date = as.Date(InvoiceDate, "%Y-%m-%d")
                                                          , tot_ord = Quantity*Price)

cust_clust <- read.csv('train.csv')

#Add customer cluster segment information to raw dataframe

df_cust <- merge(df_clean, cust_clust[ , c("id", "Segment")], by = 'id')

#Create Time Series
# 1. Extract Week Number from data
# 2. Convert to TS object
# 3. Observe TS components
# 4. Experiment with TS models

#1. Setting up for TS data

df_cust$week_num <- week(df_cust$clean_date)

#Create week level aggregations for every customer cluster in the data set
df_cust_wk <- df_cust %>% group_by(week_num,Segment) %>% summarise(seg_ord_val = sum(tot_ord),seg_ord = n_distinct(Invoice)) %>% mutate(seg_avg_ord_val = seg_ord_val/seg_ord)

#Performing TS for Promising Customers
promising_cust <- df_cust_wk[df_cust_wk$Segment == 'Promising',]
#Convert DF to TSIBBLE

promising_cust_ts <- ts(promising_cust[,5],frequency = 365.25 / 7,start = 2010)
promising_cust_ts <- as_tsibble(promising_cust_ts)

names(promising_cust_ts)[2] <- "seg_avg_ord_val"

str(promising_cust_ts)

#promising_cust_ts <- promising_cust_ts %>% fill_gaps(seg_avg_ord_val = NA)
#Visualize Data
autoplot(promising_cust_ts)
#No significant autocorrelation exists, plot points to some seasonality
autoplot(ACF(promising_cust_ts, seg_avg_ord_val, lag_max = 20))

#Exponential Smoothing - Data doesnt really have clear trend or seasonality
fit_1  <- promising_cust_ts %>% model(ETS(seg_avg_ord_val))
#Model: ETS(A,N,N) validates our earlier finding that data does not have much seasonality/trend
aug_fit1 <- augment(fit_1)

#Plot fitted vs Actual
autoplot(promising_cust_ts, seg_avg_ord_val) +
  autolayer(aug_fit1,.fitted, colour = "Blue")

accuracy(fit_1) 
# RMSE - 87.8
# MAPE - 20.8
# Best prediction looks like a mean line - experimented with different values of Alpha. 
#Trying with ARIMA - no real seasonality/trend

fit_2 <- model(promising_cust_ts, ARIMA(seg_avg_ord_val~pdq(1,0,1)))
aug_fit2 <- augment(fit_2)

autoplot(promising_cust_ts, seg_avg_ord_val) +
  autolayer(aug_fit2,.fitted, colour = "Blue")

accuracy(fit_2)
#ARIMA(101) - RMSE - 85.7,MAPE - 19.8
#ARIMA(100) - RMSE - 86.4,MAPE - 20
#ARIMA(010) - RMSE - 112, MAPE - 26.5 - Series shifted by 1 unit(differenced by 1)
#ARIMA(001) - RMSE - 85.9,MAPE - 19.9
#ARIMA(111) - Massive error:p
#Best model - ARIMA(101)


#Performing TS for Loyalist Customers
loyalist_cust <- df_cust_wk[df_cust_wk$Segment == 'Loyalists',]
loyalist_cust_tts <- ts(loyalist_cust[,5],frequency = 365.25 / 7,start = 2010)
loyalist_cust_tts <- as_tsibble(loyalist_cust_tts)

autoplot(loyalist_cust_tts)

fit_1_l  <- loyalist_cust_tts %>% model(ETS(value))
#Model: ETS(A,N,N) validates our earlier finding that data does not have much seasonality/trend
aug_fit1_l <- augment(fit_1_l)

#Plot fitted vs Actual
autoplot(loyalist_cust_tts, value) +
  autolayer(aug_fit1_l,.fitted, colour = "Blue")

accuracy(fit_1_l)
#RMSE - 87.8
#MAPE - 20.8

#Trying with ARIMA - no real seasonality/trend

fit_2_l <- model(loyalist_cust_tts, ARIMA(value~pdq(1,0,1)))
aug_fit2_l <- augment(fit_2_l)

autoplot(loyalist_cust_tts, value) +
  autolayer(aug_fit2_l,.fitted, colour = "Blue")

accuracy(fit_2_l)
#RMSE - 85.7
#MAPE - 19.8

#Create forecast for Segment Values in 2011

loyalist_2011_pred <- predict(fit_2_l,h = 52)

#Compute MAPE for Out of Sample Predictions
ol_df_1011 <-
  read_excel("online_retail_II.xlsx", sheet = "Year 2010-2011") %>%
  rename(id = `Customer ID`)

df_clean2 <- ol_df_1011 %>%
  filter((is.na(id) == FALSE) &  # Keep not null id
           (Quantity > 0) &  # Keep Valid Quantity
           (Price > 0) & # Keep valid price
           (substr(
             toupper(Invoice), nchar(Invoice), nchar(Invoice)
           ) != 'C') & # Keep invoice without C as last character
           (check.numeric(StockCode) == TRUE)) %>% mutate(clean_date = as.Date(InvoiceDate, "%Y-%m-%d")
                                                          , tot_ord = Quantity*Price)

cust_clust2 <- read.csv('test.csv')
test_df_cust <- merge(df_clean2, cust_clust2[ , c("id", "Segment")], by = 'id')
nrow(test_df_cust)
nrow(df_clean2)

test_df_cust$week_num <- week(test_df_cust$clean_date)
#Create week level aggregations for every customer cluster in the data set
test_df_cust_wk <- test_df_cust %>% group_by(week_num,Segment) %>% summarise(seg_ord_val = sum(tot_ord),seg_ord = n_distinct(Invoice)) %>% mutate(seg_avg_ord_val = seg_ord_val/seg_ord)

#Promising Customers
test_promising_cust <- test_df_cust_wk[test_df_cust_wk$Segment == 'Promising',]
#Convert DF to TSIBBLE

test_promising_cust_tts <- ts(test_promising_cust[,5],frequency = 365.25 / 7,start = 2011)
test_promising_cust_tts <- as_tsibble(test_promising_cust_tts)

names(test_promising_cust_tts)[2] <- "seg_avg_ord_val"
autoplot(test_promising_cust_tts)

prom_forecast <- predict(fit_2,h=47)
test_promising_cust_tts$forecast <- prom_forecast$.mean

MAPE <- mean(abs(test_promising_cust_tts$forecast - test_promising_cust_tts$seg_avg_ord_val)/test_promising_cust_tts$seg_avg_ord_val)
#20.7% DECENT!

# Performing TS for Need Attention Segment
needattn_cust <- df_cust_wk[df_cust_wk$Segment == 'Need attention',]
needattn_cust_tts <- ts(needattn_cust[,5],frequency = 365.25 / 7,start = 2010)
needattn_cust_tts <- as_tsibble(needattn_cust_tts)

autoplot(needattn_cust_tts)

fit_1_n  <- needattn_cust_tts %>% model(ETS(value))
#Model: ETS(A,N,N) validates our earlier finding that data does not have much seasonality/trend
aug_fit1_n <- augment(fit_1_n)

#Plot fitted vs Actual
autoplot(needattn_cust_tts, value) +
  autolayer(aug_fit1_n,.fitted, colour = "Blue")
#Got straight fitted line

accuracy(fit_1_n)
report(fit_1_n)
#RMSE: 54.8
#MAPE: 16.9

#Trying with ARIMA - no real seasonality/trend

fit_2_n <- model(needattn_cust_tts, ARIMA(value~pdq(1,0,1)))
aug_fit2_n <- augment(fit_2_n)

autoplot(needattn_cust_tts, value) +
  autolayer(aug_fit2_n,.fitted, colour = "Blue")

accuracy(fit_2_n)
report(fit_2_n)
#RMSE: 51.2 , RMSE improved
#MAPE: 15.1
#Slight improvement in AIC, AICc, BIC values

#Create forecast for Segment Values in 2011

needattn_2011_pred <- predict(fit_2_n,h = 52)
#Why don't we have one number for each week


# Performing TS for Detractors Segment
detractor_cust <- df_cust_wk[df_cust_wk$Segment == 'Detractors',]
detractor_cust_tts <- ts(detractor_cust[,5],frequency = 365.25 / 7,start = 2010)
detractor_cust_tts <- as_tsibble(detractor_cust_tts)

autoplot(detractor_cust_tts)

fit_1_d  <- detractor_cust_tts %>% model(ETS(value))
fit_1_d  <- detractor_cust_tts %>% model(ETS(value))
#Model: ETS(M,N,N) validates our earlier finding that data does not have much seasonality/trend
aug_fit1_d <- augment(fit_1_d)

#Plot fitted vs Actual
autoplot(detractor_cust_tts, value) +
  autolayer(aug_fit1_d,.fitted, colour = "Blue")


accuracy(fit_1_d)
report(fit_1_d)
#RMSE: 19.0
#MAPE: 11.6

#Trying with ARIMA - no real seasonality/trend

fit_2_d <- model(detractor_cust_tts, ARIMA(value~pdq(1,0,1)))
aug_fit2_d <- augment(fit_2_d)

autoplot(detractor_cust_tts, value) +
  autolayer(aug_fit2_d,.fitted, colour = "Blue")

accuracy(fit_2_d)
report(fit_2_d)
#RMSE: 18.5 , RMSE improved
#MAPE: 11.1
#Improvement in AIC, AICc, BIC values

#Create forecast for Segment Values in 2011

detractor_2011_pred <- predict(fit_2_d,h = 52)
#Why don't we have one number for each week

# MAPE for 'Loyalist' Segment
test_loyalist_cust <- test_df_cust_wk[test_df_cust_wk$Segment == 'Loyalists',]
#Convert DF to TSIBBLE

test_loyalist_cust_tts <- ts(test_loyalist_cust[,5],frequency = 365.25 / 7,start = 2011)
test_loyalist_cust_tts <- as_tsibble(test_loyalist_cust_tts)

names(test_loyalist_cust_tts)[2] <- "seg_avg_ord_val"
autoplot(test_loyalist_cust_tts)

loyal_forecast <- predict(fit_2_l,h=51) #Changed h to 51
test_loyalist_cust_tts$forecast <- loyal_forecast$.mean

MAPE <- mean(abs(test_loyalist_cust_tts$forecast - test_loyalist_cust_tts$seg_avg_ord_val)/test_loyalist_cust_tts$seg_avg_ord_val)
#15.86%

# MAPE for 'Detractors' Segment
test_detractors_cust <- test_df_cust_wk[test_df_cust_wk$Segment == 'Detractors',]
#Convert DF to TSIBBLE

test_detractors_cust_tts <- ts(test_detractors_cust[,5],frequency = 365.25 / 7,start = 2011)
test_detractors_cust_tts <- as_tsibble(test_detractors_cust_tts)

names(test_detractors_cust_tts)[2] <- "seg_avg_ord_val"
autoplot(test_detractors_cust_tts)

detractors_forecast <- predict(fit_2_d,h=51) #Changed h to 51
test_detractors_cust_tts$forecast <- detractors_forecast$.mean

MAPE <- mean(abs(test_detractors_cust_tts$forecast - test_detractors_cust_tts$seg_avg_ord_val)/test_detractors_cust_tts$seg_avg_ord_val)
#12.59%