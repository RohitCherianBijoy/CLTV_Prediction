#Load necessary packages. Duh. 
library(dplyr)
library(readxl)
library(devtools)
library(varhandle)
library(lubridate)
library(BTYD)
library(ggplot2)
library(sm)


#Readings
#https://towardsdatascience.com/predicting-customer-lifetime-value-with-buy-til-you-die-probabilistic-models-in-python-f5cac78758d9
#

ol_df_0910 <-
  read_excel("online_retail_II.xlsx", sheet = "Year 2009-2010") %>%
  rename(id = `Customer ID`)

str(ol_df_0910)

###Data Cleaning

df_clean <- ol_df_0910 %>%
  filter((is.na(id) == FALSE) &  # Keep not null id
           (Quantity > 0) &  # Keep Valid Quantity
           (Price > 0) & # Keep valid price
           (substr(
             toupper(Invoice), nchar(Invoice), nchar(Invoice)
           ) != 'C') & # Keep invoice without C as last character
           (check.numeric(StockCode) == TRUE)) %>% mutate(clean_date = as.Date(InvoiceDate, "%Y-%m-%d"))

#Add additional columns for analysis
max(df_clean$clean_date) #"2010-12-09"
min(df_clean$clean_date) #"2009-12-01"

analysis_date <- as.Date("2010-12-10") #max date+1
df_clean <- df_clean %>% mutate(total_cost = Quantity*Price)

#Model inputs - Time units in weeks
# x - (Frequency - 1) - number of repeat purchases made by the customer
# tx - most recent purchase in weeks since customers first purchase
# T - time to be considered in weeks since customers first purchase
bgnbd_df <- df_clean %>% group_by(id) %>% summarise(recency = as.numeric((max(clean_date) - min(clean_date))/7),
                                                    rep_freq = n_distinct(Invoice) - 1,
                                                    T = as.numeric((analysis_date - min(clean_date))/7))
df_est <- select(bgnbd_df,c('rep_freq','recency','T'))
names(df_est) <- c('x','t.x','T.cal')
str(df_est)

df_est_mat <- as.matrix(df_est)
#Experimenting with different values of max.params


params_bgnbd2 <- bgnbd.EstimateParameters(df_est_mat, 
                                          par.start = c(1, 3, 1, 3),
                                          max.param.value = 20,
                                          hessian = FALSE)

params_bgnbd3 <- bgnbd.EstimateParameters(df_est_mat, 
                                          par.start = c(1, 3, 1, 3),
                                          max.param.value = 60,
                                          hessian = FALSE)


params_bgnbd4 <- bgnbd.EstimateParameters(df_est_mat, 
                                         par.start = c(1, 3, 1, 3),
                                         max.param.value = 120,
                                         hessian = FALSE)

params_bgnbd5 <- bgnbd.EstimateParameters(df_est_mat, 
                                          par.start = c(1, 3, 1, 3),
                                          max.param.value = 300,
                                          hessian = FALSE)



#Plot Actual vs Model Results - No difference by messing around the max.params value, set it 300

bgnbd.PlotFreqVsConditionalExpectedFrequency(params_bgnbd2, 
                                             T.star = 52, 
                                             df_est_mat, 
                                             x.star = df_est$x, 
                                             censor = 20)
#Final Estimated CF
params_bgnbd5
#r         alpha        a        b 
#0.78355514 7.74982534 0.06229663 4.59035746 
#r,alpha - Gamma Distribution 
#a,b - Beta Distribution
#Estimated Gamma & Beta Distribution for our customers





#Make Predictions for customers in 2010 for expected transactions in 2011
df_est$predicted_bgnbd <- bgnbd.ConditionalExpectedTransactions(
  params = params_bgnbd5,
  T.star = 52,
  x = df_est$x,
  t.x = df_est$t.x,
  T.cal= df_est$T.cal
)

df_pred <- as.data.frame(cbind(bgnbd_df$id,df_est$predicted_bgnbd))
names(df_pred) <- c('id','pred_freq')
df_pred$pred_freq <- round(df_pred$pred_freq+1)

#How does the model predict on Holdout data?

ol_df_1011 <-
  read_excel("online_retail_II.xlsx", sheet = "Year 2010-2011") %>%
  rename(id = `Customer ID`)

df_clean_10_11 <- ol_df_1011 %>%
  filter((is.na(id) == FALSE) &  # Keep not null id
           (Quantity > 0) &  # Keep Valid Quantity
           (Price > 0) & # Keep valid price
           (substr(
             toupper(Invoice), nchar(Invoice), nchar(Invoice)
           ) != 'C') & # Keep invoice without C as last character
           (check.numeric(StockCode) == TRUE)) %>% mutate(clean_date = as.Date(InvoiceDate, "%Y-%m-%d"))


holdout_period <- df_clean_10_11 %>% group_by(id) %>% summarise(rep_freq = n_distinct(Invoice))

df_comparison <- merge(x=holdout_period,y=df_pred,by="id",all.x=TRUE)
df_comparison[is.na(df_comparison$pred_freq),3] <- 0
df_comparison$index <- 1:nrow(df_comparison)

#Plotting these values as a probability distribution function

plot(density(df_comparison$rep_freq))
lines(density(df_comparison$pred_freq),col="blue") 
#Fit follows the distribution of actual customer frequencies of 2010-2011. What is the RSS & RMSE

RSS <- sum((df_comparison$rep_freq-df_comparison$pred_freq)^2)
MSE <- sqrt(RSS/nrow(df_comparison)) #MSE for this is ~6.59, average predictions are off by 4.93
MAPE = mean(abs(df_comparison$rep_freq-df_comparison$pred_freq)/df_comparison$rep_freq) #MAPE = 1.31

write.csv(df_comparison,'pred_trans_2011.csv')


































