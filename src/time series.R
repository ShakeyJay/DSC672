library(ggplot2)
library(ggfortify)
library(zoo)
library(xts)
library(lubridate)
library(forecast) 
library(fBasics)
library(akima)
library(tseries)
library(fpp2)
library(astsa)
library(lmtest)
library(readr)
library(naniar)
library(corrplot)
library(TSA) 
library(dummies)
library(fastDummies)

df <- read.csv("99th_single_df_anom.csv") # Import data.
###############

df$DateTime <- as.POSIXct(df$DateTime,format="%m/%d/%Y %H:%M") # Covert date to time/date obj.
head(df)

ts = ts(df$KWH.hh, c(2013, 1), frequency = 48) # Create ts object.

autoplot(ts) # Plot ts. 
plot(decompose(ts)) # Plot the composition.

#### Check lags for analysis. #########
# TO-DO

################# Anomaly detection. ####################
# 2. Historical day prediction.
week3_24hr <- subset(df, DateTime >= "2013-01-15 00:00:00" & DateTime <= "2013-01-15 23:30:00", select = c(DateTime, KWH.hh)) # Week 2.
week3_24hr$DateTime <- as.POSIXct(week3_24hr$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
week3_24hr_ts = ts(week3_24hr$KWH.hh, c(2013, 1), frequency = 48)

#1. 
fit1 = tslm(week3_24hr_ts ~ trend + fourier(week3_24hr_ts, K=8))
#2. 
real_consumption_24hr <- week3_24hr[,2]
#3.
forecast_24hr <- forecast(fit1, data.frame(fourier(week3_24hr_ts, K=8, h=48)))
#4.
error_24hr <- (forecast_24hr$mean - real_consumption_24hr)^2
#5.
std_24hr <- sd(error_24hr)

# anomaly detection for the week.
week3_test_df <- subset(df, DateTime >= "2013-01-15 00:00:00" & DateTime <= "2013-01-21 23:30:00", select = c(DateTime, KWH.hh)) # Week 2.

### set up metrics ####
true_positives <- 0
false_positives <- 0
true_negatives <- 0
false_negatives <- 0


for(i in 1:nrow(week3_test_df)){ # one week of data.
  # create window.
  start <- 0 + i 
  end <- i + 47

  if (i == 288){ # break our of the loop when you get to the last day.
    break;
  }
  # create a ts object of size window.
  week3_test_df_slice <-  week3_test_df[start:end,] # Time slice of size window

  week3_test_df_slice$DateTime <- as.POSIXct(week3_test_df_slice$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
  week3_test_ts = ts(week3_test_df_slice$KWH.hh, c(2013, 1), frequency = 48)
  
  #1. 
  fit2 = tslm(week3_test_ts ~ trend + fourier(week3_test_ts, K=8))
  #2. 
  t_1 <- end + 1
  real_consumption <- week3_test_df[t_1,2]
  #3.
  forecast_one_step <- forecast(fit2, data.frame(fourier(week3_test_ts, K=8, h=1)))
  #4.
  error <- (forecast_one_step$mean - real_consumption)^2
  # 5.
  std_24hr
  
  Upper_bound <- std_24hr * 2
  Lower_bound <- std_24hr * -2
  
  #6.
  if(error > Upper_bound | error < Lower_bound){
    print(paste0("Value ",  round(week3_test_df[i,2], digits = 3), " is anomalous! ",  "The error is: ", round(error, digits = 3), 
                 " The Forecast is: ", round(forecast_one_step$mean, digits = 3), sep = ' '))
    # metrics
    if (week3_test_df[i,2] >= 1){
      true_positives <- true_positives + 1
    }else {
      false_positives <- false_positives + 1
    }
  }else {
    print("Not an anomalous value!") 
    # metrics
    if (week3_test_df[i,2] >= 1){
      false_negatives <- false_negatives + 1
    }else {
      true_negatives <- true_negatives + 1
    }
  }
} 

percision <- (true_positives / (true_positives + false_positives)) # lots of false positives
recall <- (true_positives / (true_positives + false_negatives)) # but catches most of the anomaly



################# Forecasting. ####################
jan_week.1 <- df[2352:2688,]
jan_week.1$DateTime <- as.POSIXct(jan_week.1$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
jan_week.1_ts = ts(jan_week.1$KWH.hh, c(2013, 1), frequency = 48)

jan_week.2 <- df[2352:3023,]
jan_week.2$DateTime <- as.POSIXct(jan_week.2$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
jan_week.2_ts = ts(jan_week.2$KWH.hh, c(2013, 1), frequency = 48)

###################

spect = spectrum(jan_week.1_ts, log="no", spans=c(2, 2), plot=T, xlab="Frequency (Cycles/Year)", xlim = c(0,8))

##############

fit2 = tslm(jan_week.1_ts ~ trend + fourier(jan_week.1_ts, K=6))
summary(fit2)

forecast <- forecast(fit2, data.frame(fourier(jan_week.1_ts, K=6, h=336))) # add temperature later to my series.
autoplot( forecast(fit12, data.frame(fourier(jan_week.1_ts, K=6, h=336))))

#################

df1 = data.frame(time = seq(1,672,length=672), M = jan_week.2$KWH.hh, isin = "observations")
df2 = data.frame(time = seq(336,672,length=336), M = forecast$mean, isin = "my_forecast")
df3 = data.frame(time = seq(336,672,length=336), M = forecast$upper[,2], isin = "upper_bound")
df4 = data.frame(time = seq(336,672,length=336), M = forecast$lower[,2], isin = "lower_bound")
df5 = rbind(df1, df2, df3, df4)
  
ggplot(df5, aes(x = time, y = M, color = isin)) + geom_line(size=1.2) + 
  scale_colour_manual(values=c(observations='blue', my_forecast='red', upper_bound='black', lower_bound='orange'))


autoplot(fit1$residuals)

acf(autoplot(fit1$residuals))

df6 = data.frame(time = seq(1,672,length=672), M = jan_week.2$KWH.hh, isin = "observations")
df7 = data.frame(time = seq(1,336,length=336), M = forecast$mean, isin = "my_forecast")

df8 = rbind(df6, df7)

ggplot(df8, aes(x = time, y = M, color = isin)) + geom_line(size=1.2) + 
  scale_colour_manual(values=c(observations='blue', my_forecast='red'))
