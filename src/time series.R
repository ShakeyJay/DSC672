library(ggplot2)
library(forecast) 
library(tseries)
library(fpp2)
library(lubridate)
library(Metrics)
############################################################################################################################################
# Here I import my data frame that is the average of all the series that are in the 99th percentile and below. 
# This series also has additional data that has been artificially created to be anomalous. The artificial data was created in 
# excel by generating a random number between 10 and 20 then dividing by 10 to create a floating-point number between 1 and 2 inclusive. 
# The anomalous data starts 16/01/2013 12:30 PM and runs till 17/01/2013 at 03:00.
############################################################################################################################################

df <- read.csv("99th_single_df_anom.csv") # Import data.
df$DateTime <- as.POSIXct(df$DateTime,format="%m/%d/%Y %H:%M") # Covert date to time/date obj.

head(df)

ts = ts(df$KWH.hh, c(2013, 1), frequency = 48) # Create ts object.

############################################################################################################################################
# Here we do some EDA.
############################################################################################################################################
autoplot(ts) # Plot ts. 
plot(decompose(ts)) # Plot the composition.
Acf(ts)
Pacf(ts)

############################################################################################################################################
############################################### Anomaly detection. #########################################################################
# Here I start the implementation on the anomaly detection algorithm. The purpose of this is to detect anomalous energy consumption due to 
# energy theft. This algorithm is from the paper "Drift-Aware_Methodology_for_Anomaly_Detection_in_Smart_Grid" with a few changes. The first
# change is that I was not able to extract representative  clusters from the data before hand but I believe in our case this was not necessary 
# because one, all the meters belong to residential households while the data set in the paper has meter data from different types of economic 
# activities such as business, households, and factories. Two, After excluding the top 1 % of data from our data set we were able to get a 
# more uniform distribution of data. 
############################################################################################################################################
std_24hr <- function(date_df) {
  
  ### This function gets called in step 5 of the anomaly detection algorithm. 
  ### It returns the standard deviation of the error squared of the previous day.
  ### This function only gets called once every 48 time steps or one a day.
  
  # Set the start and end date to the previous day.
  start_date <- date_df - hours(24) + minutes(0)
  end_date <- start_date + hours(23) + minutes(30)
  
  # 0. Step zero creates a time series of the previous day.
  previous_24hr <- subset(df, DateTime >= toString(start_date) & DateTime <= toString(end_date), select = c(DateTime, KWH.hh)) 
  previous_24hr$DateTime <- as.POSIXct(previous_24hr$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
  previous_24hr_ts = ts(previous_24hr$KWH.hh, c(2013, 1), frequency = 48)
  
  #1. Fit a model on the previous day.
  fit1 = tslm(previous_24hr_ts ~ trend + fourier(previous_24hr_ts, K=8))
  #2. Get the actual power consumption at time 
  t_1 <- end + 1
  real_consumption_24hr <- previous_24hr[,2]
  #3.
  forecast_24hr <- forecast(fit1, data.frame(fourier(previous_24hr_ts, K=8, h=48)))
  #4.
  error_24hr <- (forecast_24hr$mean - real_consumption_24hr)^2
  #5.
  std_24hr <- sd(error_24hr)
  return(std_24hr)
  
}
############################################################################################################################################

# anomaly detection for the week.
week3_test_df <- subset(df, DateTime >= "2013-01-15 00:00:00" & DateTime <= "2013-01-21 23:30:00", select = c(DateTime, KWH.hh)) # Week 2.

### set up metrics ####
true_positives <- 0
false_positives <- 0
true_negatives <- 0
false_negatives <- 0


for(i in 1:nrow(week3_test_df)){ # one week of data.
  # create a sliding window 24-hours long and increment one time step.
  start <- 0 + i 
  end <- i + 47

  if (i == 288){ # break our of the loop when you get to the last day.
    break;
  }
  # 0. create a ts object of size window.
  week3_test_df_slice <-  week3_test_df[start:end,] # Time slice of size window
  week3_test_df_slice$DateTime <- as.POSIXct(week3_test_df_slice$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
  week3_test_ts = ts(week3_test_df_slice$KWH.hh, c(2013, 1), frequency = 48) # TS object.
  
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
  if (end %% 48 == 0){
    std_24hr_value <- std_24hr(week3_test_df[t_1,1])
    Upper_bound <- std_24hr_value * 2
    Lower_bound <- std_24hr_value * -2
  }

  
  # #6.
  if(error > Upper_bound | error < Lower_bound){
    print(paste0("Value ",  round(week3_test_df[t_1,2], digits = 3), " is anomalous! ",
                 " The Forecast is: ", round(forecast_one_step$mean, digits = 3),
                 " The error squared is: ", round(error, digits = 3),
                 sep = ' '))
    # metrics
    if (week3_test_df[t_1,2] >= 1){
      true_positives <- true_positives + 1
    }else {
      false_positives <- false_positives + 1
    }
  }else {
    print("Not an anomalous value!")

    # metrics
    if (week3_test_df[t_1,2] >= 1){
      false_negatives <- false_negatives + 1
    }else {
      true_negatives <- true_negatives + 1
    }
  }
  print(" ")
} 

percision <- (true_positives / (true_positives + false_positives)) # lots of false positives
recall <- (true_positives / (true_positives + false_negatives)) # but catches most of the anomaly

############################################################################################################################################
############################################ 7 day a-head Forecasting ######################################################################
############################################################################################################################################
# Here I try to answer research question 3. Which is can I make a 7-day ahead forecast? This data does not include anomalous data. 
# It is just the average of my series.
############################################################################################################################################

df_non_anom <- read.csv("99th_single_df_anom.csv") # Import data.
df_non_anom$DateTime <- as.POSIXct(df_non_anom$DateTime,format="%m/%d/%Y %H:%M") # Covert date to time/date obj.

# Week 1 is my training data.
jan_week.1 <- df_non_anom[2352:2688,]
jan_week.1_ts = ts(jan_week.1$KWH.hh, c(2013, 1), frequency = 48)

# Week 2 is my test data.
jan_week.2 <- df_non_anom[2352:3023,]
jan_week.2_ts = ts(jan_week.2$KWH.hh, c(2013, 1), frequency = 48)

# EDA to figure out how many periods I should use in the model.

spect = spectrum(jan_week.1_ts, log="no", spans=c(2, 2), plot=T, xlab="Frequency (Cycles/Year)", xlim = c(0,8))

############################################################################################################################################
# Modeling.
fit2 = tslm(jan_week.1_ts ~ trend + fourier(jan_week.1_ts, K=4)) # Harmonic regression.
summary(fit2)
forecast <- forecast(fit2, data.frame(fourier(jan_week.1_ts, K=4, h=336))) # Forecast one week out.
autoplot( forecast(fit2, data.frame(fourier(jan_week.1_ts, K=4, h=336)))) # Plot the forecast.

################
# Create data frames out of the actual values, the predictions, the upper and lower bound.
df1 = data.frame(time = seq(1,672,length=672), M = jan_week.2$KWH.hh, isin = "observations")
df2 = data.frame(time = seq(336,672,length=336), M = forecast$mean, isin = "my_forecast")
df3 = data.frame(time = seq(336,672,length=336), M = forecast$upper[,2], isin = "upper_bound")
df4 = data.frame(time = seq(336,672,length=336), M = forecast$lower[,2], isin = "lower_bound")
df5 = rbind(df1, df2, df3, df4)
  
# Plot the results.
ggplot(df5, aes(x = time, y = M, color = isin)) + geom_line(size=1.2) + 
  scale_colour_manual(values=c(observations='blue', my_forecast='red', upper_bound='black', lower_bound='orange'))

# Error analysis. Check to see if the residuals have any pattern. 
autoplot(fit2$residuals)
acf(autoplot(fit2$residuals))

# RMSE
rmse(df_non_anom[2688:3023,2],as.numeric(df2[,2]))
