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

df <- read.csv("99th_single_df.csv") # Import data.
head(df)
###############

df[3271,1] <- '2013-03-10 02:00:00'
df[3272,1] <- '2013-03-10 02:30:00'

df <- df

df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
head(df)

ts = ts(df$KWH.hh, c(2013, 1), frequency = 48)

autoplot(ts)

plot(decompose(ts))

eacf(ts)

head(df)
##########################################
train_set <-  df[1:8790, c(1,2)]  # Training set
test_set <-  df[8791:17580, c(1,2)]  # Test set

train_set$DateTime <- as.POSIXct(train_set$DateTime,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
test_set$DateTime <- as.POSIXct(test_set$DateTime,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.

train_set_ts = ts(train_set$KWH.hh, c(2013, 1), frequency = 48)
test_set_ts = ts(test_set$KWH.hh, c(2013, 1), frequency = 48)

autoplot(mstl(msts(train_set_ts, seasonal.periods = c(7,365)), s.window = 7))

####################
fit1 = auto.arima(ts)
fit1

#################
jan_week.2 <- df[1:672,]
jan_week.2$DateTime <- as.POSIXct(jan_week.2$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
jan_week.2_ts = ts(jan_week.2$KWH.hh, c(2013, 1), frequency = 48)

jan_week.1 <- df[1:336,]
jan_week.1$DateTime <- as.POSIXct(jan_week.1$DateTime ,format="%Y-%m-%d %H:%M:%S") # Covert date to time/date obj.
jan_week.1_ts = ts(jan_week.1$KWH.hh, c(2013, 1), frequency = 48)

autoplot(jan_week.2_ts)
###################

spect = spectrum(ts, log="no", spans=c(2, 2), plot=T, xlab="Frequency (Cycles/Year)", xlim = c(0,8))

##############

fit1 = tslm(jan_week.1_ts ~ trend + fourier(jan_week.1_ts, K=8))
summary(fit1)

forecast <- forecast(fit1, data.frame(fourier(jan_week.1_ts, K=8, h=336))) # add temperature later to my series.
autoplot( forecast(fit1, data.frame(fourier(jan_week.1_ts, K=8, h=336))))

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
