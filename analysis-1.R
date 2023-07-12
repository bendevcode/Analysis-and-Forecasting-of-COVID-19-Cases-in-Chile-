#loading requird libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(readr)
library(tidyr)
library(tseries)
library(lubridate)
library(xts)
library(scales)
library(tseries)



# Loading the excel file data
covid_data <- read_excel("TS-covid-data_Full.xlsx")

# view the data
View(covid_data)
head(covid_data)
summary(covid_data)

#Selected location - Chile
#Selected variable - new_cases

# Filter data for Chile and the variable 'new_cases'
chile_data <- covid_data %>%
  filter(location == "Chile") %>%
  select(date, new_cases)

View(chile_data)
summary(chile_data)


#converting the date column to date format
chile_data$date <- as.Date(chile_data$date)

summary(chile_data)

#check for missing values
sum(is.na(chile_data$new_cases))

#there are three missing values

# Handling missing values

# fill the missing values using the first non-missing value 
#(for the beginning of the series) and the last non-missing value (for the end of the series)
chile_data$new_cases <- na.locf(chile_data$new_cases, fromLast = FALSE) # Fill missing values at the beginning
chile_data$new_cases <- na.locf(chile_data$new_cases, fromLast = TRUE)  # Fill missing values at the end

summary(chile_data)

#this might not be needed actually since the NA's have been filled with the locf function
# Interpolate missing values of new cases variable
chile_data$new_cases <- na.approx(chile_data$new_cases)
summary(chile_data)

# Creating a time series plot with missing values interpolated
ggplot(data = chile_data, aes(x = date, y = new_cases)) +
  geom_line() +
  labs(title = "Daily New Confirmed COVID-19 Cases in Chile",
       x = "Date",
       y = "Number of New Cases")+
  theme_minimal()

?ggplot
?ts
?ts
#----------
# Daily time series
start_date <- as.Date(min(chile_data$date))
chile_daily <- ts(chile_data$new_cases, frequency = 7, start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%j"))))

# Weekly time series
weekly_cases <- aggregate(chile_daily, nfrequency = 1)
weekly_start <- c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%U")) + 1)
chile_weekly <- ts(weekly_cases, frequency = 52, start = weekly_start)


# Monthly time series
#aggregate() function is not suitable for converting daily data to monthly data,
#as the number of days per month is not fixed. Instead, we can use the xts and 
#zoo packages to create the monthly time series.

# Creating an xts object for daily cases
daily_cases_xts <- xts(chile_data$new_cases, order.by = chile_data$date)

# Calculating the monthly sum of cases
monthly_cases_xts <- apply.monthly(daily_cases_xts, FUN = sum)

# Converting the xts object to a ts object
chile_monthly <- ts(monthly_cases_xts, frequency = 12, start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%m"))))


length(chile_monthly)

summary(chile_monthly)
head(chile_monthly)


#----------

View(chile_monthly)
View(chile_weekly)
View(chile_daily)

chile_monthly
chile_weekly
chile_daily

#-----------prem analysis


# Summary statistics for daily, weekly, and monthly data
summary_daily <- summary(chile_daily)
summary_weekly <- summary(chile_weekly)
summary_monthly <- summary(chile_monthly)

# Function to create time series plots
create_ts_plot <- function(ts_data, title) {
  ggplot(data = as.data.frame(ts_data), aes(x = index(ts_data), y = coredata(ts_data))) +
    geom_line() +
    ggtitle(title) +
    xlab("Date") +
    ylab("New Cases") +
    theme_minimal()
}

# Create time series plots for daily, weekly, and monthly data
daily_plot <- create_ts_plot(chile_daily, "Daily New Cases (Chile)")
weekly_plot <- create_ts_plot(chile_weekly, "Weekly New Cases (Chile)")
monthly_plot <- create_ts_plot(chile_monthly, "Monthly New Cases (Chile)")

# Display summary statistics and plots
print(summary_daily)
print(summary_weekly)
print(summary_monthly)

daily_plot
weekly_plot
monthly_plot

#----------premiliary analysis

# Decomposing the time series using stl

#for daily and weekly data
decomp_daily <- stl(chile_daily, s.window = "periodic")
decomp_weekly <- stl(chile_weekly, s.window = "periodic")

#for monthly data

decomp_monthly_add <- decompose(chile_monthly, type = "additive")
decomp_monthly_mult <- decompose(chile_monthly, type = "multiplicative")


autoplot(decomp_daily) + ggtitle("Decomposition of Daily New Cases")
autoplot(decomp_weekly) + ggtitle("Decomposition of Weekly New Cases")
autoplot(decomp_monthly_add) + ggtitle("Decomposition of Monthly New Cases (Additive)")
autoplot(decomp_monthly_mult) + ggtitle("Decomposition of Monthly New Cases (Multiplicativetive)")

#Identification of the covid wave
#increased date scales to identify the waves correctly

ggplot(data = chile_data, aes(x = date, y = new_cases)) +
  geom_line() +
  labs(title = "Daily New Confirmed COVID-19 Cases in Chile",
       x = "Date",
       y = "Number of New Cases") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%Y-%m-%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#waves
#2020-05-01 to 2020-08-01, 2022 – 02-01 to 2022-05-01,  2022-06-01 to 2022-08-01 

# Identifying the start and end dates of each wave
wave1_start <- as.Date("2020-05-01")
wave1_end <- as.Date("2020-08-01")
wave2_start <- as.Date("2022-02-01")
wave2_end <- as.Date("2022-05-01")
wave3_start <- as.Date("2022-06-01")
wave3_end <- as.Date("2022-08-01")

# Create subsets for each wave
wave1_data <- chile_data[chile_data$date >= wave1_start & chile_data$date <= wave1_end, ]
wave2_data <- chile_data[chile_data$date >= wave2_start & chile_data$date <= wave2_end, ]
wave3_data <- chile_data[chile_data$date >= wave3_start & chile_data$date <= wave3_end, ]

# Create time series objects for each wave
wave1_ts <- ts(wave1_data$new_cases, frequency = 7)
wave2_ts <- ts(wave2_data$new_cases, frequency = 7)
wave3_ts <- ts(wave3_data$new_cases, frequency = 7)

#decomposition analysis for the three waves
decomposed_wave1 <- stl(wave1_ts, s.window = "periodic")
decomposed_wave2 <- stl(wave2_ts, s.window = "periodic")
decomposed_wave3 <- stl(wave3_ts, s.window = "periodic")

autoplot(decomposed_wave1)
autoplot(decomposed_wave2)
autoplot(decomposed_wave3)



#Time series modeling
#Implement the Classical Method:


# Fit Holt-Winters models with different combinations of trend and seasonality
hw1_add <- hw(wave1_ts, seasonal = "additive", h=30)
hw1_mul <- hw(wave1_ts, seasonal = "multiplicative",h=30)
hw2_add <- hw(wave2_ts, seasonal = "additive",h=30)
hw2_mul <- hw(wave2_ts, seasonal = "multiplicative",h=30)
hw3_add <- hw(wave3_ts, seasonal = "additive",h=30)
hw3_mul <- hw(wave3_ts, seasonal = "multiplicative",h=30)


# Create a copy of the original daily time series
chile_daily_positive <- chile_daily

# Add a small positive constant to only zero values
constant <- 0.000001
chile_daily_positive[chile_daily_positive == 0] <- chile_daily_positive[chile_daily_positive == 0] + constant

# Fit additive and multiplicative Holt-Winters models
hw_full_add <- hw(chile_daily_positive, seasonal = "additive", h=28)
hw_full_mul <- hw(chile_daily_positive, seasonal = "multiplicative",h=28)

# Compare the models using AIC
# Extract AIC values
hw1_add_aic <- hw1_add$model$aic
hw1_mul_aic <- hw1_mul$model$aic
hw2_add_aic <- hw2_add$model$aic
hw2_mul_aic <- hw2_mul$model$aic
hw3_add_aic <- hw3_add$model$aic
hw3_mul_aic <- hw3_mul$model$aic
hw_full_add_aic <- hw_full_add$model$aic
hw_full_mul_aic <- hw_full_mul$model$aic

# Create data frame to compare models
models_aic <- data.frame(
  Wave = c("Wave 1", "Wave 1", "Wave 2", "Wave 2", "Wave 3", "Wave 3", "Full Period", "Full Period"),
  Model = c("Additive", "Multiplicative", "Additive", "Multiplicative", "Additive", "Multiplicative", "Additive", "Multiplicative"),
  AIC = c(hw1_add_aic, hw1_mul_aic, hw2_add_aic, hw2_mul_aic, hw3_add_aic, hw3_mul_aic, hw_full_add_aic, hw_full_mul_aic)
)

models_aic



#checking for stationarity
#timeseries plots
par(mfrow=c(2,2))

plot(wave1_ts, main = "Wave 1 Time Series", xlab = "Time", ylab = "Cases")
plot(wave2_ts, main = "Wave 2 Time Series", xlab = "Time", ylab = "Cases")
plot(wave3_ts, main = "Wave 3 Time Series", xlab = "Time", ylab = "Cases")
plot(chile_daily, main = "Full Period Time Series", xlab = "Time", ylab = "Cases")

#Autocorrelation Function (ACF) Plots:
acf(wave1_ts, main = "ACF of Wave 1 Time Series")
acf(wave2_ts, main = "ACF of Wave 2 Time Series")
acf(wave3_ts, main = "ACF of Wave 3 Time Series")
acf(chile_daily, main = "ACF of Full Period Time Series")


# Perform the ADF test
adf_wave1 <- adf.test(wave1_ts)
adf_wave2 <- adf.test(wave2_ts)
adf_wave3 <- adf.test(wave3_ts)
adf_full <- adf.test(chile_daily)

# Display the p-values
adf_wave1$p.value
adf_wave2$p.value
adf_wave3$p.value
adf_full$p.value


#transformation for stationarity
wave1_ts_diff <- diff(wave1_ts)
wave2_ts_diff <- diff(wave2_ts)

#test for stationarity
adf_wave1_diff <- adf.test(wave1_ts_diff)
adf_wave2_diff <- adf.test(wave2_ts_diff)

print(adf_wave1_diff)
print(adf_wave2_diff)

#Time series plots:
plot(wave1_ts_diff, main = "Differenced Wave 1 Time Series", xlab = "Time", ylab = "Differenced Cases")
plot(wave2_ts_diff, main = "Differenced Wave 2 Time Series", xlab = "Time", ylab = "Differenced Cases")

#ACF plots
acf(wave1_ts_diff, main = "ACF of Differenced Wave 1 Time Series")
acf(wave2_ts_diff, main = "ACF of Differenced Wave 2 Time Series")


#analysing the transformed timeseries using acf and pacf
#acf
acf(wave1_ts_diff, main = "ACF of Differenced Wave 1 Time Series")
acf(wave2_ts_diff, main = "ACF of Differenced Wave 2 Time Series")
acf(wave3_ts, main = "ACF of Wave 3 Time Series")
acf(chile_daily, main = "ACF of full Time Series")

#pacf
pacf(wave1_ts_diff, main = "PACF of Differenced Wave 1 Time Series")
pacf(wave2_ts_diff, main = "PACF of Differenced Wave 2 Time Series")
pacf(wave3_ts, main = "PACF of Wave 3 Time Series")
pacf(chile_daily, main = "PACF of full Time Series")

    
#ARIMA

#Splitting the data into test and train
# Split the dataset into training and testing sets
train_end <- floor(0.8 * length(wave1_ts_diff))
train_wave1 <- wave1_ts_diff[1:train_end]
test_wave1 <- wave1_ts_diff[(train_end + 1):length(wave1_ts_diff)]

train_end2 <- floor(0.8 * length(wave2_ts_diff))
train_wave2 <- wave2_ts_diff[1:train_end2]
test_wave2 <- wave2_ts_diff[(train_end2 + 1):length(wave2_ts_diff)]

train_end3 <- floor(0.8 * length(wave3_ts))
train_wave3 <- wave3_ts[1:train_end3]
test_wave3 <- wave3_ts[(train_end3 + 1):length(wave3_ts)]

train_end_full <- floor(0.8 * length(chile_daily))
train_full <- chile_daily[1:train_end_full]
test_full <- chile_daily[(train_end_full + 1):length(chile_daily)]

# Fiting ARIMA and SARIMA models for each transformed time series

# Wave 1
# ARIMA
fit_wave1_arima <- arima(train_wave1, order = c(0, 1, 0))
# SARIMA
fit_wave1_sarima <- arima(train_wave1, order = c(0, 1, 0), seasonal = list(order = c(0, 0, 1), period = 7))

# Wave 2
# ARIMA
fit_wave2_arima <- arima(train_wave2, order = c(1, 1, 0))
# SARIMA
fit_wave2_sarima <- arima(train_wave2, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7))

# Wave 3
# ARIMA
fit_wave3_arima <- arima(train_wave3, order = c(1, 1, 0))
# SARIMA
fit_wave3_sarima <- arima(train_wave3, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 7))

# Full period
# ARIMA
fit_full_arima <- arima(train_full, order = c(1, 1, 0))
# SARIMA
fit_full_sarima <- arima(train_full, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 7))


# Auto.arima
auto_wave1 <- auto.arima(train_wave1)
auto_wave2 <- auto.arima(train_wave2)
auto_wave3 <- auto.arima(train_wave3)
auto_full <- auto.arima(train_full)

#compare models using BIC and AIC
# Compare AIC and BIC
aic_wave1 <- c(fit_wave1_arima$aic, fit_wave1_sarima$aic, auto_wave1$aic)
aic_wave2 <- c(fit_wave2_arima$aic, fit_wave2_sarima$aic, auto_wave2$aic)
aic_wave3 <- c(fit_wave3_arima$aic, fit_wave3_sarima$aic, auto_wave3$aic)
aic_full <- c(fit_full_arima$aic, fit_full_sarima$aic, auto_full$aic)

bic_wave1 <- c(fit_wave1_arima$bic, fit_wave1_sarima$bic, auto_wave1$bic)
bic_wave2 <- c(fit_wave2_arima$bic, fit_wave2_sarima$bic, auto_wave2$bic)
bic_wave3 <- c(fit_wave3_arima$bic, fit_wave3_sarima$bic, auto_wave3$bic)
bic_full <- c(fit_full_arima$bic, fit_full_sarima$bic, auto_full$bic)

#a table with the aic and bic
comparison <- data.frame(TimeSeries = c("Wave 1", "Wave 2", "Wave 3", "Full Period"),
                         Model1_AIC = c(aic_wave1[1], aic_wave2[1], aic_wave3[1], aic_full[1]),
                         Model2_AIC = c(aic_wave1[2], aic_wave2[2], aic_wave3[2], aic_full[2]),
                         Auto_AIC = c(aic_wave1[3], aic_wave2[3], aic_wave3[3], aic_full[3]),
                         Model1_BIC = c(bic_wave1[1], bic_wave2[1], bic_wave3[1], bic_full[1]),
                         Model2_BIC = c(bic_wave1[2], bic_wave2[2], bic_wave3[2], bic_full[2]),
                         Auto_BIC = c(bic_wave1[3], bic_wave2[3], bic_wave3[3], bic_full[3]))

print(comparison)

               
#Plotting residuals

# Plotting residuals and ACF for each model
par(mfrow=c(2,2)) # create 2x2 grid of plots

# Wave 1
resid_wave1_1 <- residuals(fit_wave1_arima)
plot(resid_wave1_1, main = "Wave 1 ARIMA Model Residuals")
acf(resid_wave1_1, main = "Wave 1 ARIMA Model Residual ACF")

resid_wave1_2 <- residuals(fit_wave1_sarima)
plot(resid_wave1_2, main = "Wave 1 SARIMA Model Residuals")
acf(resid_wave1_2, main = "Wave 1 SARIMA Model Residual ACF")

# Wave 2
resid_wave2_1 <- residuals(fit_wave2_arima)
plot(resid_wave2_1, main = "Wave 2 ARIMA Model Residuals")
acf(resid_wave2_1, main = "Wave 2 ARIMA Model Residual ACF")

resid_wave2_2 <- residuals(fit_wave2_sarima)
plot(resid_wave2_2, main = "Wave 2 SARIMA Model Residuals")
acf(resid_wave2_2, main = "Wave 2 SARIMA Model Residual ACF")

# Wave 3
resid_wave3_1 <- residuals(fit_wave3_arima)
plot(resid_wave3_1, main = "Wave 3 ARIMA Model Residuals")
acf(resid_wave3_1, main = "Wave 3 ARIMA Model Residual ACF")

resid_wave3_2 <- residuals(fit_wave3_sarima)
plot(resid_wave3_2, main = "Wave 3 SARIMA Model Residuals")
acf(resid_wave3_2, main = "Wave 3 SARIMA Model Residual ACF")

# Full period
resid_full_1 <- residuals(fit_full_arima)
plot(resid_full_1, main = "Full Period ARIMA Model Residuals")
acf(resid_full_1, main = "Full Period ARIMA Model Residual ACF")

resid_full_2 <- residuals(fit_full_sarima)
plot(resid_full_2, main = "Full Period SARIMA Model Residuals")
acf(resid_full_2, main = "Full Period SARIMA Model Residual ACF")


#residuals for auto arima
plot(residuals(auto_wave1), main = "Wave 1 auto arima Model Residuals")
acf(residuals(auto_wave1), main = "Wave 1 auto arima Model Residuals ACF")

plot(residuals(auto_wave2), main = "Wave 2 auto arima Model Residuals")
acf(residuals(auto_wave2), main = "Wave 2 auto arima Model Residuals ACF")

plot(residuals(auto_wave3), main = "Wave 3 auto arima Model Residuals")
acf(residuals(auto_wave3), main = "Wave 3 auto arima Model Residuals ACF")

plot(residuals(auto_full), main = "full period auto arima Model Residuals")
acf(residuals(auto_full), main = "full period auto arima Model Residuals ACF")


#boxtest
# Wave 1

ljung_box_wave1_arima <- Box.test(resid_wave1_1, lag = 10, type = "Ljung-Box")
print(ljung_box_wave1_arima)

ljung_box_wave1_sarima <- Box.test(resid_wave1_2, lag = 10, type = "Ljung-Box")
print(ljung_box_wave1_sarima)

ljung_box_wave1_autoarima <- Box.test((residuals(auto_wave1)), lag = 10, type = "Ljung-Box")
print(ljung_box_wave1_autoarima)

# Wave 2
ljung_box_wave_arima <- Box.test(resid_wave2_1, lag = 10, type = "Ljung-Box")
print(ljung_box_wave_arima)

ljung_box_wave2_sarima <- Box.test(resid_wave2_2, lag = 10, type = "Ljung-Box")
print(ljung_box_wave2_sarima)

ljung_box_autowave2 <- Box.test((residuals(auto_wave2)), lag = 10, type = "Ljung-Box")
print(ljung_box_autowave2)

# Wave 3
ljung_box_wave3_arima <- Box.test(resid_wave3_1, lag = 10, type = "Ljung-Box")
print(ljung_box_wave3_arima)

ljung_box_wave3_sarima <- Box.test(resid_wave3_2, lag = 10, type = "Ljung-Box")
print(ljung_box_wave3_sarima)

ljung_box_autowave3 <- Box.test((residuals(auto_wave3)), lag = 10, type = "Ljung-Box")
print(ljung_box_autowave3)

#full period

ljung_box_full_arima <- Box.test(resid_full_1, lag = 10, type = "Ljung-Box")
print(ljung_box_full_arima)

ljung_box_full_sarima <- Box.test(resid_full_2, lag = 10, type = "Ljung-Box")
print(ljung_box_full_sarima)

ljung_box_autowavefull <- Box.test((residuals(auto_full)), lag = 10, type = "Ljung-Box")
print(ljung_box_autowavefull)

#classical model forcasting

?forecast
# Wave 1
wave1_add_forecast <- forecast(hw1_add, h = 28)
wave1_mul_forecast <- forecast(hw1_mul, h = 28)

# Wave 2
wave2_add_forecast <- forecast(hw2_add, h = 28)
wave2_mul_forecast <- forecast(hw2_mul, h = 28)

# Wave 3
wave3_add_forecast <- forecast(hw3_add, h = 28)
wave3_mul_forecast <- forecast(hw3_mul, h = 28)

# Full Period
full_add_forecast <- forecast(hw_full_add, h = 28)
full_mul_forecast <- forecast(hw_full_mul, h = 28)


#plots

# Wave 1
autoplot(wave1_ts, series = "Observed") +
  autolayer(wave1_add_forecast, series = "Additive Forecast") +
  autolayer(wave1_mul_forecast, series = "Multiplicative Forecast") +
  ggtitle("Wave 1 Forecasts") +
  xlab("Time") + ylab("Value")

# Wave 2
autoplot(wave2_ts, series = "Observed") +
  autolayer(wave2_add_forecast, series = "Additive Forecast") +
  autolayer(wave2_mul_forecast, series = "Multiplicative Forecast") +
  ggtitle("Wave 2 Forecast") +
  xlab("Time") + ylab("Value") +
  guides(colour = guide_legend(title = "Series"))

# Wave 3
autoplot(wave3_ts, series = "Observed") +
  autolayer(wave3_add_forecast, series = "Additive Forecast") +
  autolayer(wave3_mul_forecast, series = "Multiplicative Forecast") +
  ggtitle("Wave 3 Forecast") +
  xlab("Time") + ylab("Value") +
  guides(colour = guide_legend(title = "Series"))

# Full Period
autoplot(chile_daily, series = "Observed") +
  autolayer(full_add_forecast, series = "Additive Forecast") +
  autolayer(full_mul_forecast, series = "Multiplicative Forecast") +
  ggtitle("Full Period Forecast") +
  xlab("Time") + ylab("Value") +
  guides(colour = guide_legend(title = "Series"))



# ARIMA model forecasting:

#plots

# Wave 1
library(gridExtra)

# Creating the individual plots
plot_wave1_arima <- fit_wave1_arima %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 1 ARIMA Forecast")
plot_wave1_auto_arima <- auto_wave1 %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 1 Auto ARIMA Forecast")
plot_wave1_sarima <- fit_wave1_sarima %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 1 SARIMA Forecast")

# Arranging the plots in three rows and one column
grid.arrange(plot_wave1_arima, plot_wave1_auto_arima, plot_wave1_sarima, nrow = 3, ncol = 1)


# Wave 2
plot_wave2_arima <- fit_wave2_arima %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 2 ARIMA Forecast")
plot_wave2_auto_arima <- auto_wave2 %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 2 Auto ARIMA Forecast")
plot_wave2_sarima <- fit_wave2_sarima %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 2 SARIMA Forecast")

grid.arrange(plot_wave2_arima, plot_wave2_auto_arima, plot_wave2_sarima, nrow = 3, ncol = 1)

# Wave 3
plot_wave3_arima <- fit_wave3_arima %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 3 ARIMA Forecast")
plot_wave3_auto_arima <- auto_wave3 %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 3 Auto ARIMA Forecast")
plot_wave3_sarima <- fit_wave3_sarima %>% forecast(h = 28) %>% autoplot() + ggtitle("Wave 3 SARIMA Forecast")

grid.arrange(plot_wave3_arima, plot_wave3_auto_arima, plot_wave3_sarima, nrow = 3, ncol = 1)

# Full Period
plot_full_arima <- fit_full_arima %>% forecast(h = 28) %>% autoplot() + ggtitle("Full Period ARIMA Forecast")
plot_full_auto_arima <- auto_full %>% forecast(h = 28) %>% autoplot() + ggtitle("Full Period Auto ARIMA Forecast")
plot_full_sarima <- fit_full_sarima %>% forecast(h = 28) %>% autoplot() + ggtitle("Full Period SARIMA Forecast")

grid.arrange(plot_full_arima, plot_full_auto_arima, plot_full_sarima, nrow = 3, ncol = 1)


#comparison of forcasts
# Comparison of forecasts

# Classical models

accuracy(wave1_add_forecast, test_wave1)
accuracy(wave1_mul_forecast, test_wave1)

accuracy(wave2_add_forecast, test_wave2)
accuracy(wave2_mul_forecast, test_wave2)

accuracy(wave3_add_forecast, test_wave3)
accuracy(wave3_mul_forecast, test_wave3)

# Create a subset of the test dataset with the last 28 points
test_full_last_28 <- tail(test_full, 28)

# Calculate the accuracy of the forecasts for the last 28 points
accuracy(full_add_forecast, test_full_last_28)
accuracy(full_mul_forecast, test_full_last_28)

# ARIMA models

# Wave 1
wave1_arima_forecast <- fit_wave1_arima %>% forecast(h = 28)
wave1_auto_arima_forecast <- auto_wave1 %>% forecast(h = 28)
wave1_sarima_forecast <- fit_wave1_sarima %>% forecast(h = 28)
accuracy(wave1_sarima_forecast, test_wave1)
accuracy(wave1_arima_forecast, test_wave1)
accuracy(wave1_auto_arima_forecast, test_wave1)

# Wave 2
wave2_arima_forecast <- fit_wave2_arima %>% forecast(h = 28)
wave2_auto_arima_forecast <- auto_wave2 %>% forecast(h = 28)
wave2_sarima_forecast <- fit_wave2_sarima %>% forecast(h = 28)
accuracy(wave2_sarima_forecast, test_wave2)
accuracy(wave2_arima_forecast, test_wave2)
accuracy(wave2_auto_arima_forecast, test_wave2)

# Wave 3
wave3_arima_forecast <- fit_wave3_arima %>% forecast(h = 28)
wave3_auto_arima_forecast <- auto_wave3 %>% forecast(h = 28)
wave3_sarima_forecast <- fit_wave3_sarima %>% forecast(h = 28)
accuracy(wave3_sarima_forecast, test_wave3)
accuracy(wave3_arima_forecast, test_wave3)
accuracy(wave3_auto_arima_forecast, test_wave3)

# Full Period
full_arima_forecast <- fit_full_arima %>% forecast(h = 28)
full_auto_arima_forecast <- auto_full %>% forecast(h = 28)
full_sarima_forecast <- fit_full_sarima %>% forecast(h = 28)
accuracy(full_sarima_forecast, test_full_last_28)
accuracy(full_arima_forecast, test_full_last_28)
accuracy(full_auto_arima_forecast, test_full_last_28)

#results
# Combine accuracy metrics into a data frame
results <- data.frame(
  Model = c(
    "Wave1_Additive",
    "Wave1_Multiplicative",
    "Wave2_Additive",
    "Wave2_Multiplicative",
    "Wave3_Additive",
    "Wave3_Multiplicative",
    "FullPeriod_Additive",
    "FullPeriod_Multiplicative",
    "Wave1_ARIMA",
    "Wave1_Auto_ARIMA",
    "Wave1_SARIMA",
    "Wave2_ARIMA",
    "Wave2_Auto_ARIMA",
    "Wave2_SARIMA",
    "Wave3_ARIMA",
    "Wave3_Auto_ARIMA",
    "Wave3_SARIMA",
    "FullPeriod_ARIMA",
    "FullPeriod_Auto_ARIMA",
    "FullPeriod_SARIMA"
  ),
  RMSE = c(
    accuracy(wave1_add_forecast, test_wave1)[2,2],
    accuracy(wave1_mul_forecast, test_wave1)[2,2],
    accuracy(wave2_add_forecast, test_wave2)[2,2],
    accuracy(wave2_mul_forecast, test_wave2)[2,2],
    accuracy(wave3_add_forecast, test_wave3)[2,2],
    accuracy(wave3_mul_forecast, test_wave3)[2,2],
    accuracy(full_add_forecast, test_full_last_28)[2,2],
    accuracy(full_mul_forecast, test_full_last_28)[2,2],
    accuracy(wave1_arima_forecast, test_wave1)[2,2],
    accuracy(wave1_auto_arima_forecast, test_wave1)[2,2],
    accuracy(wave1_sarima_forecast, test_wave1)[2,2],
    accuracy(wave2_arima_forecast, test_wave2)[2,2],
    accuracy(wave2_auto_arima_forecast, test_wave2)[2,2],
    accuracy(wave2_sarima_forecast, test_wave2)[2,2],
    accuracy(wave3_arima_forecast, test_wave3)[2,2],
    accuracy(wave3_auto_arima_forecast, test_wave3)[2,2],
    accuracy(wave3_sarima_forecast, test_wave3)[2,2],
    accuracy(full_arima_forecast, test_full_last_28)[2,2],
    accuracy(full_auto_arima_forecast, test_full_last_28)[2,2],
    accuracy(full_sarima_forecast, test_full_last_28)[2,2]
  )
)

# Display the results
results

