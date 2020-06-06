# DATA 598 Capstone Project
# Liem Luong
# 6/6/2020
# ------------------------
library(dplyr) 
library(forecast)
library(TSA)
library(tseries)
require(gamlss)
library(plotly)
library(quantmod)

# Get the data
data <- read.csv("C:\\Users\\lieml\\OneDrive\\Desktop\\stock_series_train.csv")

# -----------------------------------------------------------------------------
# DATA EXPLORATION (EDA)
# -----------------------------------------------------------------------------
# Perform summary statistics
summary(data)

# Plot the regular stock chart (Open, Close, High Low)
ss = filter(data, stock_id == 1)

df <- data.frame(Date=index(ss),coredata(ss))
df <- tail(df, 200)

figure <- df %>% plot_ly(x = ~Date, type="candlestick",
                        open = ~ss$Open, close = ~ss$Close,
                        high = ~ss$High, low = ~ss$Low) 

figure <- figure %>% layout(title = paste0("Stock ", 1 , " chart"),
                          xaxis = list(rangeslider = list(visible = F)))
figure

# -----------------------------------------------------
# Plot the histogram of 21 stocks of the Close attribute
par(mar=c(2,2,3,3))
par(mfrow=c(7,3))

for(ii in (1:21)){
  ss = filter(data, stock_id == ii)
  hist(ss$Close, main = paste0("Stock ", ii))
}

# ----------------------------------------------------------------------
# MODELING and SIMULATION of the SAMPLE PATH
# ----------------------------------------------------------------------

# Function FIT
fit_function <- function(training_data)
{
  # convert to time series object
  S <- ts(training_data)
  
  # fit the train data with ETS model
  fit_model <- ets(S)
  
  return (fit_model)
}

# Function EXPORT
export_function <- function(file_path, fit_vals, ma, horizon)
{
  # Apply the simulate function in forecast package to simulate the future sample path
  for (c in (0:499)){
    ma = cbind(ma, simulate(fit_vals, nsim = horizon, future = TRUE, bootstrap=TRUE)[1:horizon])
  }
  
  write.csv(ma, file = file_path, row.names = FALSE)
}

# Main CODE
for (i in (1:21)){
  # start 11/1/2019     # end   1/31/2020     # interval 7 days
  # Split the data frame into individual 21 stocks (sub data frames)
  stock = filter(data, stock_id == i)  
  
  # split data into train val, and test of each stock 
  n = length(stock$Date)
  train_size = floor(length(stock$Date)*.8)
  val_size =  floor((n - train_size))/2
  test_size = length(stock$Date) - train_size - val_size
  
  train = stock[1:train_size,]
  val = stock[(train_size+1):(train_size+val_size),]
  test = stock[(train_size+val_size+1):n,]
  
  path = paste0("C:\\Users\\lieml\\OneDrive\\Desktop\\result\\simulate_",i,".csv")
  ma = matrix(i, nrow = 14, ncol = 0)
  
  # Call the training model function
  fit_vals = fit_function(train$Close)
  
  # Call the export function
  export_function(path, fit_vals, ma, 14)
}
## ----------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------








