require(forecast) #seasonplot
require(fda) #create bases for functional data
require(rainbow) #for coloring curves
require(qcc) #ewma control chart
require(car) #better QQ plots
require(gamlss)#for worm plots and modeling mean/variance
require(tseries)
require(MASS)
require(tsfknn)
require(fracdiff)
require(Metrics)

#######################################
# function to compute forecasting data
#######################################
stock_forecast <- function(train_data, pre_size){
  
  # Get the close price and transfer to time series
  train_close = train_data$Close
  train_ts <- ts(train_close, frequency = 52, start= 1)
  
  # Fit ARFIMA model (ML Estimates For Fractionally-Differenced ARIMA (P,D,Q) Model)
  tsar <- fracdiff(train_ts)
  
  # Initiate output matrix
  ma = matrix(0, nrow = pre_size, ncol = 1)
  
  #Generate 500 sample paths
  for (c in (1:500)){
    ma = cbind(ma, simulate(tsar, nsim=pre_size, future=TRUE)[1:pre_size])
  }
  return(ma)
}


## Load data
setwd("/Users/s.ma/Desktop/UW/DATA598A(SP)/Project")
stocks <- read.csv("stock_series_train.csv")

# Drop unrelated variables. Only keep: stock id, date and closing price
stocks <- stocks[c("stock_id", "Date", "Close")]



#####################################################
# Forecast with training, validation and testing set
#####################################################

for (i in (1:21) ){
  # Get a single stock
  stock1 = stocks[stocks$stock_id == i, ]

  # Split train/valid/test data set (80/10/10)
  n = length(stock1$Date)
  train_size = floor(length(stock1$Date)*.8)
  val_size =  floor((n - train_size))/2
  test_size = length(stock1$Date) - train_size - val_size

  train = stock1[1:train_size,]
  val = stock1[(train_size+1):(train_size+val_size),]
  test = stock1[(train_size+val_size+1):n,]

  # Fit the model and generate sample paths
  forecast_m = stock_forecast(train, val_size)

  # Write to files
  write.csv(forecast_m, file=paste0("/Users/s.ma/Desktop/UW/DATA598A(SP)/Project/forecast_test/stock_series_forecasts_", i, ".csv"), row.names=FALSE)

}



#############################
# Forecast with all the data
#############################

for (i in (1:21) ){
  # Get a single stock
  stock1 = stocks[stocks$stock_id == i, ]
  
  # 14 weeks
  forecast_size = 14
 
  # Fit the model and generate sample paths
  forecast_m = stock_forecast(stock1, forecast_size)
  
  # Write to files
  write.csv(forecast_m, file=paste0("/Users/s.ma/Desktop/UW/DATA598A(SP)/Project/forecast/stock_series_forecasts_", i, ".csv"), row.names=FALSE)
  
}


