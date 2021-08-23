
## Install and load Packages 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

## Data Loading 
#https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory/download
#https://github.com/dmedina-lab/cryptocurrency/raw/main/cryptocurrency_price.zip

dlf <- tempfile()
download.file("https://github.com/dmedina-lab/cryptocurrency/raw/main/cryptocurrency_price.zip", dlf)

unzip(dlf, exdir = "crypto-price-hist")

crypto_hist <-
  list.files(path = "crypto-price-hist", pattern = "*.csv", full.names = T) %>% 
  map_df(~read_csv(.))

rm(dlf)

## Data exploration

#Check NAs
anyNA(crypto_hist)

#Structure 
str(crypto_hist)
dim(crypto_hist)

#Head
head(crypto_hist)

#Summary
summary(crypto_hist)

#Count rows for each coin
crypto_hist %>% group_by(Name) %>% 
  summarize(count = n(), date_from= min(Date), date_to= max(Date) ) %>%
  arrange(desc(count))
#There are 23 coins. Bitcoin, Litecoin, XRP, Dogecoin are the most old, since 2013.  

#top 5 news coins
crypto_hist %>% group_by(Name) %>% 
  summarize(date_from= min(Date), date_to= max(Date) ) %>%
  arrange(desc(date_from)) %>%
  head(5)

#Transformation of date formats to short date, year, month, day and price market relationship
crypto_hist_p <- crypto_hist %>% 
  mutate(year = as.integer(format(Date, format="%Y"))
        , month = as.integer(format(Date, format="%m"))
        , day = as.integer(format(Date, format="%d"))
        , date_short = as.Date(format(Date, format="%Y-%m-%d"))
        , Price_Market = as.numeric(Close / Marketcap)
        )

tail(crypto_hist_p)


#Trend historical Close price for each Coin Name
crypto_hist_p %>% ggplot(aes(date_short, Close, col=Name)) +
  geom_line() 

#Boxplot historical Close price for each Coin Name
crypto_hist_p %>% ggplot(aes(Name, Marketcap, fill=Name)) +
  geom_boxplot() 

# Trend hisotorical Market Capitalization 
crypto_hist_p %>%  group_by(date_short) %>%
  summarize(mt = sum(Marketcap), dt = min(date_short)) %>%
  ggplot(aes(dt, mt)) +
  geom_line()

#Correlation between Close price and Market Capitalization
crypto_hist_p %>% summarize(r = cor(Close, Marketcap)) %>% pull(r)
#0.6884597

#Correlation between Close price and Volume
crypto_hist_p %>% summarize(r = cor(Close, Volume)) %>% pull(r)
#0.2857774

#Volatility is best defined as the 30-day standard deviation of daily log returns annualized. 
#With this in mind, high volatility is at 100% or above, medium volatility is between 50% and 100%, 
#while low volatility is below 50%. 
# top 10 least Volatility (Anual)
crypto_hist_p %>% group_by(Name) %>% 
  summarize(avg_close = mean(Close),  sd_close = sd(Close), volatility = sqrt(365) * sd(diff(log(Close))) * 100) %>%
  arrange(volatility) %>%
  head(10)

# top 10 Most Volatility (Anual)
crypto_hist_p %>% group_by(Name) %>% 
  summarize(avg_close = mean(Close),  sd_close = sd(Close), volatility = sqrt(365) * sd(diff(log(Close))) * 100) %>%
  arrange(desc(volatility))%>%
  top_n(10)

#Monthly Volatility
crypto_hist_p %>% group_by(Name) %>% 
  summarize(avg_close = mean(Close),  sd_close = sd(Close), volatility = sqrt(30) * sd(diff(log(Close))) * 100) %>%
  arrange(volatility)

## We observe that Bitcoin es the third least volatility coin, The first and second least volatility are USD COin and Theter respectible,
# Both base on dollar.

# we will use Bitcoin in ours models. We will show the Volatility for year.
crypto_hist_p %>% filter(Name=="Bitcoin") %>%group_by(year) %>% 
  summarize(avg_close = mean(Close),  sd_close = sd(Close), volatility = sqrt(365) * sd(diff(log(Close))) * 100) %>%
  arrange(volatility)

# Trend of Bitcoin
crypto_hist_p %>% filter(Name == "Bitcoin") %>%
  ggplot(aes(date_short, Close)) +
  geom_line() 

##Data wrangling
btc <- crypto_hist_p %>% filter(Name == "Bitcoin") 

set.seed(1, sample.kind="Rounding")

##Split the btc data set to train set(80%) and test set(20%)
test_index_btc <- createDataPartition(y = btc$Close, times = 1, p = 0.2, list = FALSE)
train_set_btc <- btc[-test_index_btc,]
test_set_btc <- btc[test_index_btc,]

#Calculation
# Mean Absolute Percentage Error (MAPE)
MAPE <- function(actual_price,pred_price){
  mape <- mean(abs((actual_price - pred_price)/actual_price))*100
  return (mape)
}

# Residual Mean Squared Error (RMSE)
RMSE <- function(actual_price, pred_price){
  sqrt(mean((actual_price - pred_price)^2, na.rm = TRUE))
}

#Random Forest
set.seed(1, sample.kind="Rounding")
library(randomForest)

fit_rf <- randomForest(Close ~ Marketcap + Volume + Open + Low + High + date_short  + year + month + day, data=train_set_btc)
#print(fit_rf)
pred_rf <- predict(fit_rf, test_set_btc)

MAPE(test_set_btc$Close, pred_rf) #1.18 %

RMSE(test_set_btc$Close, pred_rf) #246.0422

#Plot the importance
varImpPlot(fit_rf)
#The most important variables are Marketcap, Low, High, date_short and Open

#To avoid overtraing we will use only the most important variables to build the model
rf_revised <- randomForest(Close ~ Marketcap + Low + High + date_short  + Open, data = train_set_btc)
print(rf_revised)
pred_rf_r <- predict(rf_revised, test_set_btc)

MAPE(test_set_btc$Close, pred_rf_r) #1.26 %

RMSE(test_set_btc$Close, pred_rf_r) #277.52 

test_set_btc %>%
  mutate(Close_hat = pred_rf_r ) %>%
  ggplot() +
  geom_point(aes(date_short, Close)) +
  geom_line(aes(date_short, Close_hat), col="red")

## k-nearest neighbors (KNN)
set.seed(1, sample.kind="Rounding")
train_knn <- train(Close ~ Marketcap + Low + High + date_short  + Open, method = "knn",
                   data = train_set_btc,
                   tuneGrid = data.frame(k = seq(1, 100, 2)))
print(train_knn)

ped_knn <- predict(train_knn, test_set_btc)


MAPE(test_set_btc$Close, ped_knn) #3.27 %

RMSE(test_set_btc$Close, ped_knn) #246.33

test_set_btc %>%
  mutate(Close_hat = ped_knn ) %>%
  ggplot() +
  geom_point(aes(date_short, Close)) +
  geom_line(aes(date_short, Close_hat), col="red")



