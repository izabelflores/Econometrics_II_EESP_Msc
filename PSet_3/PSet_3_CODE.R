
### PSET 3

# library

library(tidyr) 
library(dplyr)
library(readr)
library(lmtest)
library(quantmod)

# clearing

data <- read.csv("PSet_3\\data_brazil.csv")

colnames(data) <- c("Year", "GDP", "FX", "IPC")

data <- data %>%
  filter(Year >= 1942 & Year <= 2020)

data1 <- data %>%
  filter(Year >= 1942 & Year <= 2019)

# estimating ADL

ADL21 <- lm(GDP ~ lag(GDP, 1) + lag(GDP, 2) + lag(FX, 1), data = data1)

predictions <- predict(ADL21, n.ahead = 1)

ADL22 <- lm(GDP ~ lag(GDP, 1) + lag(GDP, 2) + lag(IPC, 1) + lag(IPC, 2), data = data1)

predictions <- predict(ADL22, n.ahead = 1)

MODEL3 <- lm(GDP ~ lag(GDP, 1) + lag(GDP, 2) + lag(FX, 1) + lag(FX, 2) + lag(IPC, 1) + lag(IPC, 2), data = data1)

predictions <- predict(ADL22, n.ahead = 1)

# data for ARMA models

data1_xts <- xts( # objeto xts
  x = data1$GDP,
  order.by = as.Date(as.character(data1$Year), format = "%Y")
)

ARMA20 <- arima(
  x = data1_xts,
  order = c(2, 0, 0),
  include.mean = TRUE)

predictions <- predict(ARMA20, n.ahead = 1)



