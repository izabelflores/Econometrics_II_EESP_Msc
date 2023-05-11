
# code 2

#### PSET 1 ####

library(readr) # importar
library(dplyr) # limpeza
library(quantmod) # time series
library(stargazer) # tabela

#### questao 01 ####

# importando

data <- read_csv("PSet_1/Problem Set 1/data/data_gdp_brazil.csv")

# limpando

colnames(data) <- c("year", "gdp_brazil") # renomear colunas

data_xts <- xts( # objeto xts
  x = data$gdp_brazil,
  order.by = as.Date(as.character(data$year), format = "%Y")
)

# parametros

alpha <- 0.05 # Significance level

# estimando

AR1 <- arima(data_xts, order = c(1, 0, 0), include.mean = TRUE)
AR2 <- arima(data_xts, order = c(2, 0, 0), include.mean = TRUE)
MA1 <- arima(data_xts, order = c(0, 0, 1), include.mean = TRUE)
MA2 <- arima(data_xts, order = c(0, 0, 2), include.mean = TRUE)
ARMA11 <- arima(data_xts, order = c(1, 1, 0), include.mean = TRUE)
ARMA21 <- arima(data_xts, order = c(2, 1, 0), include.mean = TRUE)
ARMA12 <- arima(data_xts, order = c(1, 2, 0), include.mean = TRUE)
ARMA22 <- arima(data_xts, order = c(2, 2, 0), include.mean = TRUE)

stargazer(AR1,
          AR2,
          #MA1,
          #MA2
          ARMA11,
          ARMA21,
          ARMA12,
          ARMA22,
          type = "text")

# AIC

# BIC


