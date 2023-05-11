
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

# Parameters

P <- 2 # parametro maximo do AR
Q <- 2 # parametro maximo do MA

alpha <- 0.05 # Significance level

# estimando

arma <- vector(mode = "list", length = 0)

# Loop through all combinations of p and q

for (p in 0:P) {
  for (q in 0:Q) {
    # Define the name of the element based on p and q
    name <- if (p == 0) {
      paste0("MA(", q, ")")
    } else if (q == 0) {
      paste0("AR(", p, ")")
    } else {
      paste0("ARMA(", p, ",", q, ")")
    }
    # Estimate an ARMA(p,q) model and store it in the corresponding element
    arma[[name]] <- arima(
      x = data_xts,
      order = c(p, 0, q),
      include.mean = TRUE
    )
  }
}

arma[["MA(0)"]] <- NULL # retirar MA(0)


#### report ####

# cria a função que reporta as regressões

tabela_regressoes <- function(lista_regressoes) {
  
  # cria a lista com os nomes das regressões
  nomes <- sapply(lista_regressoes, function(x) deparse(substitute(x)))
  
  # cria a tabela com as regressões
  stargazer(lista_regressoes, 
            type = "text",
            column.labels = names(lista_regressoes),
            header = FALSE,
            keep.stat = c("aic"))
}


# chama a função
tabela_regressoes(arma)

#### BIC e AIC ####

# criar matriz vazia para armazenar resultados
result_matrix <- matrix(nrow = 2, ncol = length(arma), dimnames = list(c("AIC", "BIC"), names(arma)))

# calcular AIC e BIC para cada modelo
for (i in seq_along(arma)) {
  result_matrix["AIC", names(arma)[i]] <- AIC(arma[[i]])
  result_matrix["BIC", names(arma)[i]] <- BIC(arma[[i]])
}

# imprimir matriz de resultados

stargazer(result_matrix, type = "text")

#### Forecast ####

lista_predict <- lapply(arma, function(x) predict(x, n.ahead = 10))

#### Graficos ####



# Create a matrix with all the necessary objects for ggplot


graficos_regressoes <- function(lista_regressoes) {
  
  # cria a lista com os nomes das regressões
  nomes <- sapply(lista_regressoes, function(x) deparse(substitute(x)))
  
  # cria graficos com as regressões
  temp <- data.frame(
    "year" = 2000:2030,
    "gdp_growth" = c(data$gdp_growth[ds$year %in% 2000:2030], rep(NA, 10)),
    "forecast" = c(rep(NA, 21), lista_regressoes[[pred]]),
    "CI_U" = c(rep(NA, 21), forecast$pred + qnorm(1 - alpha/2) * forecast$se),
    "CI_L" = c(rep(NA, 21), forecast$pred + qnorm(alpha/2) * forecast$se)
  )
}

# chama a função
graficos_regressoes(arma)


lista_predict <- lapply(arma, function(x) predict(x, n.ahead = 10))

temp <- data.frame(
  "year" = 2000:2030,
  "gdp_growth" = c(data$gdp_growth[ds$year %in% 2000:2030], rep(NA, 10)),
  "forecast" = c(rep(NA, 21), forecast$pred),
  "CI_U" = c(rep(NA, 21), forecast$pred + qnorm(1 - alpha/2) * forecast$se),
  "CI_L" = c(rep(NA, 21), forecast$pred + qnorm(alpha/2) * forecast$se)
)


#### plot função que gera repetidamente ####


# cria a função que gera grafico das regressões

graficos_regressoes <- function(lista_regressoes) {
  
  # cria a lista com os nomes das regressões
  nomes <- sapply(lista_regressoes, function(x) deparse(substitute(x)))
  
  # cria graficos com as regressões
  stargazer(lista_regressoes, 
            type = "text",
            column.labels = names(lista_regressoes),
            header = FALSE,
            keep.stat = c("aic"))
}

# chama a função
graficos_regressoes(arma)



