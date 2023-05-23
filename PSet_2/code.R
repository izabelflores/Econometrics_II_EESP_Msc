

# packages
library(urca) # teste df
library(zoo)
library(ggplot2) # graficos
library(readr) # importar
library(dplyr) # limpeza
library(stringr) # limpeza
library(lubridate) # datas


#### Questao 01 ####

# importando

data <- read_csv("PSet_2/corn-production-land-us.csv")

# limpando

ds <- data %>% select(Year, `Corn production (tonnes)`) %>% #selecionar colunas
  rename(Prod = `Corn production (tonnes)`) %>% # renomear
  filter(Year <= 2021 & # filtrar periodo 
           Year >= 1950)

ds$Year <- year(as.Date(as.character(ds$Year),
                        format = "%Y")) # especificar data

## Defining Augmented Dickey-Fuller Test

# Plot the data


gg <- ggplot(ds, aes(x = Year)) +
  theme_bw(base_size = 12) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Years") + ylab("Corn Production") +
  geom_line(aes(y = Prod), size = 1.5, color = "#0F2080") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(gg)

# parece haver uma tendencia temporal na série, 
# mas faremos todos os testes

### ADF Testes ####

##### Drift #####

df_drift <- ur.df(
  y = ds$Prod,
  type = 'drift', # so com drift
  selectlags = c("BIC") # criterio BIC para escolher numero de lags
)

# lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag) AR(2)


print(summary(df_drift))

# phi1: 
# estatistica < valor critico
# nao rejeita H0 de phi1 -> tem raiz unitaria &
                          # nao tem drift

# é um AR(2) com raiz unitária, sem desvio. 

##### Drift and Time Trend ####

# Run the test using BIC to choose the number of lags
df_drift <- ur.df(
  y = ds$Prod,
  type = 'trend',
  selectlags = c("BIC")
)

print(summary(df_drift)) # AR(2)

# phi2:
# valor critico < estatistica 
# rejeita HO de phi 2 -> nao tem raiz unitaria 
                        # ou tem drift 
                        # ou tem tendencia


### do primeiro teste sabemos que tem raiz unitaria
### e nao tem drift
### entao tem tendencia

# phi3:
# valor critico < estatistica
# rejeita H0 de phi 3 -> não tem raiz unitaria 
                       # ou não tem tendencia

### ? sabemos que tem tendencia, raiz unitaria e nao tem drift


# tau3:
# estatistica < valor critico
# rejeita H0 de tau3 -> nao tem raiz unitaria

## logo, nao tem raiz unitaria


##### No Drift and No Time Trend #####

# Run the test using BIC to choose the number of lags
df_drift <- ur.df(
  y = ds$Prod,
  type = 'none',
  selectlags = c("BIC")
)

print(summary(df_drift))

# tau1
# valor critico < estatistica -> nao rejeita H0
# tem raiz unitaria



