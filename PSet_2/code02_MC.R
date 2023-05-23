# Monte Carlo

# Clean the working environment
rm(list = ls())

# Load the required packages
library("ggplot2")
library("lmtest")
library("lpdensity")

# Set seed
set.seed(18061999)

# Model 
# Yt = alpha + delta * t + epsilon_{t}
# ARIMA(0,0,0)
# epsilon_{t} iid

# Parameters
capT <- 50 #10000     # Number of observed periods (sample size)
alpha <- 0   # Intercept
M <- 100 #10000      # Number of MC repetitions
delta <- 1

# Create a data frame to store the results.

sim_results <- matrix(NA, nrow = capT, ncol = M)

t_stat <- matrix(NA, nrow = M, ncol = 1)

###############################################################################
# Run the Monte Carlo Simulation
###############################################################################
# Create 1000 random walk processes with a drift: When creating random walk
# processes, the function arima.sim always start with y_0 = 0. The function
# replicate is a very concise way to write a quick for loop.

# erro 

epsilon <- t_dist <- rt(n = M, df = 5)

# modelo sem tendencia
sim_out_trend <- replicate(
  n = M,
  arima.sim(model = list(order = c(0, 0, 0)), n = capT, mean = alpha, innov = epsilon)
)

# tendencia

t <- 1:capT
trend <-  t*delta

# modelo com tendencia

sim_trend <- ds + trend

# Perform OLS regression

regress <- lm(sim_trend ~ t)

coefficients <- summary_regress[["Response Y1"]][["coefficients"]]

# Extract the residual standard error (RSE)

std_error <- coefficients[2,2]
estimate <- coefficients[2,1]

t_stat[i] <- (estimate - delta) / std_error

# For each time series that we created, I will run two regressions:
# 1) Unrestricted AR(1) model, saving the estimated bias and the t-statistics
# centered around the true value of the coefficient
# 2) Y_t = a + b * X_{t - 1} + e_t, where Y_t is any time series and X_t is the next
# time series, saving the p-value of a test whose null is b = 0. Under the null,
# two stationary series would generate a p-value that is uniformly distributed.

  #####################################
  # Estimate an unrestricted AR(1) model
  #####################################
  # Run the regression
  ar1 <- lm(ds[2:(capT + 1), m] ~ ds[1:capT, m])
  
  # Store the bias
  results$bias[m] <- ar1$coefficients[2] - 1
  
  # Store the t-statistic
  results$tstat[m] <- (ar1$coefficients[2] - 1)/coeftest(ar1)[2,2]
  
  #####################################
  # Estimate Y_t = a + b * X_t + e_t
  #####################################
  # Since I cannot run this type of regression for the very last time series,
  # I need an if statement to avoid dimensional problems
  if (m < M) {
    # Run the regression
    regYX <- lm(ds[2:(capT + 1), m] ~ ds[1:capT, m + 1])
    
    # Store the pvalue
    results$pvalue[m] <- coeftest(regYX)[2,4]
  }
  

##############################################################################
# Problem 1: Analyze the average bias.
##############################################################################
print(paste0(
  "The average bias of our OLS estimator is equal to ",
  round(mean(results$bias),2),
  ", which is quite large compared to the true value (rho = 1)."
))




##### Problem 2 ####

# Analyze the density of the t-statistic. Under the null, a
# stationary process would generate a t-statistic that is normally distributed.
# Here, we find that unit root processes generate t-statistics that are not
# normally distributed.
# Estimate the density of the t-stastistic nonparametrically
tdens <- lpdensity(
  data = results$tstat,
  grid = quantile(results$tstat, probs = seq(from = 0.01, to = 0.99, by = 0.01)),
)

# Create a data.frame with the grid points and the estimated density.
temp <- data.frame(
  "grid" = tdens$Estimate[, 1],
  "tdens" = tdens$Estimate[, 5]
)

# Plot the estimated density and compare it against the normal density.
gg <- ggplot(data = temp, aes(x = grid)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  ylab("Density") + xlab("Grid") +
  geom_line(aes(y = tdens, color = "Estimated Density"), size = 1.5) +
  geom_function(
    fun = dnorm, n = 101, args = list(mean = 0, sd = 1),
    aes(color = "Normal Density"), size = 1.5, linetype = "dashed",
    show.legend = TRUE
  ) +
  scale_colour_manual(values = c("#0F2080", "#85C0F9")) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
print(gg)

# Save the plot
ggsave("figures/figure-t-density-vs-normal.pdf", width = 11, height = 8.5)

#### Problem 3 #### 

# Analyze the density of the pvalue. Under the null, a stationary
# process would generate a p-value that is uniformly distributed. Here, we
# find that unit root processes generate p-values that are NOT uniformly
# distributed.

# Estimate the density of the p-value nonparametrically
pdens <- lpdensity(
  data = results$pvalue[1:(M-1)],
  grid = quantile(
    results$pvalue[1:(M-1)], probs = seq(from = 0.01, to = 0.99, by = 0.01)
  ),
)

# Create a data.frame with the grid points and the estimated density.
temp <- data.frame(
  "grid" = pdens$Estimate[, 1],
  "pdens" = pdens$Estimate[, 5]
)

# Plot the estimated density and compare it against the uniform density.
gg <- ggplot(data = temp, aes(x = grid)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  ylab("Density") + xlab("Grid") +
  geom_line(aes(y = pdens, color = "Estimated Density"), size = 1.5) +
  geom_function(
    fun = dunif, n = 101,
    aes(color = "Uniform Density"), size = 1.5, linetype = "dashed",
    show.legend = TRUE
  ) +
  scale_colour_manual(values = c("#0F2080", "#85C0F9")) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
print(gg)

# Save the plot
ggsave("figures/figure-p-density-vs-uniform.pdf", width = 11, height = 8.5)
