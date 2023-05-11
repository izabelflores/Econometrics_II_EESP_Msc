
#### Set seed ####

set.seed(18061999)


#### FUNÇÃO MONTE CARLO EXPERIMENT

# vou criar uma funcao que faz a simulacao de Monte Carlo, dado os parametros:

# p
# q
# g
# theta
# c

monte_carlo_ARMA <- function(p = 1, q = 1, g = 1, theta = 0.5, c = FALSE) {
  
  # Run a parallel loop over sample sizes
  
  results <- foreach(
    capT = capT_vec, .inorder = TRUE, .errorhandling = "remove", .verbose = FALSE
  ) %dopar% {
    # Create a dataframe to store the results for each MC repetition
    resultsT <- data.frame(
      "capT" = rep(capT, M),
      "bias" = rep(NA, M),
      "normalized_coef" = rep(NA, M),
      "reject" = rep(NA, M)
    )
    
    # Loop over MC repetitions
    for (m in 1:M) {
      # Simulate an MA(1) process. If g == 1, it is a Gaussian process.
      if (g == 1) {
        Y <- arima.sim(model = list(ma = theta), n = capT, rand.gen = rnorm)
        
        # If g == 0, it is a exponential process
      } else {
        Y <- arima.sim(model = list(ma = theta), n = capT, rand.gen = rexp)
        
      }
      
      # Estimate an ARMA model
      ma1 <- arima(
        x = Y,
        order = c(p, 0, q),
        include.mean = c
      )
      
      ### Store the results
      
      # Store the estimated bias
      resultsT$bias[m] <- ma1$coef[1] - theta
      
      # Store the normalized coefficient
      resultsT$normalized_coef[m] <- (ma1$coef[1] - theta) / sqrt(ma1$var.coef[1,1])
      
      # Store the test decision
      resultsT$reject[m] <- as.numeric(
        abs((ma1$coef[1] - theta) / sqrt(ma1$var.coef[1,1])) >= qnorm(0.975)
      )
      
    }
    
    # Return the results
    return(resultsT)
  }
  
}

#### FUNÇÃO CONVERGÊNCIA EM PROBABILIDADE

# Illustrate convergence in probability

convergence_probability <- function(x) {
  
  # Create a matrix to store the results
  probs <- data.frame(
    "capT" = capT_vec,
    "delta1" = rep(NA, length(capT_vec)),
    "delta2" = rep(NA, length(capT_vec)),
    "delta3" = rep(NA, length(capT_vec)),
    "delta4" = rep(NA, length(capT_vec))
  )
  
  # Loop over sample size
  for (t in 1:length(capT_vec)) {
    # Loop over values of delta: Once more, I chose a slow code so that I would
    # save my own time.
    for (d in delta) {
      # Compare all MC estimates against delta
      temp <- abs(x[[t]]$bias) > d
      
      # Compute the probability of the bias being small
      probs[t, which(d == delta) + 1] <- mean(temp)
    }
    
  }
  
  # Create a plot with the results
  gg <- ggplot(data = probs, aes(x = capT)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Sample Size") + ylab("Probability") +
    geom_line(
      aes(y = delta1, color = "d = 0.25"),
      size = 1.5
    ) +
    geom_line(
      aes(y = delta2, color = "d = 0.20"),
      size = 1.5
    ) +
    geom_line(
      aes(y = delta3, color = "d = 0.15"),
      size = 1.5
    ) +
    geom_line(
      aes(y = delta4, color = "d = 0.10"),
      size = 1.5
    ) +
    scale_colour_manual(values = c(
      "#85C0F9", "#0F2080", "#F5793A", "#A95AA1"
    )) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  print(gg)
  
}


#### Illustrate convergence in distribution

# Collect the CDF of our normalized coefficient for each sample size

# Create a data frame to store the results

convergence_distribution <- function(a) {
  
temp <- data.frame(
  "capT" = rep(NA, 6 * 101),
  "Fy" = rep(NA, 6 * 101),
  "Qy" = rep(NA, 6 * 101)
)

# Find the relevant sample size indexes
i_vec <- which(capT_vec %in% c(5, 10, 15, 20, 50, 100))

# Loop over the sample sizes
for (i in i_vec) {
  # Index within i_vec
  j <- which(i == i_vec)
  
  # Write the sample size
  temp$capT[(1 + (j - 1) * 101):(j * 101)] <- a[[i]]$capT[1]
  
  # Write down the probabilities
  temp$Fy[(1 + (j - 1) * 101):(j * 101)] <- seq(0, 1, 0.01)
  
  # Write down the quantiles
  temp$Qy[(1 + (j - 1) * 101):(j * 101)] <- quantile(
    a[[i]]$normalized_coef, probs = seq(0, 1, 0.01), na.rm = TRUE
  )
}

# Write capT as a factor to enforce the ordering
temp$capT <- factor(temp$capT)

# Create a ggplot
gg <- ggplot(temp, aes(x = Qy, y = Fy)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Normalized Coefficient") + ylab("CDF") +
  geom_line(aes(colour = capT), size = 2) +
  stat_function(fun = pnorm, size = 1, linetype = "dashed")  +
  theme(
    legend.position = "bottom"
  ) + guides(
    colour = guide_legend(nrow = 2, byrow = TRUE, title = "Sample Size")
  )

# Show the plot
print(gg)

}

