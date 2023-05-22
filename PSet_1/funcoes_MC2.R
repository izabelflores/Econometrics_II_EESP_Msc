#### Illustrate convergence in distribution

# Collect the CDF of our normalized coefficient for each sample size

# Create a data frame to store the results



convergence_distribution <- function(a) {
  temp <- data.frame(
    "capT" = rep(NA, 6 * 101),
    "Fy" = rep(NA, 6 * 101),
    "Qy" = rep(NA, 6 * 101),
    
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
