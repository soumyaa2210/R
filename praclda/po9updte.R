# Step 1: Data preparation
# 6-MP patients' remission times (uncensored and censored data)
remission_times <- c(6, 6, 6, 7, 9, 10, 10, 11, 13, 16, 17, 19, 20, 22, 23, 25, 32, 32, 34, 35)
censored_times <- c(6, 9, 10, 11, 17, 19, 20, 25, 32, 32, 34)

# Combining uncensored and censored data
times <- c(remission_times, censored_times)
status <- c(rep(1, length(remission_times)), rep(0, length(censored_times)))

# Step 2: ECDF (Empirical Cumulative Distribution Function)
# Sorting the data for ECDF plot
sorted_times <- sort(times)
ecdf_values <- seq(1, length(sorted_times)) / length(sorted_times)

# Plot ECDF
plot(sorted_times, ecdf_values, type = "s", col = "blue", 
     main = "Empirical CDF (Kaplan-Meier) for 6-MP Patients",
     xlab = "Remission Time (weeks)", ylab = "Survival Probability")

# Step 3: Fit Weibull Distribution (Manually)
# We will estimate the shape and scale parameters for the Weibull distribution
# First, calculate the logarithm of the times and use the formula for Weibull parameters
log_times <- log(sorted_times)

# Weibull shape parameter (b) estimation using least squares fitting of log-log transformation
shape <- sum(log_times * (ecdf_values - 1)) / sum((ecdf_values - 1)^2)

# Weibull scale parameter (lambda) using the formula:
scale <- exp(sum(log_times * (ecdf_values - 1)) / sum(ecdf_values - 1))

# Step 4: Hazard Function Plot
# Hazard function for Weibull distribution: h(t) = (b / lambda) * (t / lambda)^(b - 1)
# where b = shape, lambda = scale
hazard_function <- function(t, shape, scale) {
  return((shape / scale) * (t / scale)^(shape - 1))
}

# Generate a sequence of time values to plot the hazard function
time_seq <- seq(1, 40, by = 0.1)
hazard_values <- hazard_function(time_seq, shape, scale)
# Plot the hazard function
plot(time_seq, hazard_values, type = "l", col = "red", lwd = 2, 
     main = "Hazard Function (Weibull Distribution) for 6-MP Patients", 
     xlab = "Time (weeks)", ylab = "Hazard Rate")
