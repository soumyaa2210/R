# Set the parameters for the Weibull distribution
shape <- 1.5  # Shape parameter (k)
scale <- 10   # Scale parameter (λ)

# Number of patients
n <- 500

# Generate lifetime data from the Weibull distribution
lifetime_data <- scale * (-log(runif(n)))^(1 / shape)

# ---- Right Censoring ----
right_censor_time <- 15
right_censored_data <- pmin(lifetime_data, right_censor_time)
right_censored <- lifetime_data > right_censor_time

# ---- Type-I Censoring ----
type1_censor_time <- 12
type1_censored_data <- pmin(lifetime_data, type1_censor_time)
type1_censored <- lifetime_data > type1_censor_time

# ---- Type-II Censoring ----
failure_count <- 200
sorted_data <- sort(lifetime_data)
type2_censored_data <- lifetime_data
if (n > failure_count) {
  type2_censored_data[lifetime_data > sorted_data[failure_count]] <- sorted_data[failure_count]
}
type2_censored <- lifetime_data > max(type2_censored_data)

# ---- Random Censoring ----
random_censor_times <- runif(n, min = 10, max = 20)
random_censored_data <- pmin(lifetime_data, random_censor_times)
random_censored <- lifetime_data > random_censor_times

# ---- Left Censoring ----
left_censor_times <- runif(n, min = 0, max = 5)
left_censored_data <- pmax(lifetime_data, left_censor_times)
left_censored <- lifetime_data < left_censor_times

# ---- Interval Censoring ----
interval_lower <- runif(n, min = 0, max = 5)
interval_upper <- runif(n, min = 5, max = 15)
interval_censored_data <- runif(n, min = interval_lower, max = interval_upper)
interval_censored <- (lifetime_data < interval_lower) | (lifetime_data > interval_upper)

# ---- Plot Histograms ----
# Plot complete data
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
hist(lifetime_data, main = "Complete Data", xlab = "Lifetime", col = "lightblue", border = "blue", xlim = c(0, 25))

# Right Censored Data
hist(right_censored_data, main = "Right Censoring", xlab = "Lifetime", col = "lightcoral", border = "red", xlim = c(0, 25))

# Type-I Censored Data
hist(type1_censored_data, main = "Type-I Censoring", xlab = "Lifetime", col = "lightgreen", border = "green", xlim = c(0, 25))

# Type-II Censored Data
hist(type2_censored_data, main = "Type-II Censoring", xlab = "Lifetime", col = "orange", border = "orange", xlim = c(0, 25))

# Random Censored Data
hist(random_censored_data, main = "Random Censoring", xlab = "Lifetime", col = "purple", border = "purple", xlim = c(0, 25))

# Left Censored Data
hist(left_censored_data, main = "Left Censoring", xlab = "Lifetime", col = "brown", border = "brown", xlim = c(0, 25))

# Reset plotting layout
par(mfrow = c(1, 1))

# ---- Compute Summary Statistics ----
# Function to compute summary statistics
compute_stats <- function(data) {
  return(c(mean = mean(data), median = median(data), variance = var(data)))
}

# Summary statistics for complete data
complete_stats <- compute_stats(lifetime_data)

# Summary statistics for each censored dataset
right_censored_stats <- compute_stats(right_censored_data)
type1_censored_stats <- compute_stats(type1_censored_data)
type2_censored_stats <- compute_stats(type2_censored_data)
random_censored_stats <- compute_stats(random_censored_data)
left_censored_stats <- compute_stats(left_censored_data)
interval_censored_stats <- compute_stats(interval_censored_data)

# Display summary statistics
cat("\nComplete Data Stats:\n")
print(complete_stats)

cat("\nRight Censored Data Stats:\n")
print(right_censored_stats)

cat("\nType-I Censored Data Stats:\n")
print(type1_censored_stats)

cat("\nType-II Censored Data Stats:\n")
print(type2_censored_stats)

cat("\nRandom Censored Data Stats:\n")
print(random_censored_stats)

cat("\nLeft Censored Data Stats:\n")
print(left_censored_stats)

cat("\nInterval Censored Data Stats:\n")
print(interval_censored_stats)

