# Set parameters
lambda <- 0.1  # rate parameter for exponential distribution
n <- 100  # number of patients

# Generate random lifetimes from exponential distribution
lifetimes <- rexp(n, rate = lambda)

# Display basic statistics of the generated lifetimes
cat("First 10 Generated Lifetimes:\n")
print(head(lifetimes, 10))
cat("Mean Lifetime: ", mean(lifetimes), "\n")
# Set censoring parameters
C <- 12  # Right censoring threshold
T_type1 <- 12  # Type-I censoring time limit
r_type2 <- 50  # Type-II censoring (observe first 50 failures)
random_censoring_rate <- 0.15  # For random censoring, 15% censoring rate
left_censoring_time <- 2  # Left censoring starts after time 2
interval_start <- 5  # Interval censoring starts at time 5
interval_end <- 10  # Interval censoring ends at time 10

# Censoring schemes
# 1. Right Censoring
right_censored_lifetimes <- pmin(lifetimes, C)

# 2. Type-I Censoring Scheme
type1_censored_lifetimes <- pmin(lifetimes, T_type1)

# 3. Type-II Censoring Scheme
# Sort lifetimes and censor at the r-th event
sorted_lifetimes <- sort(lifetimes)
type2_censored_lifetimes <- rep(NA, n)
type2_censored_lifetimes[1:r_type2] <- sorted_lifetimes[1:r_type2]
type2_censored_lifetimes[(r_type2+1):n] <- sorted_lifetimes[r_type2]

# 4. Random Censoring
random_censoring_times <- rexp(n, rate = random_censoring_rate)
random_censored_lifetimes <- pmin(lifetimes, random_censoring_times)

# 5. Left Censoring
left_censored_lifetimes <- pmax(lifetimes, left_censoring_time)

# 6. Interval Censoring
interval_censored_lifetimes <- ifelse(lifetimes >= interval_start & lifetimes <= interval_end, lifetimes, NA)

# Display some of the censored data
cat("First 10 Right Censored Lifetimes:\n")
print(head(right_censored_lifetimes, 10))

cat("First 10 Type-I Censored Lifetimes:\n")
print(head(type1_censored_lifetimes, 10))

cat("First 10 Type-II Censored Lifetimes:\n")
print(head(type2_censored_lifetimes, 10))

cat("First 10 Random Censored Lifetimes:\n")
print(head(random_censored_lifetimes, 10))

cat("First 10 Left Censored Lifetimes:\n")
print(head(left_censored_lifetimes, 10))

cat("First 10 Interval Censored Lifetimes:\n")
print(head(interval_censored_lifetimes, 10))


# Plot histograms of the complete and censored data
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# Complete data histogram
hist(lifetimes, breaks = 20, col = rgb(0, 0, 1, 0.5), main = "Complete Data Distribution", xlab = "Lifetime")

# Censored data histograms
hist(right_censored_lifetimes, breaks = 20, col = rgb(0, 1, 0, 0.5), main = "Right Censored Data", xlab = "Lifetime")
hist(type1_censored_lifetimes, breaks = 20, col = rgb(1, 0, 0, 0.5), main = "Type-I Censored Data", xlab = "Lifetime")
hist(type2_censored_lifetimes, breaks = 20, col = rgb(1, 0, 1, 0.5), main = "Type-II Censored Data", xlab = "Lifetime")
hist(random_censored_lifetimes, breaks = 20, col = rgb(0, 1, 1, 0.5), main = "Random Censored Data", xlab = "Lifetime")
hist(left_censored_lifetimes, breaks = 20, col = rgb(1, 1, 0, 0.5), main = "Left Censored Data", xlab = "Lifetime")

# Reset plotting layout
par(mfrow = c(1, 1))

cat("\nMean and Median Comparison:\n")
cat("Mean of Complete Data: ", mean(lifetimes), "\n")
cat("Median of Complete Data: ", median(lifetimes), "\n")

cat("Mean of Right Censored Data: ", mean(right_censored_lifetimes, na.rm = TRUE), "\n")
cat("Median of Right Censored Data: ", median(right_censored_lifetimes, na.rm = TRUE), "\n")

cat("Mean of Type-I Censored Data: ", mean(type1_censored_lifetimes, na.rm = TRUE), "\n")
cat("Median of Type-I Censored Data: ", median(type1_censored_lifetimes, na.rm = TRUE), "\n")

cat("Mean of Type-II Censored Data: ", mean(type2_censored_lifetimes, na.rm = TRUE), "\n")
cat("Median of Type-II Censored Data: ", median(type2_censored_lifetimes, na.rm = TRUE), "\n")

cat("Mean of Random Censored Data: ", mean(random_censored_lifetimes, na.rm = TRUE), "\n")
cat("Median of Random Censored Data: ", median(random_censored_lifetimes, na.rm = TRUE), "\n")

cat("Mean of Left Censored Data: ", mean(left_censored_lifetimes, na.rm = TRUE), "\n")
cat("Median of Left Censored Data: ", median(left_censored_lifetimes, na.rm = TRUE), "\n")

cat("Mean of Interval Censored Data: ", mean(interval_censored_lifetimes, na.rm = TRUE), "\n")
cat("Median of Interval Censored Data: ", median(interval_censored_lifetimes, na.rm = TRUE), "\n")

