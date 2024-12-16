# Set parameters for the Log-Normal distribution
lognormal_meanlog <- 1   # Mean of the log of the distribution
lognormal_sdlog <- 0.5   # Standard deviation of the log of the distribution

# Number of patients
n <- 500

# ---- Generate lifetime data from the Log-Normal distribution ----
lifetime_data_lognormal <- rlnorm(n, meanlog = lognormal_meanlog, sdlog = lognormal_sdlog)

# ---- Censoring Schemes ----
apply_censoring_lognormal <- function(lifetime_data) {
  # ---- Right Censoring ----
  right_censor_time <- 15
  right_censored_data <- pmin(lifetime_data, right_censor_time)
  
  # ---- Type-I Censoring ----
  type1_censor_time <- 12
  type1_censored_data <- pmin(lifetime_data, type1_censor_time)
  
  # ---- Type-II Censoring ----
  failure_count <- 200
  sorted_data <- sort(lifetime_data)
  type2_censored_data <- lifetime_data
  if (n > failure_count) {
    type2_censored_data[lifetime_data > sorted_data[failure_count]] <- sorted_data[failure_count]
  }
  
  # ---- Random Censoring ----
  random_censor_times <- runif(n, min = 10, max = 20)
  random_censored_data <- pmin(lifetime_data, random_censor_times)
  
  # ---- Left Censoring ----
  left_censor_times <- runif(n, min = 0, max = 5)
  left_censored_data <- pmax(lifetime_data, left_censor_times)
  
  # ---- Interval Censoring ----
  interval_lower <- runif(n, min = 0, max = 5)
  interval_upper <- runif(n, min = 5, max = 15)
  interval_censored_data <- runif(n, min = interval_lower, max = interval_upper)
  
  return(list(
    complete_data = lifetime_data,
    right_censored_data = right_censored_data,
    type1_censored_data = type1_censored_data,
    type2_censored_data = type2_censored_data,
    random_censored_data = random_censored_data,
    left_censored_data = left_censored_data,
    interval_censored_data = interval_censored_data
  ))
}

# Apply censoring schemes for Log-Normal data
censoring_lognormal <- apply_censoring_lognormal(lifetime_data_lognormal)

# ---- Print Patients' Lifetime Data for Log-Normal Distribution ----
cat("\n---- Log-Normal Distribution - Patients' Lifetime (Complete Data) ----\n")
print(head(censoring_lognormal$complete_data))  # Display the first few values for complete data

cat("\n---- Log-Normal Distribution - Patients' Lifetime (Right Censoring) ----\n")
print(head(censoring_lognormal$right_censored_data))  # Display the first few values for right censored data

cat("\n---- Log-Normal Distribution - Patients' Lifetime (Type-I Censoring) ----\n")
print(head(censoring_lognormal$type1_censored_data))  # Display the first few values for type-I censored data

cat("\n---- Log-Normal Distribution - Patients' Lifetime (Type-II Censoring) ----\n")
print(head(censoring_lognormal$type2_censored_data))  # Display the first few values for type-II censored data

cat("\n---- Log-Normal Distribution - Patients' Lifetime (Random Censoring) ----\n")
print(head(censoring_lognormal$random_censored_data))  # Display the first few values for random censored data

cat("\n---- Log-Normal Distribution - Patients' Lifetime (Left Censoring) ----\n")
print(head(censoring_lognormal$left_censored_data))  # Display the first few values for left censored data

cat("\n---- Log-Normal Distribution - Patients' Lifetime (Interval Censoring) ----\n")
print(head(censoring_lognormal$interval_censored_data))  # Display the first few values for interval censored data
