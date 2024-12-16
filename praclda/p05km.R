# Kaplan-Meier function to estimate survival probabilities
kaplan_meier <- function(time, status) {
  # Combine time and status into a data frame and sort by time
  df <- data.frame(time = time, status = status)
  df <- df[order(df$time), ]
  
  # Initialize survival probabilities (first survival probability is 1)
  df$survival_prob <- rep(1, nrow(df))
  
  # Calculate survival probabilities at each time point
  for (i in 1:nrow(df)) {
    # Calculate the number at risk at time[i]
    at_risk <- sum(df$time >= df$time[i])
    
    # If the event occurred (status = 1), calculate survival probability
    if (df$status[i] == 1) {
      if (i == 1) {
        df$survival_prob[i] <- 1 - 1 / at_risk  # First time point, initial survival probability
      } else {
        df$survival_prob[i] <- df$survival_prob[i - 1] * (1 - 1 / at_risk)
      }
    } else {
      df$survival_prob[i] <- df$survival_prob[i - 1]  # If censored, carry forward previous survival probability
    }
  }
  
  return(df)
}

# Hazard function to estimate hazard rates at each time point
hazard_rate <- function(time, status) {
  # Combine time and status into a data frame and sort by time
  df <- data.frame(time = time, status = status)
  df <- df[order(df$time), ]
  
  # Initialize hazard rates
  df$hazard_rate <- rep(0, nrow(df))
  
  # Calculate hazard rates at each time point
  for (i in 1:nrow(df)) {
    # Calculate the number at risk at time[i]
    at_risk <- sum(df$time >= df$time[i])
    
    # If the event occurred (status = 1), calculate hazard rate
    if (df$status[i] == 1) {
      df$hazard_rate[i] <- 1 / at_risk
    }
  }
  
  return(df)
}

# Data for both groups (time in weeks)
group1 <- c(6, 6, 6, 7, 10, 13, 16, 22, 23, 6, 9, 10, 11, 17, 19, 20, 25, 32, 32, 34, 35)  # Treatment
group1_censored <- c(6, 9, 10, 11, 17, 19, 20, 25, 32, 32, 34, 35)  # Censored times for Treatment

group2 <- c(1, 1, 2, 2, 3, 4, 4, 5, 5, 8, 8, 8, 8, 11, 11, 12, 12, 15, 17, 22, 23)  # Placebo

# Combine data into a single data frame (with censoring for Group 1)
treatment_data <- data.frame(time = c(group1, group1_censored), status = c(rep(1, length(group1)), rep(0, length(group1_censored))))
placebo_data <- data.frame(time = group2, status = rep(1, length(group2)))

# Apply Kaplan-Meier estimator and hazard rate estimator for both groups
treatment_km <- kaplan_meier(treatment_data$time, treatment_data$status)
placebo_km <- kaplan_meier(placebo_data$time, placebo_data$status)

treatment_hazard <- hazard_rate(treatment_data$time, treatment_data$status)
placebo_hazard <- hazard_rate(placebo_data$time, placebo_data$status)

# Plot the Kaplan-Meier survival curves
par(mfrow = c(1, 2))  # Set up a 1x2 plotting layout

# Plot Survival Curves
plot(treatment_km$time, treatment_km$survival_prob, type = "s", col = "blue", lwd = 2, 
     xlab = "Time (weeks)", ylab = "Survival Probability", main = "Kaplan-Meier Survival Curve")
lines(placebo_km$time, placebo_km$survival_prob, type = "s", col = "red", lwd = 2)
legend("topright", legend = c("Treatment", "Placebo"), col = c("blue", "red"), lwd = 2)

# Plot Hazard Curves
plot(treatment_hazard$time, treatment_hazard$hazard_rate, type = "s", col = "blue", lwd = 2, 
     xlab = "Time (weeks)", ylab = "Hazard Rate", main = "Hazard Curve")
lines(placebo_hazard$time, placebo_hazard$hazard_rate, type = "s", col = "red", lwd = 2)
legend("topright", legend = c("Treatment", "Placebo"), col = c("blue", "red"), lwd = 2)

# Reset layout
par(mfrow = c(1, 1))

