# Input data
time <- c(5, 12, 18, 20, 24, 8, 10, 15, 18, 22)
event <- c(1, 1, 0, 0, 1, 1, 0, 1, 1, 1)  # 1 = death, 0 = censored
group <- c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")

# Split data by group
group_A <- which(group == "A")
group_B <- which(group == "B")

# Function to calculate Kaplan-Meier estimates manually
km_manual <- function(times, events) {
  # Sort by time
  order_idx <- order(times)
  times <- times[order_idx]
  events <- events[order_idx]
  
  # Initialize variables
  survival <- 1
  km_survival <- c(survival)
  n_risk <- length(times)  # Start with all patients at risk
  
  # Iterate through time points
  for (i in 1:length(times)) {
    if (events[i] == 1) {  # Only consider uncensored data (death)
      survival <- survival * (1 - 1 / n_risk)  # Update survival probability
    }
    km_survival <- c(km_survival, survival)
    n_risk <- n_risk - 1  # Reduce the number of people at risk
  }
  
  return(km_survival[-1])  # Return survival probabilities (excluding initial 1)
}

# Calculate KM estimates for Group A and Group B
km_A <- km_manual(time[group_A], event[group_A])
km_B <- km_manual(time[group_B], event[group_B])

# Plot KM estimates for Group A
plot(c(0, time[group_A]), c(1, km_A), type = "s", col = "blue", ylim = c(0, 1),
     xlab = "Time in months", ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curves", lwd = 2, las=1)

# Add KM estimates for Group B
lines(c(0, time[group_B]), c(1, km_B), type = "s", col = "red", lwd = 2)

# Annotate points on Group A curve with survival probabilities
text(c(0, time[group_A]), c(1, km_A), labels = round(c(1, km_A), 2), 
     col = "blue", pos = 3, cex = 0.8)

# Annotate points on Group B curve with survival probabilities
text(c(0, time[group_B]), c(1, km_B), labels = round(c(1, km_B), 2), 
     col = "red", pos = 3, cex = 0.8)

# Add legend to distinguish between groups
legend("bottomleft", legend = c("Group A", "Group B"), col = c("blue", "red"), lwd = 2)

# Display the KM estimates in the console
cat("Kaplan-Meier survival estimates for Group A:\n", round(km_A, 2), "\n")
cat("Kaplan-Meier survival estimates for Group B:\n", round(km_B, 2), "\n")

# Part b data
total_patients <- 10
deaths <- 4
survivors <- total_patients - deaths

# Proportion of patients surviving after 24 months
proportion_surviving <- survivors / total_patients
cat("Proportion of patients surviving after 24 months: ", proportion_surviving, "\n")