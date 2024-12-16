# Data for the new drug and standard drug
new_drug <- c(10, 22, 12, 15, 17, 19, 23)  # Replace + with actual values
standard_drug <- c(7, 11, 14, 17, 18, 18, 19)

# Event indicator (1 = event occurred, 0 = censored)
# 1 means the patient died, 0 means censored (we assume no event occurred)
new_drug_event <- c(1, 1, 0, 0, 0, 0, 0)  # Censoring for new drug: 12+, 15+, 17+, 19+, 23+
standard_drug_event <- c(1, 1, 1, 1, 1, 1, 1)  # No censoring for standard drug

# Combine the data
time_data <- c(new_drug, standard_drug)
event_data <- c(new_drug_event, standard_drug_event)
group_data <- c(rep(1, length(new_drug)), rep(2, length(standard_drug)))  # 1 = new drug, 2 = standard drug

# Create a data frame
survival_data <- data.frame(
  time = time_data,
  event = event_data,
  group = group_data
)

# Sort data by time
survival_data <- survival_data[order(survival_data$time),]

# Perform the log-rank test using the survival package
# This is a simplified version, calculating the log-rank statistic manually

# Step 1: Calculate the observed and expected number of events for each group
observed_new <- 0
observed_standard <- 0
expected_new <- 0
expected_standard <- 0
total_events <- 0

# Calculate observed and expected events at each time point
for (i in 1:nrow(survival_data)) {
  if (survival_data$event[i] == 1) {
    if (survival_data$group[i] == 1) {
      observed_new <- observed_new + 1
    } else {
      observed_standard <- observed_standard + 1
    }
    total_events <- total_events + 1
  }
}

# Compute expected events (based on the null hypothesis that the survival curves are identical)
expected_new <- (observed_new + observed_standard) * (length(new_drug) / length(time_data))
expected_standard <- (observed_new + observed_standard) * (length(standard_drug) / length(time_data))

# Step 2: Calculate the log-rank test statistic
log_rank_stat <- (observed_new - expected_new)^2 / expected_new + (observed_standard - expected_standard)^2 / expected_standard

# Step 3: Calculate the p-value using the chi-squared distribution with 1 degree of freedom
p_value <- 1 - pchisq(log_rank_stat, df = 1)

cat("Log-Rank Test Statistic: ", log_rank_stat, "\n")
cat("p-value: ", p_value, "\n")
