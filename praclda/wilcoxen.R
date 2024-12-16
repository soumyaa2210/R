# Data for the two groups
new_drug <- c(10, 22, 12, 15, 17, 19, 23)  # Observed times for new drug (ignoring '+' for censored)
standard_drug <- c(7, 11, 14, 17, 18, 18, 19)

# Combine the data
combined_data <- c(new_drug, standard_drug)

# Rank all the combined data
ranks <- rank(combined_data)

# Split the ranks for each group
new_drug_ranks <- ranks[1:length(new_drug)]
standard_drug_ranks <- ranks[(length(new_drug) + 1):length(combined_data)]

# Sum the ranks for each group
sum_ranks_new_drug <- sum(new_drug_ranks)
sum_ranks_standard_drug <- sum(standard_drug_ranks)

# Calculate the U statistic for each group
n1 <- length(new_drug)
n2 <- length(standard_drug)

U1 <- sum_ranks_new_drug - (n1 * (n1 + 1)) / 2
U2 <- sum_ranks_standard_drug - (n2 * (n2 + 1)) / 2

# The smaller of the two U statistics is used for the test
U <- min(U1, U2)

# Print the U statistic
cat("U statistic: ", U, "\n")

# Calculate the mean and variance of U for the normal approximation
mean_U <- (n1 * n2) / 2
var_U <- (n1 * n2 * (n1 + n2 + 1)) / 12

# Standardize the U statistic (z-score)
z <- (U - mean_U) / sqrt(var_U)

# Calculate the p-value using the standard normal distribution (two-tailed test)
p_value <- 2 * (1 - pnorm(abs(z)))

# Print the results
cat("Z-score: ", z, "\n")
cat("P-value: ", p_value, "\n")

# Conclusion based on p-value
if (p_value < 0.05) {
  cat("There is a significant difference in survival between the two groups.\n")
} else {
  cat("There is no significant difference in survival between the two groups.\n")
}
