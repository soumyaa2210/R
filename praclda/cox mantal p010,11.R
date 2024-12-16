# The following dataset is collected from a clinical trial in which researchers are testing
# the effectiveness of a new drug compared to a standard drug in increasing the survival
# time of cancer patients. Use a non-parametric method such as the Cox-Mantel test to
# determine if the new drug prolongs survival significantly compared to the standard
# drug.

# New drug  : 10, 22, 12+ , 15+, 17+, 19+, 23+
# Standard drug: 7,11,14,17,18,18,19


lifetimes <- data.frame(
  time = c(10, 22, 12, 15, 17, 19, 23, 7, 11, 14, 17, 18, 18, 19),
  event = c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
  group = c(rep("A", 7), rep("B", 7))
)


n1 <- sum(lifetimes$group == "A")
n2 <- sum(lifetimes$group == "B")
n <- n1 + n2

r1 <- sum(lifetimes$group =="A" & lifetimes$event == 1)
r2 <- sum(lifetimes$group =="B" & lifetimes$event == 1)

df_cen <- lifetimes[lifetimes$event == 1, ]
time_counts <- data.frame(table(df_cen$time))
time_counts$Var1 <- as.numeric(as.character(time_counts$Var1))  

df <- data.frame(time = c(), m = c(), G1 = c(), G2 = c(), R = c(), A = c())


for (i in 1:nrow(time_counts)) {
  time_point <- time_counts$Var1[i]
  
  G1 <- sum(lifetimes$group == "A" & lifetimes$time >= time_point)
  G2 <- sum(lifetimes$group == "B" & lifetimes$time >= time_point)
  
  R <- G1 + G2
  A <- G2 / R  
  
  df_row <- data.frame(
    time = time_point,
    m = time_counts$Freq[i],
    G1 = G1,
    G2 = G2,
    R = R,
    A = A
  )
  df <- rbind(df, df_row)
}
print(df)

U <- r2 - sum(df$m * df$A)

I <- sum(df$m* (df$R - df$m)*df$A*(1-df$A)/(df$R - 1))

Z <- U/sqrt(I)
cat("Cox-Mantel Test Statistic (U):", U)