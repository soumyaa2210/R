# Parameters
set.seed(123)
n_values <- c(100, 200, 300)       # Sample sizes
true_shape <- 2                    # True Weibull shape parameter
true_scale <- 3                    # True Weibull scale parameter
censoring_time <- 5                # Censoring time for Type-I censoring
simulation <- 1000                 # Number of simulations

# Log-likelihood function for censored Weibull data
weibull_log_likelihood <- function(params, data, censoring_time) {
  shape <- params[1]; scale <- params[2]
  uncensored <- data[data <= censoring_time]
  censored <- data[data > censoring_time]
  
  # Log-likelihood: uncensored and censored contributions
  ll_uncensored <- sum(dweibull(uncensored, shape, scale, log = TRUE))
  ll_censored <- sum(pweibull(censored, shape, scale, lower.tail = FALSE, log.p = TRUE))
  
  return(-(ll_uncensored + ll_censored))  # Negative log-likelihood
}

# Performance Evaluation for Type-I Censoring
evaluate_performance <- function(n, shape, scale, censoring_time, simulation) {
  shape_estimates <- scale_estimates <- numeric(simulation)
  
  for (i in 1:simulation) {
    # Generate and censor data
    life <- rweibull(n, shape, scale)
    censored_life <- pmin(life, censoring_time)
    
    # Estimate parameters via MLE
    fit <- optim(par = c(1, 1), fn = weibull_log_likelihood, data = censored_life,
                 censoring_time = censoring_time, method = "L-BFGS-B", lower = c(0.1, 0.1))
    shape_estimates[i] <- fit$par[1]
    scale_estimates[i] <- fit$par[2]
  }
  
  # Calculate bias, variance, MSE
  return(list(
    bias_shape = mean(shape_estimates) - shape, 
    bias_scale = mean(scale_estimates) - scale,
    var_shape = var(shape_estimates), 
    var_scale = var(scale_estimates),
    mse_shape = mean((shape_estimates - shape)^2),
    mse_scale = mean((scale_estimates - scale)^2)
  ))
}

# Run simulations and collect results
results <- data.frame()
for (n in n_values) {
  perf <- evaluate_performance(n, true_shape, true_scale, censoring_time, simulation)
  results <- rbind(results, data.frame(
    n = n, shape_hat = true_shape - perf$bias_shape, scale_hat = true_scale - perf$bias_scale,
    bias_shape = perf$bias_shape, bias_scale = perf$bias_scale, 
    var_shape = perf$var_shape, var_scale = perf$var_scale,
    mse_shape = perf$mse_shape, mse_scale = perf$mse_scale
  ))
}

print(results)
