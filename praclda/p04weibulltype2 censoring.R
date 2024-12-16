# Parameters
set.seed(123)
n_values <- c(100, 200, 300)     # Sample sizes
true_shape <- 2                  # True Weibull shape parameter
true_scale <- 3                  # True Weibull scale parameter
c2 <- 80                         # Number of observed events before censoring (Type-II)
simulation <- 1000               # Number of simulations

# Log-likelihood function for Type-II censored Weibull data
weibull_log_likelihood <- function(params, data, c2) {
  shape <- params[1]; scale <- params[2]
  uncensored <- data[1:c2]                    # First c2 events are observed (uncensored)
  censored_part <- (length(data) - c2) * log(pweibull(data[c2], shape, scale, lower.tail = FALSE))
  ll_uncensored <- sum(dweibull(uncensored, shape, scale, log = TRUE))
  return(-(ll_uncensored + censored_part))    # Negative log-likelihood
}

# Performance Evaluation for Type-II Censoring
evaluate_performance <- function(n, shape, scale, c2, simulation) {
  shape_estimates <- scale_estimates <- numeric(simulation)
  
  for (i in 1:simulation) {
    # Generate and sort Weibull sample
    life <- sort(rweibull(n, shape, scale))
    
    # Estimate parameters using MLE on censored data
    fit <- optim(par = c(1, 1), fn = weibull_log_likelihood, data = life,
                 c2 = c2, method = "L-BFGS-B", lower = c(0.1, 0.1))
    shape_estimates[i] <- fit$par[1]
    scale_estimates[i] <- fit$par[2]
  }
  
  # Calculate bias, variance, and MSE
  return(list(
    bias_shape = mean(shape_estimates) - shape, 
    bias_scale = mean(scale_estimates) - scale,
    var_shape = var(shape_estimates), 
    var_scale = var(scale_estimates),
    mse_shape = mean((shape_estimates - shape)^2),
    mse_scale = mean((scale_estimates - scale)^2)
  ))
}

# Run simulations and collect results for different sample sizes
results <- data.frame()
for (n in n_values) {
  perf <- evaluate_performance(n, true_shape, true_scale, c2, simulation)
  results <- rbind(results, data.frame(
    n = n, shape_hat = true_shape - perf$bias_shape, scale_hat = true_scale - perf$bias_scale,
    bias_shape = perf$bias_shape, bias_scale = perf$bias_scale, 
    var_shape = perf$var_shape, var_scale = perf$var_scale,
    mse_shape = perf$mse_shape, mse_scale = perf$mse_scale
  ))
}

print(results)
