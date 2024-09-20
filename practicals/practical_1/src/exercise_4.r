source("./practicals/practical_1/src/exercise_3.r")


#' Conduct a Monte Carlo simulation on the rejection sampler.
#' #'
#' This function conducts a Monte Carlo simulation on the rejection sampler.
#' It samples from the Weibull distribution and computes the mean power values.
#'
#' @param m The number of Monte Carlo simulations to run.
#' @param n The number of samples to generate.
#' @param k The shape parameter of the Weibull distribution.
#' @param lambda The scale parameter of the Weibull distribution.
#'
#' @export Returns the standard deviation of the mean power values.
#'
#' @example
#'
compute_mean_power_std <- function(m, n, k, lambda) {
  # Initialize a vector to store the mean power values
  mean_power_values <- numeric(m)

  for (i in 1:m) {
    # Sample from the Weibull distribution
    samples <- weibull_rejection_sampler(n, k, lambda)
    mean_power_values[i] <- mean(samples)
  }

  return(sd(mean_power_values) / sqrt(m))
}

n_samples <- 1000
shape_param <- 2
scale_param <- 1
m <- 100

result <- compute_mean_power_std(m, n_samples, shape_param, scale_param)
