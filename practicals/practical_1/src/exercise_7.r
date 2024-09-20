source("./practicals/practical_1/src/exercise_5.r")


calculate_sd_n_times <- function(
    k,
    lambda,
    num_samples) {
  y_values <- runif(num_samples, 0, 1)
  sample <- inverse_weibull(y_values = y_values, k = k, lambda = lambda)
  return(mean(sample))
}


calculate_sd_for_sample_size <- function(
    num_samples,
    k,
    lambda,
    m) {
  means_for_n_samples <- replicate(
    m,
    calculate_sd_n_times(k = k, lambda = lambda, num_samples = num_samples)
  )
  return(sd(means_for_n_samples) / sqrt(num_samples))
}



k <- 1.679
lambda <- 10.128
n_samp <- seq(5, 100, 5)
m <- 1000
lapply(
  n_samp,
  function(x) {
    calculate_sd_for_sample_size(
      num_samples = x,
      k = k,
      lambda = lambda,
      m = m
    )
  }
)
