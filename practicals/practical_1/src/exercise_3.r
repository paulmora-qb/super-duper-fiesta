library(ggplot2)

#' Create a weibull reject sampler.
#' #'
#' Description
#' This function builds a Weibull rejection sampler to generate random samples
#' from a Weibull distribution with shape parameter k and scale parameter
#' lambda.
#' @param n The number of samples to generate.
#' @param k The shape parameter of the Weibull distribution.
#' @param lambda The scale parameter of the Weibull distribution.
#'
#' @export Returns a vector of n samples from the Weibull distribution.
#'
#' @example
#'
weibull_rejection_sampler <- function(n, k, lambda) {
    # Initialize a vector to store the samples
    samples <- numeric(n)
    count <- 0

    # Define the proposal distribution (e.g., exponential distribution)
    proposal_lambda <- lambda / (k^(1 / k)) # Scale for the exponential proposal

    # Find the maximum of the Weibull PDF for normalization
    max_weibull <- dweibull(lambda, shape = k, scale = lambda)

    while (count < n) {
        # Sample from the proposal distribution
        x <- rexp(1, rate = 1 / proposal_lambda)

        # Sample a uniform random number
        u <- runif(1, 0, max_weibull)

        # Acceptance criterion
        if (u < dweibull(x, shape = k, scale = lambda)) {
            count <- count + 1
            samples[count] <- x
        }
    }

    return(samples)
}

# Exercise 3)
n_samples <- 1000
shape_param <- 2
scale_param <- 1
samples <- weibull_rejection_sampler(n_samples, shape_param, scale_param)

# Prepare the data for plotting
mean_value <- mean(samples)
df_plotting <- data.frame(x = samples)

# Create the plot
plot <- ggplot(data = df_plotting, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", alpha = 0.5) +
    geom_density(color = "red", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    geom_vline(xintercept = mean_value, color = "green", linetype = "dashed", linewidth = 1) +
    annotate("text",
        x = mean_value, y = 0.1, label = paste("Mean =", round(mean_value, 2)),
        color = "green", vjust = -1
    ) + # Annotate mean
    labs(
        title = "Histogram with Overlayed Density Curve",
        x = "Value",
        y = "Density"
    ) +
    theme_minimal()

# Save the plot
ggsave("./practicals/practical_1/data/output/dweibull_rejection_sampler.png", plot = plot, width = 8, height = 6, dpi = 300)
