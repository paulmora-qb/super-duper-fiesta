source("./practicals/practical_1/src/exercise_1.r")
source("./practicals/practical_1/src/exercise_5.r")
library(ggplot2)


#' Calculate the power for a range of wind-speeds
#' #'
#' Description
#'  This function calculates the power output of a wind turbine for a range of
#' wind-speeds.
#' @param k The shape parameter of the Weibull distribution.
#' @param lambda The scale parameter of the Weibull distribution.
#' @param m The number of samples to generate.
#'
#' @export Returns a list of power values.
#'
#' @example
#'
calculate_average_power_per_day <- function(k, lambda, m) {
  y_values <- runif(m, 0, 1)
  samples <- inverse_weibull(y_values = y_values, k = k, lambda = lambda)
  return(lapply(samples, function(x) power_knot(wind_speed = x)))
}

#' Create and save boxplot
#' #'
#' Description
#' This function creates a boxplot and saves it to the specified file path.
#'
#' @param data The data frame containing the data to be plotted.
#' @param x_col The name of the column to be plotted on the x-axis.
#' @param y_col The name of the column to be plotted on the y-axis.
#' @param x_title The title of the x-axis.
#' @param y_title The title of the y-axis.
#' @param save_path The file path to save the plot to.
#'
#' @export None
#'
#' @example
#'
create_boxplot <- function(data, x_col, y_col, x_title, y_title, save_path) {
  # Create the boxplot
  plot <- ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = "Boxplot", x = x_title, y = y_title) +
    theme_minimal()

  # Save the plot to the specified file path
  ggsave(save_path, plot = plot, width = 8, height = 6, dpi = 300)

  # Print a message indicating the plot has been saved
  message(paste("Plot saved to:", save_path))
}

k <- 1.679
lambda <- 10.128
hours_of_the_day <- 24
m <- 1000
samples <- calculate_average_power_per_day(k = k, lambda = lambda, m = m)

boxplot_df <- data.frame(power = unlist(samples))
create_boxplot(
  data = boxplot_df,
  x_col = "power",
  y_col = NULL,
  x_title = "Power Percentage",
  y_title = NULL,
  save_path = "./practicals/practical_1/data/output/boxplot.png"
)

percentage_of_0 <- sum(samples == 0) / length(samples)
percentage_of_1 <- sum(samples == 1) / length(samples)

paste("Percentage of 0:", percentage_of_0)
paste("Percentage of 1:", percentage_of_1)
