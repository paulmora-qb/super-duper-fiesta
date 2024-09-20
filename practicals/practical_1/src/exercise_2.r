# Exercise 2

source("practicals/practical_1/src/utils.r")


#' Return Dweibull PDF.
#' #'
#' This function takes in a vector of x-values and returns the Dweibull PDF.
#'
#' @param x_values A vector of x-values.
#' @param k Shape parameter.
#' @param lambda Scale parameter.
#'
#' @export Returns a vector of Dweibull PDF values.
#'
#' @example
#'
dweibull_pdf <- function(
    x_values,
    k,
    lambda) {
    return(dweibull(x_values, shape = k, scale = lambda))
}


# Load the data
data <- read.csv("practicals/practical_1/data/input/WindData.csv")
dweibull_y_values <- dweibull_pdf(x_values = data$speed, k = 1.679, lambda = 10.128)
utils_plot_and_save_png(
    x = x_values,
    y = dweibull_y_values,
    x_label = "Hourly wind speed (knots)",
    y_label = "Probability Density",
    main_title = "Dweibull PDF",
    output_path = "./practicals/practical_1/data/output/dweibull_pdf.png"
)
