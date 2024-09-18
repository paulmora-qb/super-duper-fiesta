# Exercise 1

#' Title
#' #'
#' Description
#'
#' @param wind_speed List of wind-speeds for which we would like to know the
#' power in watts.
#'
#' @export Returns a list of power values in watts.
#'
#' @example /
#'
power_knot <- function(wind_speed) {
    if (wind_speed < 3.5) {
        return(0)
    } else if (wind_speed <= 14) {
        return(exp(-213.974 * exp(-0.633 * wind_speed)))
    } else if (wind_speed <= 25) {
        return(1)
    } else {
        return(0)
    }
}

#' Plotting function
#' #'
#' This function takes in two vectors, x and y, and plots them on a graph.
#'
#' @param x A vector of x-coordinates.
#' @param y A vector of y-coordinates.
#'
#' @export Nothing as the function only plots and saves the plot.
#'
#' @example /
#'
plot_and_save_png <- function(
    x,
    y,
    x_label,
    y_label,
    main_title,
    output_path,
    width = 400,
    height = 200) {
    # Check if x and y are the same length
    if (length(x) != length(y)) {
        stop("x and y must be of the same length.")
    }

    # Check if output_path ends with ".png"
    if (!grepl("\\.png$", output_path, ignore.case = TRUE)) {
        stop("Output path must end with '.png'")
    }

    # Open the graphics device for PNG
    png(filename = output_path, width = width, height = height)

    # Create the plot
    plot(
        x,
        y,
        type = "l", # type of plot
        xlab = x_label, # x-axis label
        ylab = y_label, # y-axis label
        main = main_title # plot title
    )

    # Close the graphics device
    dev.off()
}

wind_speed_list <- seq(0, 40)
watt_list <- lapply(wind_speed_list, power_knot)
normalized_wind_speed_list <- wind_speed_list / max(wind_speed_list)
plot_and_save_png(
    x = normalized_wind_speed_list,
    y = watt_list,
    x_label = "Wind Speed (knots)",
    y_label = "Power (Watts)",
    main_title = "Wind turbine power output",
    output_path = "./practicals/practical_1/data/output/wind_power.png"
)
