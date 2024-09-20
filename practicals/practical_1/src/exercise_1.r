# Exercise 1

source("practicals/practical_1/src/utils.r")


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

wind_speed_list <- seq(0, 40)
watt_list <- lapply(wind_speed_list, power_knot)
normalized_wind_speed_list <- wind_speed_list / max(wind_speed_list)
utils_plot_and_save_png(
  x = normalized_wind_speed_list,
  y = watt_list,
  x_label = "Wind Speed (knots)",
  y_label = "Power (Watts)",
  main_title = "Wind turbine power output",
  output_path = "./practicals/practical_1/data/output/wind_power.png"
)
