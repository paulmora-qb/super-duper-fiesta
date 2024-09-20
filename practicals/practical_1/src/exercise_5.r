#' Inverse Weibull
#' #'
#' Description
#'  This function computes the inverse Weibull distribution.
#'
#' @param y_values A vector of y-values.
#' @param k The shape parameter of the Weibull distribution.
#' @param lambda The scale parameter of the Weibull distribution.
#'
#' @export Returns a vector of x-values.
#'
#' @example
#'
inverse_weibull <- function(y_values, k, lambda) {
    return(qweibull(y_values, shape = k, scale = lambda))
}


y_values <- runif(1000, 0, 1)
x_values <- inverse_weibull(y_values = y_values, k = 1.679, lambda = 10.128)
mean(x_values)
