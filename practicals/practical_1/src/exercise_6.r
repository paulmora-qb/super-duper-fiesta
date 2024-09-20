source("./practicals/practical_1/src/exercise_3.r")
source("./practicals/practical_1/src/exercise_5.r")


# Weibull rejection sampler
n <- 10000
k <- 1.679
lambda <- 10.128
system.time(weibull_rejection_sampler(n = n, k = k, lambda = lambda))

# Inverse Weibull
y_values <- runif(n, 0, 1)
system.time(inverse_weibull(y_values = y_values, k = k, lambda = lambda))


# This first function is to be preferred since it also works without having the
# inverse cumulative function readily available. The second function though
# seems to be faster.
