
# Elasticity estimation with non-bayesian priors --------------------------

rm(list = ls())

# packages
library(ggplot2)
library(dplyr)
library(tidyr)
source("01_code/non_bayesian_priors_functions.R")

# simulate data
N <- 1000
X <- data.frame(x1 = rnorm(N),
                x2 = runif(N),
                x3 = sample(c(1,0), N, replace = TRUE))
theta <- c(2.3, -3.7, 1.03)
initial_values <- rnorm(3) # guess values 
prior <- c(-1.2, 3.4, -1.04) # priors 
lambda <- exp(as.matrix(X) %*% theta)
y <- rpois(nrow(X), lambda = lambda)

# example 
ml_estimates(.data = list(X = X, y = y),
             .pars = initial_values,
             .prior_weight = 4.4,
             .prior_params = prior)

# test how weighting priors affects estimates 
w_seq <- seq(0, 10000, by = 200)

df_1 <- penalized_coefficients(.df = list(X = X, y = y),
                               .initial_values = initial_values,
                               .weights = w_seq, 
                               .prior = prior,
                               .ridge_penalty = TRUE)


df_2 <- penalized_coefficients(.df = list(X = X, y = y),
                               .initial_values = initial_values,
                               .weights = w_seq, 
                               .prior = prior,
                               .ridge_penalty = FALSE)

plot_params(df_1)
plot_params(df_2)
