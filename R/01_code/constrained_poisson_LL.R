
# constrained_poisson_LL --------------------------------------------------

constrained_poisson_LL <- function(.X, 
                                   .y, 
                                   .theta, 
                                   .prior_weight = 0,
                                   .prior_vector = rep(0, length(.theta)),
                                   .ridge_penalty = TRUE){
  n <- length(.y)
  lambda <- exp(as.matrix(.X) %*% .theta)
  
  penalty_term <- ifelse(.ridge_penalty,
                         .prior_weight * sum((.theta - .prior_vector)^2),
                         .prior_weight * sum(abs(.theta - .prior_vector)))
  
  
  LL <- sum(.y * log(lambda) - lambda) - penalty_term
  
  return(LL)
}