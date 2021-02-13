
# Functions for non-bayesian ela estimation -------------------------------

# function to calculate penalized loglikelihood 
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

# estimate parameters with optim
ml_estimates <- function(.data,
                         .pars, 
                         .fn = constrained_poisson_LL,
                         .prior_weight,
                         .prior_params,
                         .ridge_penalty = TRUE){
  
  if(any(!c("X", "y") %in% names(.data)) | 
     !is.list(.data) | 
     any(unlist(lapply(.data, is.null)))) {
    stop(".data must be a list with names X, y.")
  }
  
  X <- .data[["X"]]
  y <- .data[["y"]]
  
  results <- optim(par = .pars, 
                   fn = .fn,
                   .X = X,
                   .y = y,
                   .prior_weight = .prior_weight,
                   .prior_vector = .prior_params,
                   .ridge_penalty = .ridge_penalty,
                   method = "BFGS",
                   control = list(fnscale = -1),
                   hessian = TRUE)
  
  coef_vector <- results$par
  se_vector <- sqrt(-1 * diag(solve(results$hessian)))
  t_vals <- coef_vector / se_vector
  upper_band <- coef_vector + 2 * se_vector
  lower_band <- coef_vector - 2 * se_vector
  
  res_df <- data.frame(
    var_name = colnames(X),
    coefs = coef_vector,
    std_err = se_vector,
    t_values = t_vals,
    conf_lower = lower_band,
    conf_upper = upper_band
  )
  return(res_df)
  
}

# function to calculate estimates for different weights 
penalized_coefficients <- function(.df,
                                   .initial_values,
                                   .func = constrained_poisson_LL, 
                                   .weights,
                                   .prior,
                                   .ridge_penalty 
){
  
  if(any(!c("X", "y") %in% names(.df)) | 
     !is.list(.df) | 
     any(unlist(lapply(.df, is.null)))) {
    stop(".data must be a list with names X, y.")
  }
  
  params <- data.frame(weight = .weights, 
                       param_1 = NA,
                       param_2 = NA,
                       param_3 = NA)
  
  for(i in 1:length(w_seq)){
    params[i,c(2:4)] <- ml_estimates(.data = .df,
                                     .pars = .initial_values,
                                     .fn = .func,
                                     .prior_weight = params[i,1],
                                     .prior_params = .prior,
                                     .ridge_penalty = .ridge_penalty)$coefs
  }
  
  return(params)
}

# function to plot weights against estimates 
plot_params <- function(.df){
  
  .df %>% 
    tidyr::gather(... = -weight,
                  key = "var_name",
                  value = "coef") %>% 
    ggplot2::ggplot(aes(x = weight, 
                        y = coef, 
                        color = var_name)) + 
    ggplot2::geom_point(size = 0.7) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "Weight on Penatly", 
                  y = "Coefficient Estimate", 
                  color = "") + 
    ggplot2::theme_minimal()
}