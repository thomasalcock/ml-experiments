
# ml_estimates ------------------------------------------------------------

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