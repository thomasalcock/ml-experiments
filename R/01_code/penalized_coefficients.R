
# penalized_coefficients --------------------------------------------------

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