evaluation <- function(pred, obs, log = FALSE){
  
  if(log == TRUE){
    pred <- log(1 + pred)
    obs <- log(1 + obs)
  }
  
  metric <- mean((pred - obs)^2)
  
  if(root == TRUE){
    metric <- sqrt(metric)
  }
  
  return(metric)
  
}
