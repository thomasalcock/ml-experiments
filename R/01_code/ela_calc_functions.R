
# Ela calculation functions -----------------------------------------------

# z scale
z_scale <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

# Function to anonymise prod ids
anon_ids <- function(x){
  set.seed(1)
  x <- unlist(strsplit(substr(as.character(x), 2, 9), ""))
  return(paste0(sample(x, length(x),  replace = FALSE), collapse = ""))
}

# Function for poisson model without aggregation at SKU level 
poisson_sku_lvl <- function(.data, 
                            .sku,
                            .bayes = FALSE){

  df <- .data %>% 
    dplyr::filter(PROD_ID %in% .sku) 
  
  eqn <- as.formula(
    "sales ~ log(own_price) + log(comp_price) +log(pg_price) + promo + diff_outlet"
  )
  
  if(.bayes){
    pois_model <- try(arm::bayesglm(eqn, 
                                    data = df, 
                                    family = "gaussian",
                                    prior.df = Inf,
                                    prior.mean = c(-3.3, 2.8, -1, 1, 1),
                                    prior.scale = 1))
  }else{
    pois_model <- try(glm(eqn, 
                          data = df, 
                          family = poisson(link = "log")))
  }
  
  if("try-error" %in% class(pois_model)){
    print(paste0("Model could not be fitted for SKU ", .sku))
    return(NULL)
  }
  
  results <- summary(pois_model)$coefficients
  
  results <- results %>% 
    as.data.frame() %>% 
    dplyr::mutate(Variable = rownames(results),
                  Product_ID = .sku, 
                  N_Obs = nrow(df)) %>%     
    dplyr::rename(price_effect = Estimate, 
                  std_error = `Std. Error`, 
                  variable = Variable) %>%
    dplyr::select(price_effect, 
                  std_error, 
                  variable, 
                  Product_ID, 
                  N_Obs) %>% 
    dplyr::mutate(lower_bound = price_effect - 1.96 * std_error,
                  upper_bound = price_effect + 1.96 * std_error,
                  variable = as.character(variable)) %>% 
    dplyr::filter(variable %in% c("log(comp_price)", 
                                  "log(own_price)")) 
  rownames(results) <- NULL
  
  return(results)
}

# Function for bayesian poisson 
bayes_poisson_sku_lvl <- function(.data,
                                  .sku){
  df <- .data %>% 
    dplyr::filter(PROD_ID %in% .sku) %>% 
    dplyr::filter(
      dplyr::between(sales, 
                     quantile(sales, c(0.975, 0.025))[1],
                     quantile(sales, c(0.975, 0.025))[2])
    ) %>% 
    dplyr::filter(
      dplyr::between(own_price, 
                     quantile(own_price, c(0.975, 0.025))[1],
                     quantile(own_price, c(0.975, 0.025))[2])
    )
  
  
  eqn <- "sales ~ log(own_price) + 
  log(comp_price) + 
  log(pg_price) + 
  promo + diff_outlet"
  
  pois_model <- arm::bayesglm(formula = eqn,
                              family = poisson(link = "log"),
                              prior.mean = c(-2.3, 1.8, -1, 1, 1),
                              prior.scale = 2,
                              prior.df = Inf)
  
  results <- summary(pois_model)$coefficients
  
  results <- results %>% 
    as.data.frame() %>% 
    dplyr::mutate(Variable = rownames(results),
                  Product_ID = .sku, 
                  N_Obs = nrow(df))
  
  rownames(results) <- NULL
  
  return(results)
}

# Function for linear model with various forms (log-linear, etc.)
linear_sku_lvl <- function(.data,
                           .sku,
                           .add_features = NULL,
                           .func_form = "log-log"){
  df <- .data %>% 
    dplyr::filter(PROD_ID == .sku) %>% 
    dplyr::select(sales, own_price, .add_features) 
  
  if(.func_form == "log-log" & "comp_price" %in% .add_features){
    lm(log(sales) ~ log(own_price) + log(comp_price))
  }
  
}


# Function to plot elasticity densities -------------------------------------------

ela_dens_plot <- function(.data){

  .data %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_density(aes(x = price_effect, 
                              fill = variable),
                          color = "white", 
                          alpha = 0.2) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = "Elasticity", 
                  y = " ", fill = "Elasticity Type") +
    ggplot2::xlim(c(-7,7)) + 
    ggplot2::scale_fill_manual(values = c("skyblue", "green"),
                               labels = c("Competitor price", "Own price")) 
}

# Function for coeff plots
coef_plot <- function(.data){
  
  .data %>% 
    ggplot(aes(x = price_effect, y = Product_ID)) + 
    geom_point(size = 0.5) + 
    geom_segment(aes(x = lower_bound, xend = upper_bound, 
                     y = Product_ID, yend = Product_ID)) + 
    theme_minimal() + 
    facet_wrap(variable ~., labeller = as_labeller(c("log(comp_price)" = "Competitor price",
                                                     "log(own_price)" = "Own price"))) + 
    labs(x = "Elasticity", y = "Product ID")
  
}
