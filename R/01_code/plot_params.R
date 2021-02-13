
# plot_params -------------------------------------------------------------

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