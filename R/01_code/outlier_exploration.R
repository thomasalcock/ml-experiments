# Sort out outliers -------------------------------------------------------
thresh <- 10

poisson_outliers <- sku_results_poisson %>% 
  dplyr::filter(abs(price_effect) > thresh)

bayes_outliers <- sku_results_bayesglm %>% 
  dplyr::filter(abs(price_effect) > thresh)

# Exploration of outliers -------------------------------------------------
tiff("03_plots/poisson_vs_bayes_outliers.png", units = "in",
     width = 8, height = 8, res = 300)

# bayesian model
bayes_outliers %>% coef_plot()

dev.off()

# Plotting own prices against sales for outliers ---------------------------------------
tiff("03_plots/poisson_vs_bayes_outliers_scatter.png", units = "in",
     width = 8, height = 8, res = 300)

grid.arrange(
  
  df_filtered %>% 
    filter(PROD_ID %in% bayes_outliers$Product_ID) %>% 
    ggplot(aes(x = own_price, y = sales)) + 
    geom_point(size = 0.4, alpha = 0.5) + 
    facet_wrap(PROD_ID ~.) + theme_minimal() + 
    labs(x = "Own Price", y = "Sales")
)

dev.off()
