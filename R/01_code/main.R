# Bayesian Elasticity Estimation main -----------------------------------------
rm(list = ls())

# Libraries ---------------------------------------------------------------
library(arm)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggExtra)
source("01_code/ela_calc_functions.R")

# Data prep ---------------------------------------------------------------
source("01_code/data_prep.R")

# Calculate models ---------------------------------------------------
sku_results_poisson <- purrr::map_dfr(.x = unique(df_filtered$PROD_ID), 
                                      .f = poisson_sku_lvl, 
                                      .data = df_filtered)

#debugonce(poisson_sku_lvl)
sku_results_bayesglm <- purrr::map_dfr(.x = unique(df_filtered$PROD_ID), 
                                       .f = poisson_sku_lvl, 
                                       .data = df_filtered,
                                       .bayes = TRUE)

# Outlier exploration -----------------------------------------------------
source("01_code/outlier_exploration.R")


# Density plots of elasticities -------------------------------------------
tiff("03_plots/poisson_vs_bayes.png", units = "in",
     width = 8, height = 8, res = 300)
gridExtra::grid.arrange(
  ela_dens_plot(sku_results_poisson),
  ela_dens_plot(sku_results_bayesglm)
)
dev.off()
