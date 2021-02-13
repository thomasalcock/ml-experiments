
# Explore sales time series -----------------------------------------------

total_sales <- df_filtered %>% 
  dplyr::group_by(DATE_ID) %>% 
  dplyr::summarise(sales = sum(sales),
                   med_price = median(own_price),
                   mean_price = mean(own_price),
                   min_price = min(own_price), 
                   max_price = max(own_price),
                   med_comp_price = median(comp_price),
                   mean_comp_price = mean(comp_price),
                   min_comp_price = min(comp_price), 
                   max_comp_price = max(comp_price),
                   med_promo = median(promo)
                   )

z_scale <- function(x) (x-mean(x))/sd(x)

# price vs sales over time
plot_data <- total_sales %>% 
  dplyr::select(DATE_ID, sales, med_price) %>% 
  dplyr::mutate_if(is.numeric, z_scale) %>% 
  reshape2::melt(id.vars = "DATE_ID")

plot_data %>% 
  filter(variable == "sales") %>% 
  ggplot(aes(x = DATE_ID, y = value, 
             color = variable)) + 
  geom_point(size = 1.3, alpha = 0.6) +
  geom_line(alpha = 0.4) + 
  theme_minimal() + 
  labs(x = "Date", 
       y = "Sales") + 
  geom_smooth()

# price vs comp price
total_sales %>% 
  dplyr::select(DATE_ID, med_comp_price, med_price) %>% 
  dplyr::mutate_if(is.numeric, z_scale) %>% 
  reshape2::melt(id.vars = "DATE_ID") %>% 
  ggplot(aes(x = DATE_ID, y = value, 
             color = variable)) + 
  geom_point(size = 1.3, alpha = 0.6) +
  geom_line(alpha = 0.4) + 
  theme_minimal() + 
  labs(x = "Date", 
       y = "Price")

# scatterplotof logged prices and sales
p <- total_sales %>% 
  dplyr::mutate_if(is.numeric, z_scale) %>% 
  ggplot(aes(med_price, sales)) + 
  geom_point(size = 0.3, alpha = 0.4) + 
  theme_minimal() + 
  geom_smooth(method = "lm")

ggExtra::ggMarginal(p, 
                    type = "densigram", 
                    fill = "blue", 
                    color = "skyblue",
                    alpha = 0.2)
