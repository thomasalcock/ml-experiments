
# libraries ---------------------------------------------------------------

library(tidyverse)
library(forecast)
options(stringsAsFactors = FALSE)

# load data ---------------------------------------------------------------

sales <- read.csv("02_data/sales_2015_16.csv")
stores <- read.csv("02_data/stores.csv")
features <- read.csv("02_data/features.csv")


# wrangle data ------------------------------------------------------------

df <- sales %>% 
  left_join(features %>% left_join(stores))

z_scale <- function(x){
  (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

model_df <- df %>% 
  mutate(Store = as.factor(paste(Store)),
         Weekly_Sales = log(Weekly_Sales),
         Fuel_Price = log(Fuel_Price),
         Date = as.Date(Date)) %>% 
  filter(Store==1)

# plot oil price vs sales -------------------------------------------------

ts_plot <- model_df %>% 
  select(Date, Weekly_Sales, Fuel_Price) %>% 
  mutate_if(is.numeric, z_scale) %>% 
  gather("key", "value", -Date) %>% 
  ggplot(aes(Date, value, color = key)) + 
  geom_line() + theme_minimal() + 
  labs(y = "", color = "")

s_plot <- model_df %>% 
  select(Date, Weekly_Sales, Fuel_Price) %>% 
  #mutate_if(is.numeric, z_scale) %>% 
  ggplot(aes(Fuel_Price, Weekly_Sales)) + 
  geom_point() + theme_minimal()

# De-seasonalize sales ----------------------------------------------------
seasonal_ts <- model_df$Weekly_Sales
seasonal_ts <- ts(seasonal_ts, frequency = 4)

ts_decompoistion <- stl(seasonal_ts, "periodic")

autoplot(ts_decompoistion) + 
  theme_minimal()

# Plot trend / remainder against oil price --------------------------------

model_df <- model_df %>% cbind(as.data.frame(ts_decompoistion$time.series))
model_df %>% 
  ggplot(aes(Fuel_Price, trend, color=Promotion)) + 
  geom_point()


# estimate poisson model --------------------------------------------------

model <- lm("Weekly_Sales ~.", data = model_df)
summary(model)
