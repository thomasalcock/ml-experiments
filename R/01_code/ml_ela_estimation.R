library(dplyr)
library(tidymodels)
library(tibble)
library(ggplot2)
library(magrittr)
library(readr)
library(plotly)
library(parsnip)
library(rsample)
library(lubridate)

clean_names <- function(df){
  names(df) <- tolower(names(df))
  tibble::as_tibble(df)
}

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Read and prep data ------------------------------------------------------

df <- readr::read_delim(
  file = "02_data/sales_data_sample.csv",
  delim = ",",
  col_types = cols(
    .default = col_character(),
    ORDERNUMBER = col_character(),
    QUANTITYORDERED = col_integer(),
    PRICEEACH = col_double(),
    SALES = col_double(),
    ORDERDATE = col_date(format = "%m/%d/%Y %H:%M"),
    STATUS = col_character(),
    QTR_ID = col_integer(),
    MONTH_ID = col_integer(),
    YEAR_ID = col_integer(),
    PRODUCTLINE = col_character(),
    MSRP = col_double()
  )
) 


df %<>% 
  clean_names() %>% 
  dplyr::rename(
    revenue = sales,
    price = priceeach,
    quantity = quantityordered
  ) %>% 
  dplyr::mutate(
    msrp_diff = msrp - price,
    discount = dplyr::case_when(msrp_diff > 0 ~ msrp_diff, TRUE ~ 0),
    markup = dplyr::case_when(msrp_diff < 0 ~ abs(msrp_diff), TRUE ~ 0)
  )



# EDA ---------------------------------------------------------------------

plot_data <- df %>% 
  dplyr::group_by(productline, orderdate) %>% 
  dplyr::summarise(
    total_sales = sum(quantity, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::mutate(
    z_mean_price = zscore(mean_price),
    z_sales = zscore(total_sales),
    z_revenue = zscore(total_revenue)
  ) %>% 
  tidyr::pivot_longer(
    cols = total_sales:z_revenue,
    names_to = "key"
  )

pl_data <- plot_data %>% 
  filter(
    key %in% c("z_mean_price", "z_sales"), 
    productline == "Classic Cars"
  )

prodlines <- pl_data %>%  
  ggplot(aes(x = orderdate, y = value, color = key)) +
  geom_line() + theme_minimal() + 
  labs(x = "Date", y = "", color = "Metric")

plotly::ggplotly(prodlines)


# Sales vs price ----------------------------------------------------------

sc_plot <- df %>% 
  ggplot(aes(x = price, y = revenue, color = productline)) +
  geom_point(size = 2.5, alpha = 0.6) + theme_minimal()

plotly::ggplotly(sc_plot)


# Prep data for model training --------------------------------------------

mydf <- dplyr::select(
  df,
  quantity, price, month_id, 
  year_id, productline, discount, markup
)

# tt_split <- rsample::initial_split(mydf, prop = 0.6, strata = "productline")
# df_train <- training(tt_split)
# df_test <- testing(tt_split)


# Train model -------------------------------------------------------------

rf <- parsnip::rand_forest("regression") %>% 
  parsnip::set_engine("ranger") %>% 
  parsnip::fit(quantity ~ ., data = mydf)

mydf %<>% mutate(pred_sales = rf$fit$predictions)

actual_predicted <- mydf %>% ggplot(aes(x = pred_sales, y=quantity, color = productline)) +
  geom_point() + geom_smooth(se = FALSE) + theme_minimal()

#plotly::ggplotly(actual_predicted)


# Simulate Price effects --------------------------------------------------

# fix all vars except price

simulate_sales <- function(model_object,
                           min_price, 
                           max_price,
                           price_step = 0.1,
                           month_id = lubridate::month(Sys.Date()), 
                           year_id = lubridate::year(Sys.Date()), 
                           prod_line = "Classic Cars", 
                           fix_discount, 
                           fix_markup) {
  
  sim_data <- expand.grid(
    month_id = month_id,
    year_id = year_id,
    price = seq(min_price, max_price, by = price_step),
    productline = prod_line,
    discount = fix_discount,
    markup = fix_markup
  ) 
  
  sim_quantity <- predict(model_object, sim_data)
  cbind(sim_data, sim_quantity)

}

sim_data <- simulate_sales(
  model_object = rf,
  min_price = min(mydf$price),
  max_price = max(mydf$price),
  fix_discount = median(mydf$discount),
  fix_markup = median(mydf$markup)
)


ggplot(sim_data, aes(x = price, y = .pred)) +
  geom_point(size= 3, alpha = 0.2, color = "red") +
  geom_line() +
  theme_minimal() +
  labs(x = "Fictional Price", y = "Predicted Sales")


# ICE Curves --------------------------------------------------------------

ice_data <- dplyr::select(
  df %>% mutate(order_id = paste0(ordernumber, "_", orderlinenumber)), 
  order_id, month_id, year_id, productline, discount, markup
)

price_df <- tibble(price = seq(min(mydf$price), max(mydf$price), 0.1))
grid_data <- crossing(price_df, ice_data)

ice_curves <- cbind(
  grid_data,
  predict(rf, grid_data)
) %>% as_tibble()

plot_data <- ice_curves %>% 
  select(order_id, price, .pred)

avg_preds <- plot_data %>% 
  group_by(price) %>% 
  summarise(avg_pred = mean(.pred), .groups = "drop")

ice_plot <- plot_data %>%
  ggplot(aes(x = price, y = .pred, color = order_id)) +
  geom_line(alpha = 0.2) + theme_minimal() + theme(legend.position = "none") +
  labs(x = "Price", y = "Predicted Sales", title = "ICE Curves for for all IDs") + 
  geom_line(data = avg_preds, mapping = aes(x = price, y = avg_pred, color = NULL), size = 1)

ice_plot

demand_curve <- function(pars, data){
  yhat <- pars[1] * ((data$price / pars[2])^pars[3])
  sum((data$avg_pred - yhat)^2)
}

optim(
  par = c(0, 0, 0),
  fn = demand_curve,
  data = avg_preds,
  method = "L-BFGS-B"
)


# Forecasting Approach ----------------------------------------------------


