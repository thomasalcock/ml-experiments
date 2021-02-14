
# Packages ----------------------------------------------------------------

library(timetk)
library(modeltime)
library(dplyr)
library(lubridate)
library(tidymodels)
library(rsample)
library(forecast)

# Prep data ---------------------------------------------------------------

bikes <- timetk::bike_sharing_daily %>% 
  dplyr::select(dteday, cnt)


# EDA / Diagnostic Plots --------------------------------------------------

ts_plot <- timetk::plot_time_series(
  .data = bikes,
  .date_var = dteday,
  .value = cnt,
  .interactive = FALSE
)

acf_plot <- timetk::plot_acf_diagnostics(
  .data = bikes,
  .date_var = dteday,
  .value = cnt,
  .interactive = FALSE
)

seas_plot <- timetk::plot_seasonal_diagnostics(
  .data = bikes,
  .date_var = dteday,
  .value = cnt,
  .interactive = FALSE
)

outlier_plot <- timetk::plot_anomaly_diagnostics(
  .data = bikes,
  .date_var = dteday,
  .value = cnt,
  .interactive = FALSE
)



# Fit rolling models ------------------------------------------------------

bike_splits <- bikes %>% 
  rsample::rolling_origin(
    initial = 600,
    asses = 1
  )

fit_rolling_arimas <- function(split_obj){
  
  train_df <- rsample::analysis(split_obj)
  
  modeltime::arima_reg() %>% 
    parsnip::set_engine(engine = "auto_arima") %>% 
    parsnip::fit(cnt ~ dteday, data = train_df)
}

fit_rolling_ets <- function(split_obj){
  
  train_df <- rsample::analysis(split_obj)
  
  modeltime::exp_smoothing() %>% 
    parsnip::set_engine(engine = "ets") %>% 
    parsnip::fit(cnt ~ dteday, data = train_df)
  
}

arima_fits <- purrr::map(
  .x = bike_splits$splits,
  .f = fit_rolling_arimas
)

names(arima_fits) <- bike_splits$id

# Rolling Forecast using modeltime ------------------------------------------------


get_mt_forecasts <- function(split_id, split_data, arima_models) {
  
  some_split <- split_data[split_data$id == split_id, ]$splits[[1]]
  
  mt_table <- modeltime::modeltime_table(
    arima_models[[which(names(arima_models) == split_id)]]
  )
  
  mt_calibrated <- mt_table %>% 
    modeltime::modeltime_calibrate(
      new_data = rsample::assessment(some_split)
    )
  
  fc_results <- mt_calibrated %>% 
    modeltime::modeltime_forecast(
      new_data = rsample::assessment(some_split),
      actual_data = bikes
    )
}

all_forecasts <- purrr::map(
  .x = bike_splits$id,
  .f = get_mt_forecasts,
  split_data = bike_splits,
  arima_models = arima_fits
)

names(all_forecasts) <- bike_splits$id

all_forecasts[[3]] %>% filter(.key == "prediction")

preds_only <- purrr::map_dfr(all_forecasts, ~filter(., .key == "prediction"))


plot_data <- dplyr::bind_rows(
  all_forecasts[[1]] %>% filter(.key == "actual"),
  preds_only
)

plot_modeltime_forecast(plot_data)


