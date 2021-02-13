
# Packages ----------------------------------------------------------------

library(timetk)
library(modeltime)
library(dplyr)
library(lubridate)
library(tidymodels)
library(rsample)

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


# Train Test Split --------------------------------------------------------

splits <- bikes %>% 
  rsample::rolling_origin(
    initial = 12 * 50,
    asses = 7
  )


