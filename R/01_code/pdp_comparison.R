library(tidyverse)
library(ranger)
library(broom)

mt2 <- mtcars %>%
  as_tibble() %>%
  select(hp, mpg, disp, wt, qsec)

fit <- ranger(hp ~ ., mt2)


# First approach ----------------------------------------------------------

var <- quo(disp)

x_s <- select(mt2, !!var)   # grid where we want partial dependencies
x_c <- select(mt2, -!!var)  # other predictors

# if the training dataset is large, use a subsample of x_c instead
grid <- crossing(x_s, x_c)

augment.ranger <- function(x, newdata) {
  newdata <- as_tibble(newdata)
  mutate(newdata, .fitted = predict(x, newdata)$predictions)
}

au <- augment.ranger(fit, grid)

pd <- au %>%
  group_by(!!var) %>%
  summarize(yhat = mean(.fitted))

p1 <- pd %>%
  ggplot(aes(!!var, yhat)) +
  geom_line(size = 1) +
  labs(title = "Partial dependence plot for displacement",
       y = "Average prediction across all other predictors",
       x = "Engine displacement") +
  theme_bw()

# Second approach ---------------------------------------------------------

# fix estimates at mean

sim_data <- expand.grid(
  disp = mt2$disp,
  mpg = mean(mt2$mpg),
  wt = mean(mt2$wt),
  qsec = mean(mt2$qsec)
)

au2 <- augment.ranger(fit, sim_data)

p2 <- au2 %>% 
  ggplot(aes(x= disp, y =.fitted)) +
  geom_line() + 
  theme_minimal() +
  labs(title = "Alternative way of calculating the same plot",
       y = "Average prediction across all other predictors",
       x = "Engine displacement")

gridExtra::grid.arrange(p1, p2)

