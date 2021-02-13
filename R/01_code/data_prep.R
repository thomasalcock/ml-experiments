# Load Data ---------------------------------------------------------------
df <- readr::read_csv("/Users/thomasalcock/pricing-simulator-ela-model-master/data_for_ela_model/MPG_193_training_data.csv")

# clean data up
df_filtered <- df %>% 
  dplyr::filter(PRICE_OUTLET > 0 & 
                PRICE_AMAZON_REL > 0 & 
                SALES_QTY > 0) %>% 
  dplyr::filter(dplyr::between(PRICE_OUTLET, 
                               quantile(PRICE_OUTLET, probs = c(0.975, 0.025))[2], 
                               quantile(PRICE_OUTLET, probs = c(0.975, 0.025))[1])) %>%
  dplyr::filter(dplyr::between(SALES_QTY, 
                               quantile(SALES_QTY, probs = c(0.975, 0.025))[2], 
                               quantile(SALES_QTY, probs = c(0.975, 0.025))[1])) %>%
  dplyr::mutate(PRICE_AMAZON = PRICE_AMAZON_REL * PRICE_OUTLET) %>%
  dplyr::group_by(PROD_ID, DATE_ID) %>% 
  dplyr::summarise(SALES_QTY = sum(SALES_QTY),
                   PRICE_OUTLET = mean(PRICE_OUTLET),
                   PRICE_AMAZON = mean(PRICE_AMAZON),
                   LAST_WEEK_PRO_SHARE = mean(LAST_WEEK_PRO_SHARE),
                   WEIGHTED_PG_PRICE = mean(WEIGHTED_PG_PRICE),
                   DIFF_OUTLET = mean(DIFF_OUTLET),
                   DIFF_365 = mean(DIFF_365)) %>% 
  dplyr::rename(sales = SALES_QTY,
                own_price = PRICE_OUTLET,
                comp_price = PRICE_AMAZON,
                promo = LAST_WEEK_PRO_SHARE,
                pg_price = WEIGHTED_PG_PRICE,
                diff_outlet = DIFF_OUTLET,
                diff_365 = DIFF_365
  ) %>% 
  dplyr::ungroup()

# remove prod ids with small number of observations
small_prod_ids <- df_filtered %>% 
  dplyr::group_by(PROD_ID) %>% 
  dplyr::summarise(N = n()) %>% 
  dplyr::filter(N < 100) %>% 
  dplyr::pull(PROD_ID)

df_filtered <- df_filtered %>% dplyr::filter(!PROD_ID %in% small_prod_ids)

df_filtered$PROD_ID <- purrr::map_chr(df_filtered$PROD_ID, anon_ids)


# Remove outliers ---------------------------------------------------------

# Rule based outlier removal
outliers <- df_filtered %>% 
  dplyr::group_by(PROD_ID) %>% 
  dplyr::summarise(sd_price = sd(own_price),
                   sd_sales = sd(sales)) %>% 
  dplyr::mutate(outlier = (sd_price < 1 & sd_sales < 1) | 
                  (sd_price < 1 & sd_sales > 20) | 
                  (sd_price > 20 & sd_sales < 1)) 

df_filtered <- df_filtered %>% 
  dplyr::left_join(outliers) %>% 
  dplyr::filter(!outlier)
