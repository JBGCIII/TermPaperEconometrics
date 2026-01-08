

##########################################################################################################
#                                         DATA SET PROCESSING 
##########################################################################################################

library(dplyr)
library(readr)

# 1. Load combined raw dataset
df_raw <- read_csv("Raw_Data/Combined_Data_1986_2024.csv")

# 2. Transform variables
df_processed <- df_raw %>%
  arrange(date) %>%
  mutate(
    # Logs
    log_ind_prod   = log(ind_prod),
    log_cpi        = log(cpi),
    log_sp500      = log(SP500),
    log_oil_prod   = log(oil_prod_global),
    log_real_oil   = log(oil_price / cpi),

    # Growth rates / returns (percent)
    ind_prod_gr    = 100 * (log_ind_prod - lag(log_ind_prod)),
    inflation      = 100 * (log_cpi - lag(log_cpi)),
    sp500_ret      = 100 * (log_sp500 - lag(log_sp500)),
    oil_prod_gr    = 100 * (log_oil_prod - lag(log_oil_prod)),
    real_oil_gr    = 100 * (log_real_oil - lag(log_real_oil)),

    # Fed Funds change
    fedfunds_chg   = fed_funds - lag(fed_funds)


  ) %>%
  select(
    date,
    real_oil_gr,
    oil_prod_gr,
    ind_prod_gr,
    inflation,
    sp500_ret,
    fed_funds,
    fedfunds_chg
    
  ) %>%
  na.omit()

# 3. Save processed dataset
write_csv(
  df_processed,
  "Processed_Data/Processed_1986_2024.csv"
)

