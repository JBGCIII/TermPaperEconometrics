

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
    oil_production_growth   = 100 * (log(oil_prod_global) - lag(log(oil_prod_global))),
    real_activity  = global_real_economic_activity, # Already implied to be stationary, no need to detrend
    real_oil_price   = log(oil_price / cpi), #Logged and Deflated as in Killian (2009)
    real_sp500_return = 100 * ( (log(SP500) - lag(log(SP500))) - (log(cpi) - lag(log(cpi))) ),
    fedfunds   = fed_funds # Not stationary 

    # Growth rates / returns (percent)
  ) %>%
  select(
    date,
    oil_production_growth,
    real_activity,
    real_oil_price,
    real_sp500_return,
    fedfunds

  ) %>%
  na.omit()

# 3. Save processed dataset
write_csv(
  df_processed,
  "Processed_Data/Processed_1986_2024.csv"
)

