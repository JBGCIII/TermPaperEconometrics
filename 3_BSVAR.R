# 1. Install and load necessary packages
# install.packages(c("bsvars", "bsvarSIGNs"))

# Load or install required packages
required_packages <- c("readr", "dplyr", "purrr", "zoo","xts", "lubridate", "vars", "bsvars", 
                      "bsvarSIGNs", "devtools", "Rcpp","ggplot2", "Rtools")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))



#Super Important


devtools::install_github("kthohr/BMR") #Note, we need to install devtools before installing this from GIT.

install.packages("Rtools45")

library("devtools")
devtools::install_github("kthohr/BMR")








# Read processed macro data
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")

data_xts <- xts(
  macro_data %>% 
    select(
      oil_production_growth,
      real_activity,
      real_oil_price,
      real_sp500_return,
      fedfunds
    ),
  order.by = macro_data$date
)


# 2. Prepare your data matrix (ordered as per your previous logic)
# Order: Oil Prod Growth, Real Activity, Real Oil Price, SP500, Fed Funds
Y <- as.matrix(macro_data[, c("oil_production_growth", "real_activity", 
                                "real_oil_price", "real_sp500_return", "fedfunds")])


# Check multiple criteria (AIC, BIC, HQ)
lag_selection <- VARselect(Y_for_lag, lag.max = 12, type = "const")
print(lag_selection$selection)



# 1. Estimation (12 lags as per Kilian standard)
spec = specify_bsvarSIGN$new(
  data = Y,
  p = 12, 
  sign_irf = sign_matrix
)

# 2. Burn-in and Posterior Sampling
# This might take 2-5 minutes depending on your CPU
set.seed(123)
posterior = estimate(spec, iterations = 2000, burn_in = 1000)

# 3. Compute Impulse Responses (24-month horizon)
irfs = compute_impulse_responses(posterior, horizon = 24)