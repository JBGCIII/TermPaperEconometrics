##########################################################################################################
###                               BSVAR                                                                ### 
##########################################################################################################

# Comments about BMR. BMR appears to 

# Load or install required packages
required_packages <- c("readr", "dplyr", "purrr", "zoo","xts", "lubridate", "vars", "bsvars", 
                      "bsvarSIGNs", "devtools", "Rcpp","ggplot2")



installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

devtools::install_github("kthohr/BMR") #Note, You need to install devtools before installing this from GIT.
library(BMR)

# Read processed macro data
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")


##########################################################################################################
###                                 1. Kilian &  Park (2009) - S&P Return                              ### 

 Kilian_Stock<- macro_data %>%
  dplyr::select(
    oil_production_growth,
    real_activity,
    real_oil_price,
    real_sp500_return
  ) %>%
  as.matrix()

Bvar_Kilian_Stock <- new(bvarm)

Bvar_Kilian_Stock$build(
  data = Kilian_Stock,
  const = TRUE,
  lags = 6
)

# Prior mean values
prior <- c(
  0, # Oil Production growth = Stationary --> 0
  1, # Real Activity = Not Stationary --> 1
  1, # Oil Price = Not Stationary --> 1
  0 # Sp500 return = Stationary --> 0
  )  

# Hyperparameters (following Canova 2007)
Bvar_Kilian_Stock$prior(
  prior,
  var_type = 1,
  decay_type = 1,
  HP1 = 0.2,
  HP2 = 0.5,
  HP3 = 1e5,
  HP4 = 1.0
)

# Posterior distributions of the coefficients
Bvar_Kilian_Stock$gibbs(10000)

################################################## Kilian &  Park (2009)####################################### 
#Impulse Response Function
shock_names <- c("Oil_Supply", "Aggregate_Demand", "Oil_Specific_Demand")

# Save IRFs directly in Processed_Data
old_wd <- getwd()
setwd("Processed_Data")  # Temporary working directory

for (i in 1:3) {
  BMR::IRF.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, # As in Kilian & Park (2009)
    cumulative = FALSE,
    var_names = colnames(Kilian_Stock),
    percentiles = c(0.05, 0.5, 0.95),
    which_shock = i,
    which_response = 4,
    shocks_row_order = TRUE,
    save = TRUE,
    save_format = "pdf",
    save_title = paste0("IRF_", shock_names[i], "_to_SP500")  # Just filename
  )
  
  # Delete default empty IRF.pdf if created
  if (file.exists("IRFs.pdf")) file.remove("IRFs.pdf")
}

setwd(old_wd)  # Restore original working directory


#############################

setwd("Processed_Data")  # Temporary working directory

for (i in 1:3) {
  BMR::FEVD.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, # As in Kilian & Park (2009)
    cumulative = FALSE,
    var_names = colnames(Kilian_Stock),
    percentiles = c(0.05, 0.5, 0.95),
    which_shock = i,
    which_response = 4,
    shocks_row_order = TRUE,
    save = TRUE,
    save_format = "pdf",
    save_title = paste0("FEVD_", shock_names[i], "_to_SP500")  # Just filename
  )
  
  # Delete default empty IRF.pdf if created
  if (file.exists("FEVDs.pdf")) file.remove("FEVDs.pdf")
}
setwd(old_wd)  # Restore original working directory


#FEVD
# Forecast Error Variance Decomposition (15 months ahead)
setwd("Processed_Data")  
fevd_result <- BMR::FEVD.Rcpp_bvars(
  Bvar_Kilian_Stock,
  horizon = 15,                       # Forecast horizon
  var_names = colnames(Kilian_Stock),
  save = TRUE,                         # Saves plots
  save_format = "pdf",
  save_title = "Processed_Data/FEVD_Kilian_Stock.pdf"  # Save in Processed_Data
)

setwd(old_wd)  # Restore original working directory

ls("package:BMR")


ls("package:bsvarSIGNs")




##########################################################################################################
###                                 2. Kilian &  Park (2009) - FED RATE                                ### 

Kilian_Rate <- macro_data %>%
  dplyr::select(
    oil_production_growth,
    real_activity,
    real_oil_price,
    fedfunds
    ) %>%
  as.matrix()

# Prior mean values
prior <- c(0, 1, 1, 1, 0)  # oil_growth = 0, activity = 1, oil_price = 1, fed = 1, sp500_return = 0



# Hyperparameters (following Canova 2007)
bvar_obj$prior(prior,
               var_type = 1,     # 1 = standard Minnesota
               decay_type = 1,   # 1 = decay
               HP1 = 0.2,        # overall tightness
               HP2 = 0.5,        # cross-variable tightness
               HP3 = 1e5,        # dummy observations (essentially uninformative)
               HP4 = 1.0)        # optional extra hyperparameter







# 1. Create BVAR model object
bvar_obj <- new(bvarm)

# 2. Build model: include constant, 4 lags
bvar_obj$build(dat_mat,
               include_const = TRUE,
               lags = 4)


# Prior mean values
prior <- c(
  0, # oil production growth is stationary
  1, # real activity is not stationary
  1, # oil_price is not stationary
  1, # fed rate is not stationary
  0)  # oil_growth = 0, activity = 1, oil_price = 1, fed = 1, sp500_return = 0



# Hyperparameters (following Canova 2007)
bvar_obj$prior(prior,
               var_type = 1,     # 1 = standard Minnesota
               decay_type = 1,   # 1 = decay
               HP1 = 0.2,        # overall tightness
               HP2 = 0.5,        # cross-variable tightness
               HP3 = 1e5,        # dummy observations (essentially uninformative)
               HP4 = 1.0)        # optional extra hyperparameter










##########################################################################################################
###                                 2. BSVAR                                                           ### 

# Read processed macro data
macro_data <- read.csv("Processed_Data/Processed_1986_2024.csv")

gtsplot(dat[, 2:4], dates = dat[, 1])

gtsplot(macro_data[, c("oil_production_growth","real_activity", "real_oil_price", "real_sp500_return" , "fedfunds")], dates = macro_data$Date)



library(BVAR) # or the package that provides bvarm class

# Prepare data matrix
dat_mat <- data.matrix(df_processed %>%
                         select(oil_production_growth,
                                real_activity,
                                real_oil_price,
                                fedfunds,
                                real_sp500_return))

# 1. Create BVAR model object
bvar_obj <- new(bvarm)

# 2. Build model: include constant, 4 lags
bvar_obj$build(dat_mat,
               include_const = TRUE,
               lags = 4)






dat <- read.csv(file = "USdata.csv")



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


##########################################################################################################
###                                 2. Unit root tests (1986-2020)                                     ### 


devtools::install_github("kthohr/bsvarSIGNs")





library(bsvarSIGNs)