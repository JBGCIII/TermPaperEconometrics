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

plot(Bvar_Kilian_Stock, var_names = colnames(Kilian_Stock), save = FALSE)

###################################################Impulse Response Function####################################### 
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


###################################################FEVD############################################################ 

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
