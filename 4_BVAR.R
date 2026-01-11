##########################################################################################################
###                               BSVAR                                                                ### 
##########################################################################################################

# Comments about BMR. BMR appears to 

# Load or install required packages
required_packages <- c("readr", "dplyr", "purrr", "zoo","xts", "lubridate", "vars", "bsvars", 
                       "devtools", "Rcpp","ggplot2")


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

##########################################################################################################
###                                 2. Impulse Response Function                                       ### 

###################################################Global Financial Crisis############################# 

dir.create("Processed_Data/IRF2", showWarnings = FALSE)

target_index_GFC <- which(macro_data$date == "2008-09-01")
which_irfs_GFC <- target_index_GFC - tau - 2
print(which_irfs_GFC) #190

png(
    filename = "Processed_Data/FEVD/BVAR_FEVD_GFC4.png",
    width = 1800,
    height = 2500,
    res = 200
  )

irf_obj_gfc <- IRF.Rcpp_bvars(
  Bvar_Kilian_Stock, 
  periods= 15, 
  which_irfs = which_irfs_GFC,
  percentiles = c(0.05, 0.5, 0.95),
  var_names = colnames(Kilian_Stock),
  shocks_row_order = TRUE, 
  save=FALSE)

dev.off()

extract_irf <- function(irf, response, shock, negative = FALSE) {
  lo <- irf$plot_vals[,1,response,shock]
  md <- irf$plot_vals[,2,response,shock]
  hi <- irf$plot_vals[,3,response,shock]

  if (negative) {
    return(list(
      lower  = -hi,
      median = -md,
      upper  = -lo
    ))
  }

  list(lower = lo, median = md, upper = hi)
}

irf_oil  <- extract_irf(irf_obj_gfc, response = 1, shock = 2, negative = TRUE)
irf_oilp <- extract_irf(irf_obj_gfc, response = 3, shock = 2, negative = TRUE)
irf_sp   <- extract_irf(irf_obj_gfc, response = 4, shock = 2, negative = TRUE)
h <- seq_len(length(irf_oil$median))


png(
    filename = "Processed_Data/IRF2/IRF_GFC_BVAR.png",
    width = 1800,
    height = 1800,
    res = 200
  )

par(mfrow = c(3,1), mar = c(4,4,2,1))

plot(h, irf_oil$median, type="l", lwd=2,
     ylim=range(irf_oil$lower, irf_oil$upper),
     main="Oil Production",
     xlab="Horizon", ylab="Response Percent Change Oil Production")
lines(h, irf_oil$lower, lty=2)
lines(h, irf_oil$upper, lty=2)
abline(h=0, col="gray")

plot(h, irf_oilp$median, type="l", lwd=2,
     ylim=range(irf_oilp$lower, irf_oilp$upper),
     main="Real Oil Price",
     xlab="Horizon", ylab="Response Log Oil Price")
lines(h, irf_oilp$lower, lty=2)
lines(h, irf_oilp$upper, lty=2)
abline(h=0, col="gray")

plot(h, irf_sp$median, type="l", lwd=2,
     ylim=range(irf_sp$lower, irf_sp$upper),
     main="Real S&P 500 Return",
     xlab="Horizon", ylab="Response in Real Return (%)")
lines(h, irf_sp$lower, lty=2)
lines(h, irf_sp$upper, lty=2)
abline(h=0, col="gray")

dev.off()

####################################################FEVD

dir.create("Processed_Data/FEVD", showWarnings = FALSE)




#Note Variance is squared, hence we do not need to decompse things as before.


png(
    filename = "Processed_Data/FEVD/BVAR_FEVD_GFC2.png",
    width = 1800,
    height = 2500,
    res = 200
  )

  BMR::FEVD.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, 
    cumulative = FALSE,
    var_names = colnames(Kilian_Stock),
    which_irfs = which_irfs_GFC,
    percentiles = c(0.05, 0.5, 0.95),
    shocks_row_order = TRUE,
    save = FALSE
  )

dev.off()








###################################################FEVD############################################################ 





#############################





























setwd("Processed_Data")  # Temporary working directory

for (i in 1:3) {
  BMR::FEVD.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, # As in Kilian & Park (2009)
    cumulative = FALSE,
    var_names = colnames(Kilian_Stock),
    percentiles = c(0.05, 0.5, 0.95),
    shocks_row_order = TRUE,
    save = FALSE
    #save_format = "pdf",
    #save_title = paste0("FEVD_", shock_names[i], "_to_SP500")  # Just filename
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
