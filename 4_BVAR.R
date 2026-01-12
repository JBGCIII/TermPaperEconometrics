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

BMR::plot.Rcpp_bvars(Bvar_Kilian_Stock, var_names = colnames(Kilian_Stock), save = FALSE)

##########################################################################################################
###                                 2. Impulse Response Function                                       ### 
##########################################################################################################

dir.create("Processed_Data/IRF2", showWarnings = FALSE) #IRF2 will hold the results of the BVAR

###################################################Global Financial Crisis############################# 

target_index_GFC <- which(macro_data$date == "2008-09-01")
which_irfs_GFC <- target_index_GFC - 6 #Super important to change!
print(which_irfs_GFC) 

png(
    filename = "Processed_Data/IRF2/Graph_04.b_IRF_GFC_all.png",
    width = 1800,
    height = 2500,
    res = 200
  )

irf_obj_gfc <- BMR::IRF.Rcpp_bvars(
  Bvar_Kilian_Stock, 
  periods= 15, 
  which_irfs = which_irfs_GFC,
  percentiles = c(0.05, 0.5, 0.95),
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
    filename = "Processed_Data/IRF2/Graph_05.b_IRF_GFC_KilianShock.png",
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

####################################################FEVD GFC########################################################################

dir.create("Processed_Data/FEVD", showWarnings = FALSE) # creates a directory for FEVD
#Note Variance is squared, hence we do not need to decompse things as before.

png(
    filename = "Processed_Data/FEVD/Graph_04.c_FEVD_BVAR_GFC.png",
    width = 1800,
    height = 2500,
    res = 200
  )

  BMR::FEVD.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, 
    cumulative = FALSE,
    which_irfs = which_irfs_GFC,
    percentiles = c(0.05, 0.5, 0.95),
    shocks_row_order = TRUE,
    save = FALSE
  )

dev.off()


###################################################Covid-19 Outbreak################################### 
##                                                                                                   ##

target_index_Covid <- which(macro_data$date == "2020-03-01")
which_irfs_Covid <- target_index_Covid - 6 #Super important to change!
print(which_irfs_Covid) 

png(
    filename = "Processed_Data/IRF2/Graph_06.b_IRF_Covid_all.png",
    width = 1800,
    height = 2500,
    res = 200
  )
irf_obj_Covid <- BMR::IRF.Rcpp_bvars(
  Bvar_Kilian_Stock,  
  periods= 15, 
  which_irfs = which_irfs_Covid,
  percentiles = c(0.05, 0.5, 0.95),
  #var_names = colnames(Kilian_Stock), -
  shocks_row_order = TRUE, 
  save=FALSE)
  dev.off()


irf_activity <- extract_irf(irf_obj_Covid, response = 2, shock = 1, negative = TRUE)
irf_oilp     <- extract_irf(irf_obj_Covid, response = 3, shock = 1, negative = TRUE)
irf_sp       <- extract_irf(irf_obj_Covid, response = 4, shock = 1, negative = TRUE)
h <- seq_len(length(irf_activity$median))

png(
  filename = "Processed_Data/IRF2/Graph_07.b_IRF_Covid_OilShock.png",
  width = 1800,
  height = 1800,
  res = 200
)

par(mfrow = c(3,1), mar = c(4,4,2,1))

plot(h, irf_activity$median, type="l", lwd=2,
     ylim=range(irf_activity$lower, irf_activity$upper),
     main="Response of Real Economic Activity",
     xlab="Horizon", ylab="Response")
lines(h, irf_activity$lower, lty=2)
lines(h, irf_activity$upper, lty=2)
abline(h=0, col="gray")

plot(h, irf_oilp$median, type="l", lwd=2,
     ylim=range(irf_oilp$lower, irf_oilp$upper),
     main="Response of Real Oil Price",
     xlab="Horizon", ylab="Response")
lines(h, irf_oilp$lower, lty=2)
lines(h, irf_oilp$upper, lty=2)
abline(h=0, col="gray")

plot(h, irf_sp$median, type="l", lwd=2,
     ylim=range(irf_sp$lower, irf_sp$upper),
     main="Response of Real S&P 500 Return",
     xlab="Horizon", ylab="Response")
lines(h, irf_sp$lower, lty=2)
lines(h, irf_sp$upper, lty=2)
abline(h=0, col="gray")

dev.off()

####################################################FEVD COVID########################################################################


png(
    filename = "Processed_Data/FEVD/Graph_06.c_FEVD_BVAR_COVID.png",
    width = 1800,
    height = 2500,
    res = 200
  )

  BMR::FEVD.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, 
    cumulative = FALSE,
    which_irfs = which_irfs_GFC,
    percentiles = c(0.05, 0.5, 0.95),
    shocks_row_order = TRUE,
    save = FALSE
  )

dev.off()


###################################################Ukraine Invasion##################################### 
##                                                                                                   ##

target_index_Ukraine <- which(macro_data$date == "2022-03-01")
which_irfs_ukraine <- target_index_Ukraine - 6 #Super important to change!
print(which_irfs_ukraine) 

png(
    filename = "Processed_Data/IRF2/Graph_08.b_IRF_Ukraine_all.png",
    width = 1800,
    height = 2500,
    res = 200
  )
irf_obj_Ukraine <- BMR::IRF.Rcpp_bvars(
  Bvar_Kilian_Stock,
  periods= 15, 
  which_irfs = which_irfs_ukraine,
  percentiles = c(0.05, 0.5, 0.95),
  shocks_row_order = TRUE, 
  save=FALSE)
dev.off()


# Extract IRFs (shock = real oil price = 3)
irf_prod <- extract_irf(irf_obj_Ukraine, response = 1, shock = 3, negative = FALSE) # Oil production
irf_activity <- extract_irf(irf_obj_Ukraine, response = 2, shock = 3, negative = FALSE) # Real activity
irf_sp <- extract_irf(irf_obj_Ukraine, response = 4, shock = 3, negative = FALSE) # S&P 500

h <- seq_len(length(irf_activity$median))

png(
  filename = "Processed_Data/IRF2/Graph_09.b_Ukraine_Oil_Price_Shock.png",
  width = 1800,
  height = 1800,
  res = 200
)

par(mfrow = c(3,1), mar = c(4,4,2,1))

# Oil production
plot(h, irf_prod$median, type="l", lwd=2,
     ylim=range(irf_prod$lower, irf_prod$upper),
     main="Response of Oil Production",
     xlab="Horizon", ylab="Response")
lines(h, irf_prod$lower, lty=2)
lines(h, irf_prod$upper, lty=2)
abline(h=0, col="gray")

# Real economic activity
plot(h, irf_activity$median, type="l", lwd=2,
     ylim=range(irf_activity$lower, irf_activity$upper),
     main="Response of Real Economic Activity",
     xlab="Horizon", ylab="Response")
lines(h, irf_activity$lower, lty=2)
lines(h, irf_activity$upper, lty=2)
abline(h=0, col="gray")

# S&P 500
plot(h, irf_sp$median, type="l", lwd=2,
     ylim=range(irf_sp$lower, irf_sp$upper),
     main="Response of Real S&P 500 Return",
     xlab="Horizon", ylab="Response")
lines(h, irf_sp$lower, lty=2)
lines(h, irf_sp$upper, lty=2)
abline(h=0, col="gray")

dev.off()

####################################################FEVD UKRAINE ########################################################################



png(
    filename = "Processed_Data/FEVD/Graph_08.c_FEVD_BVAR_Ukraine.png",
    width = 1800,
    height = 2500,
    res = 200
  )

  BMR::FEVD.Rcpp_bvars(
    Bvar_Kilian_Stock,
    periods = 15, 
    cumulative = FALSE,
    which_irfs = which_irfs_ukraine,
    percentiles = c(0.05, 0.5, 0.95),
    shocks_row_order = TRUE,
    save = FALSE
  )

dev.off()

















