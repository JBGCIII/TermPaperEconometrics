##########################################################################################################
###                                  TVP_BVAR                                                          ### 
##########################################################################################################

# Load or install required packages
required_packages <- c("readr", "dplyr", "purrr", "zoo","xts", "lubridate", "vars", "bsvars", 
                      "devtools", "Rcpp","ggplot2")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

devtools::install_github("kthohr/BMR") #Note, You need to install devtools before installing this from GIT.
library(BMR) # Load the hellish package.

# Read processed macro data
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")

##########################################################################################################
###                                 1. Data Preparation                                                ### 


# Prepare Data
Kilian_Stock <- macro_data %>%
  dplyr::select(oil_production_growth, real_activity, real_oil_price, real_sp500_return) %>%
  as.matrix()

# Initialize Model
Tvp_Kilian_Stock <- new(BMR::bvartvp)
Tvp_Kilian_Stock$build(data = Kilian_Stock, const = TRUE, lags = 2)

# Define Hyperparameters

#ISSUE
# Prior ought be integrated as follow for a minnesota prior.
#prior <- c(
#  0, # Oil Production growth = Stationary --> 0
#  1, # Real Activity = Not Stationary --> 1
#  1, # Oil Price = Not Stationary --> 1
#  0 # Sp500 return = Stationary --> 0
#  )  

#Looking at the help function
#?bvartvp
# It doesn't offer this.

tau     <- 80    # Training sample (first 80 obs used to initialize the prior)
XiBeta  <- 4     # Prior variance for the initial coefficients
XiQ     <- 0.005 # Scaling for the variance of the coefficient drift (Q matrix)
gammaQ  <- tau   # Degrees of freedom for the Q matrix prior
XiSigma <- 1     # Scaling for the variance of the volatility drift (S matrix)
gammaS  <- 4     # Degrees of freedom for the S matrix prior


# Apply Priors
Tvp_Kilian_Stock$prior(tau,XiBeta,XiQ,gammaQ,XiSigma,gammaS) #See Above

# Estimate via Gibbs Sampling
Tvp_Kilian_Stock$gibbs(10000, #Total Iteration (Very low)
                        5000) #Burn-In

saveRDS(Tvp_Kilian_Stock, file = "Processed_Data/Tvp_Kilian_Model.rds") #Allows to save the RDS


dim(Tvp_Kilian_Stock$alpha_draws)


##########################################################################################################
###                                 3. TVP_BVAR                                                        ### 

#[TVP_BVAR results # Graph 3]
#BMR only plots in PDF and EPS which is a nightmare to copy inside the document hence i tried to use other
#methods by they failed. The following code works when it comes to saving IRF for all variables.
#png(filename = "Processed_Data/Graph_3_TVP_BVAR_Results.png")
BMR::plot.Rcpp_bvartvp(Tvp_Kilian_Stock, var_names = colnames(Kilian_Stock), save = FALSE)
#dev.off()

##########################################################################################################
###                                 2. Impulse Response Function                                       ### 
########################################################################################################## 

dir.create("Processed_Data/IRF", showWarnings = FALSE) # Creates Directory for IRF

#Because a TVP-BVAR model allows coefficients to change at every single point in time, 
#the relationship between variables (the Impulse Response) is also different at every point in time. 
#The which_irfs parameter specifies the exact time indices for which I want to calculate and plot the 
#Impulse Response Functions.

#Processed_1986_2024.csv has 468 rows
# - 80 (Tau)
# - 2 (Lags)
# = 386

#Below I test this:

#IRF.Rcpp_bvartvp(
#obj = Tvp_Kilian_Stock,
#periods = 10,
#which_irfs = 386,
#var_names = colnames(Kilian_Stock),
#which_shock = 1,
#which_response = 4,
#percentiles = c(0.05, 0.5, 0.95),
#save = FALSE)
#This model runs!

#IRF.Rcpp_bvartvp(
#obj = Tvp_Kilian_Stock,
#periods = 10,
#which_irfs = 387,
#var_names = colnames(Kilian_Stock),
#which_shock = 1,
#which_response = 4,
#percentiles = c(0.05, 0.5, 0.95),
#save = FALSE)

#This model return the following error:
#Error: Cube::subcube(): indices out of bounds or incorrectly used.

#Conclusion: 386 is the last data point in the set, i.e., 2024-12-01

###################################################Negative Shocks########################################### 


#Issue with BMR:
# A shock_scale of 1 and shock scale of -1 return exactly the same plot.
#IRF.Rcpp_bvartvp(
#    obj = Tvp_Kilian_Stock,
#    periods = 10,
#    shock_scale = +1,
#    var_names = colnames(Kilian_Stock),
#    percentiles = c(0.05, 0.5, 0.95),
#    save = FALSE

#IRF.Rcpp_bvartvp(
#    obj = Tvp_Kilian_Stock,
#    periods = 10,
#    shock_scale = -1,
#    var_names = colnames(Kilian_Stock),
#    percentiles = c(0.05, 0.5, 0.95),
#    save = FALSE

# This is fine as long as we want to model a positive shock, which we do not want to in most cases.
# In 2008, the shock to real_activity needs to be negative to signify a drop in activity
# In 2020, the shock to oil_production_growth needs to be negative as well, to signify a drop in production
# In 2022, the shock real_oil_price can remain as is.

# The only way to deal with this is to use another tool that is not BMR and redo the code, or 
# assume symmetry applies to the variables of interests. I have decided with the latter.
# This of course not possible in areas such as monetary policy, where a contraction might slow things down, 
# but an expansion might not be able to speed things up at the same level (Fed Rate will not be used as 
# originally intended)


# irf_obj <- IRF.Rcpp_bvartvp(Tvp_Kilian_Stock, periods=10, which_shock=1, which_response=1, save=FALSE)
# irf_obj <- IRF.Rcpp_bvartvp(Tvp_Kilian_Stock, periods=10, save=FALSE)
# Both returns the same value in #irf_obj$plot_vals.
# As such the variable of interest are (object = Tvp_Kilian_Stock, periods=10, which_irf =Date of interest
# shocks_row_order=TRUE)
# The rest plot graph in R for extraction and speed up computing.

# This is a shock in variable 1 (oil_production_growth) on variable oil_production_growth
# The columns are:
# low bound , response , upper bound, period ]
#, , 1, 1 [,1] [,2] [,3] [,4] 
#[1,] 0.93883215 1.0003898853 1.068710104 1 
#[2,] -0.24090764 -0.0771716310 0.085790030 2 
#[3,] -0.35344970 -0.1777558260 -0.008211902 3 
#[4,] -0.09151076 -0.0051869814 0.084651915 4 
#[5,] -0.05264686 0.0122345966 0.099917216 5 
#[6,] -0.05893304 -0.0108575115 0.031638997 6 
#[7,] -0.04828233 -0.0085722740 0.022412209 7 
#[8,] -0.03235867 -0.0022855875 0.025012762 8 
#[9,] -0.02699548 -0.0009774605 0.022411357 9 
#[10,] -0.02510621 -0.0010877097 0.019322769 10

#Meaning we can extract and swap
#lower_neg  <- -upper   
#median_neg <- -median
#upper_neg  <- -lower
#Then plot.


###################################################Global Financial Crisis############################# 
##                                                                                                   ##
target_index_GFC <- which(macro_data$date == "2008-09-01")
which_irfs_GFC <- target_index_GFC - tau - 2
print(which_irfs_GFC) #190

png(
    filename = "Processed_Data/IRF/Graph_04_IRF_GFC_all.png",
    width = 1800,
    height = 2500,
    res = 200
  )
irf_obj_gfc <- IRF.Rcpp_bvartvp(
  Tvp_Kilian_Stock, 
  periods= 15, 
  which_irfs = which_irfs_GFC,
  percentiles = c(0.05, 0.5, 0.95),
  #var_names = colnames(Kilian_Stock), Using Var names makes the graph look cluttered as such they are removed
  shocks_row_order = TRUE, 
  save=FALSE)

dev.off()

extract_irf <- function(irf, response, shock, negative = FALSE) {
  lo <- irf$plot_vals[,1,response,shock]
  md <- irf$plot_vals[,2,response,shock]
  hi <- irf$plot_vals[,3,response,shock]

  if (negative) {
    return(list(
      lower  = -hi, # Essentially switch places
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
    filename = "Processed_Data/IRF/Graph_05_IRF_GFC_KilianShock.png",
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

###################################################Covid-19 Outbreak################################### 
##                                                                                                   ##

target_index_Covid <- which(macro_data$date == "2020-03-01")
which_irfs_Covid <- target_index_Covid - tau - 2
print(which_irfs_Covid) #328

png(
    filename = "Processed_Data/IRF/Graph_06_IRF_Covid_all.png",
    width = 1800,
    height = 2500,
    res = 200
  )
irf_obj_Covid <- IRF.Rcpp_bvartvp(
  Tvp_Kilian_Stock, 
  periods= 15, 
  which_irfs = which_irfs_Covid,
  percentiles = c(0.05, 0.5, 0.95),
  shocks_row_order = TRUE, 
  save=FALSE)
  dev.off()

irf_activity <- extract_irf(irf_obj_Covid, response = 2, shock = 1, negative = TRUE)
irf_oilp     <- extract_irf(irf_obj_Covid, response = 3, shock = 1, negative = TRUE)
irf_sp       <- extract_irf(irf_obj_Covid, response = 4, shock = 1, negative = TRUE)
h <- seq_len(length(irf_activity$median))

png(
  filename = "Processed_Data/IRF/Graph_07_IRF_Covid_OilShock.png",
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


###################################################Ukraine Invasion##################################### 
##                                                                                                   ##

target_index_Ukraine <- which(macro_data$date == "2022-03-01")
which_irfs_ukraine <- target_index_Ukraine - tau - 2
print(which_irfs_ukraine) #352

png(
    filename = "Processed_Data/IRF/Graph_08_IRF_Ukraine_all.png",
    width = 1800,
    height = 2500,
    res = 200
  )
irf_obj_Ukraine <- IRF.Rcpp_bvartvp(
  Tvp_Kilian_Stock, 
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
  filename = "Processed_Data/IRF/Graph_09_Ukraine_Oil_Price_Shock.png",
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

# Having a FEVD would be helpfull in giving the relative importance of a shock.
# This is once more due to the limitation of the BMR package. Below I list all functions.
#ls("package:BMR")

 #[1] "bvarcnw"                        "bvarcnw_cpp"
 #[3] "bvarinw"                        "bvarinw_cpp"
 #[5] "bvarm"                          "bvarm_cpp"
 #[7] "bvars"                          "bvars_cpp"
 #[9] "bvartvp"                        "bvartvp_cpp"
#[11] "cvar"                           "cvar_cpp"
#[13] "dsge_gensys"                    "dsge_gensys_cpp"
#[15] "dsge_uhlig"                     "dsge_uhlig_cpp"
#[17] "dsgevar_gensys"                 "dsgevar_gensys_cpp"
#[19] "dsgevar_uhlig"                  "dsgevar_uhlig_cpp"
#[21] "FEVD"                           "FEVD.Rcpp_bvarcnw"
#[23] "FEVD.Rcpp_bvarinw"              "FEVD.Rcpp_bvarm"
#[25] "FEVD.Rcpp_bvars"                "FEVD.Rcpp_cvar"
#[27] "forecast"                       "forecast.Rcpp_bvarcnw"
#[29] "forecast.Rcpp_bvarinw"          "forecast.Rcpp_bvarm"
#[31] "forecast.Rcpp_bvars"            "forecast.Rcpp_cvar"
#[33] "forecast.Rcpp_dsge_gensys"      "forecast.Rcpp_dsge_uhlig"
#[35] "forecast.Rcpp_dsgevar_gensys"   "forecast.Rcpp_dsgevar_uhlig"   
#[37] "gensys"                         "gensys_cpp"
#[39] "gtsplot"                        "IRF"
#[41] "IRF.Rcpp_bvarcnw"               "IRF.Rcpp_bvarinw"
#[43] "IRF.Rcpp_bvarm"                 "IRF.Rcpp_bvars"
#[45] "IRF.Rcpp_bvartvp"               "IRF.Rcpp_cvar"
#[47] "IRF.Rcpp_dsge_gensys"           "IRF.Rcpp_dsge_uhlig"
#[49] "IRF.Rcpp_dsgevar_gensys"        "IRF.Rcpp_dsgevar_uhlig"
#[51] "IRF.Rcpp_gensys"                "IRF.Rcpp_uhlig"                
#[53] "IRFcomp"                        "mode_check"
#[55] "mode_check.Rcpp_dsge_gensys"    "mode_check.Rcpp_dsge_uhlig"    
#[57] "mode_check.Rcpp_dsgevar_gensys" "mode_check.Rcpp_dsgevar_uhlig"
#[59] "plot.Rcpp_bvarcnw"              "plot.Rcpp_bvarinw"
#[61] "plot.Rcpp_bvarm"                "plot.Rcpp_bvars"
#[63] "plot.Rcpp_bvartvp"              "plot.Rcpp_cvar"
#[65] "plot.Rcpp_dsge_gensys"          "plot.Rcpp_dsge_uhlig"
#[67] "plot.Rcpp_dsgevar_gensys"       "plot.Rcpp_dsgevar_uhlig"
#[69] "prior"                          "states"
#[71] "states.Rcpp_dsge_gensys"        "states.Rcpp_dsge_uhlig"        
#[73] "states.Rcpp_dsgevar_gensys"     "states.Rcpp_dsgevar_uhlig"
#[75] "uhlig"                          "uhlig_cpp"


#No FEVD functon exists for bvartvp and applying the following codes:

#BMR::FEVD(
#  Tvp_Kilian_Stock,
#  horizon = 15,                      
#  var_names = colnames(Kilian_Stock),
#  save = FALSE,                        
#)

#Return the following: Error in UseMethod("FEVD") :
#no applicable method for 'FEVD' applied to an object of class "c('Rcpp_bvartvp', 'C++Object', 'envRefClass', 
#'.environment', 'refClass', 'environment', 'refObject')"
#

#mode_check.Rcpp_bvartvp(Tvp_Kilian_Stock) # No mode check for TVP-BVAR
