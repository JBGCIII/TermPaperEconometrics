##########################################################################################################
###                               TVP_BSVAR                                                                ### 
##########################################################################################################

# Comments about BMR. BMR appears to 

# Load or install required packages
required_packages <- c("readr", "dplyr", "purrr", "zoo","xts", "lubridate", "vars", "bsvars", 
                      "bsvarSIGNs", "devtools", "Rcpp","ggplot2")


str(Tvp_Kilian_Stock)



installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

devtools::install_github("kthohr/BMR") #Note, You need to install devtools before installing this from GIT.
library(BMR)


dir.create("Processed_Data/IRF", showWarnings = FALSE)



# Read processed macro data
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")


##########################################################################################################
###                                 1. Data Preparation                                                ### 


# 1. Prepare Data
Kilian_Stock <- macro_data %>%
  dplyr::select(oil_production_growth, real_activity, real_oil_price, real_sp500_return) %>%
  as.matrix()

# 2. Initialize Model
Tvp_Kilian_Stock <- new(bvartvp)
Tvp_Kilian_Stock$build(data = Kilian_Stock, const = TRUE, lags = 2)

# 3. Define Hyperparameters
tau     <- 80    # Training sample (first 80 obs used to initialize the prior)
XiBeta  <- 4     # Prior variance for the initial coefficients
XiQ     <- 0.005 # Scaling for the variance of the coefficient drift (Q matrix)
gammaQ  <- tau   # Degrees of freedom for the Q matrix prior
XiSigma <- 1     # Scaling for the variance of the volatility drift (S matrix)
gammaS  <- 4     # Degrees of freedom for the S matrix prior

# 4. Apply Priors
Tvp_Kilian_Stock$prior(tau,XiBeta,XiQ,gammaQ,XiSigma,gammaS) #See Above

# 5. Estimate via Gibbs Sampling
Tvp_Kilian_Stock$gibbs(10000, #Total Iteration
                        5000) #Burn-In

saveRDS(Tvp_Kilian_Stock, file = "Processed_Data/Tvp_Kilian_Model.rds")


dim(Tvp_Kilian_Stock$alpha_draws)


#TVP_BSVAR result 
plot(Tvp_Kilian_Stock, var_names = colnames(Kilian_Stock), save = FALSE)



##########################################################################################################
###                                 2. Impulse Response Function                                       ### 








###################################################Full Sample########################################### 


 png(
    filename = "Processed_Data/Test",
    width = 1800,
    height = 1800,
    res = 200
  )
  IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    var_names = colnames(Kilian_Stock),
    percentiles = c(0.05, 0.5, 0.95),
    save = TRUE
  )

  dev.off()



IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    shock_scale = +1,
    var_names = colnames(Kilian_Stock),
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
  )








#Impulse Response Function (Full Sample)
shock_names <- c("OilSupply", "AggregateDemand", "OilSpecificDemand")
for (i in 1:3) {

  png(
    filename = paste0(
      "Processed_Data/1986_to_2024_IRF_TVP_",
      shock_names[i],
      "_to_SP500.png"
    ),
    width = 1800,
    height = 1800,
    res = 200
  )

  IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    var_names = colnames(Kilian_Stock),
    which_shock = i,
    which_response = 4,
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
  )

  dev.off()
}

if (file.exists("IRFs.eps")) file.remove("IRFs.eps")

###################################################Historical Periods######################################### 

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
# As such the variable of interest are (object = Tvp_Kilian_Stock, periods=10, shocks_row_order=TRUE)
# The rest plot graph in R for extraction and speed up computing

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

target_index_GFC <- which(macro_data$date == "2008-09-01")
which_irfs_GFC <- target_index_GFC - tau - 2
print(which_irfs_GFC) #190




lower  <- irf_obj$plot_vals[, 1, 1, 2]  # 5%
median <- irf_obj$plot_vals[, 2, 1, 2]  # 50%
upper  <- irf_obj$plot_vals[, 3, 1, 2]  # 95%

lower_neg  <- -upper   # NOTE: swap!
median_neg <- -median
upper_neg  <- -lower
h <- 1:10

plot(h, median_neg, type="l", lwd=2,
     ylim=range(lower_neg, upper_neg),
     xlab="Horizon", ylab="Response")

lines(h, lower_neg, lty=2)
lines(h, upper_neg, lty=2)
abline(h=0, col="gray")









?IRF.Rcpp_bvartvp

IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    var_names = colnames(Kilian_Stock),
    shock_scale = +1,
    shock_names =, 
    which_shock = 2,
    which_response = 1,
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
  )





IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    var_names = colnames(Kilian_Stock),
    shock_scale = +1,
    which_shock = 1,        # first variable
    which_response = 2:4,   # responses of other variables
    shocks_row_order = TRUE
)



IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    var_names = colnames(Kilian_Stock),
    shock_scale = -1,
    which_shock = 1,        # first variable
    which_response = 2:4,   # responses of other variables
    shocks_row_order = FALSE
)





IRF.Rcpp_bvartvp(obj = Tvp_Kilian_Stock,
                 periods = 10,
                 var_names = colnames(Kilian_Stock),
                 shock_scale = 1,
                 which_shock = 2,
                 percentiles = c(0.05,0.5,0.95),
                 save = FALSE)

IRF.Rcpp_bvartvp(obj = Tvp_Kilian_Stock,
                 periods = 10,
                 var_names = colnames(Kilian_Stock),
                 shock_scale = -1,
                 which_shock = 2,
                 percentiles = c(0.05,0.5,0.95),
                 save = FALSE)




irf_pos <- IRF.Rcpp_bvartvp(
  obj = Tvp_Kilian_Stock,
  var_names = colnames(Kilian_Stock),
  periods = 10,
  shock_scale = 1,
  which_shock = 1,
  which_response = 2,
  save = FALSE
)

irf_neg <- IRF.Rcpp_bvartvp(
  obj = Tvp_Kilian_Stock,
  periods = 10,
  var_names = colnames(Kilian_Stock),
  shock_scale = -1,
  which_shock = 1,
  which_response = 2,
  save = FALSE
)

# Compare raw posterior draws
head(irf_pos$draws)
head(irf_neg$draws)

irf_pos$


rf_neg_median <- -irf_pos$median  






irf_obj <- IRF.Rcpp_bvartvp(Tvp_Kilian_Stock, periods=10, which_shock=2, which_response=1, save=FALSE)
# Extract the median IRF
IRF_median <- irf_obj$plot_vals[,2,1,2]  # check indices carefully
# Negative shock
IRF_neg <- -IRF_median
plot(1:10, IRF_neg, type='l', col='red')



irf_obj2 <- IRF.Rcpp_bvartvp(Tvp_Kilian_Stock, periods=10, save=FALSE)

irf_obj2$plot_vals



# Positive shock
IRF_pos <- IRF_median

# Then plot manually
plot(1:10, IRF_pos, type='l', col='blue', ylim=range(c(IRF_pos, IRF_neg)))
lines(1:10, IRF_neg, col='red')
abline(h=0, lty=2)












plot_flip_irf <- function(irf_obj, which_shock=1, which_response=1, periods=10, response_name=NULL, shock_name=NULL) {
  
  # Extract the 4D array: periods x 4 x response x shock
  # Dimensions: 1=period, 2=c(lower, median, upper, time), 3=response, 4=shock
  irf_array <- irf_obj$plot_vals
  
  Time <- 1:periods
  
  # Extract lower, median, upper
  IRF_lower <- irf_array[,1,which_response,which_shock]
  IRF_median <- irf_array[,2,which_response,which_shock]
  IRF_upper <- irf_array[,3,which_response,which_shock]
  
  # Positive and negative shocks
  IRF_pos <- IRF_median
  IRF_neg <- -IRF_median
  
  # Flip confidence intervals for negative shock
  IRF_lower_neg <- -IRF_upper
  IRF_upper_neg <- -IRF_lower
  
  # Create a data frame for ggplot
  plot_df <- data.frame(
    Time = rep(Time, 4),
    IRF = c(IRF_pos, IRF_neg, IRF_lower, IRF_upper),
    Type = factor(rep(c("Positive Shock","Negative Shock","CI Lower","CI Upper"), each=periods))
  )
  
  # Better approach: use separate data frames for pos and neg
  library(ggplot2)
  
  df_pos <- data.frame(Time=Time, Median=IRF_pos, Lower=IRF_lower, Upper=IRF_upper)
  df_neg <- data.frame(Time=Time, Median=IRF_neg, Lower=IRF_lower_neg, Upper=IRF_upper_neg)
  
  p <- ggplot() +
    # Positive shock ribbon
    geom_ribbon(data=df_pos, aes(x=Time, ymin=Lower, ymax=Upper), fill="blue", alpha=0.2) +
    geom_line(data=df_pos, aes(x=Time, y=Median), color="blue", size=1.5) +
    # Negative shock ribbon
    geom_ribbon(data=df_neg, aes(x=Time, ymin=Lower, ymax=Upper), fill="red", alpha=0.2) +
    geom_line(data=df_neg, aes(x=Time, y=Median), color="red", size=1.5) +
    geom_hline(yintercept=0, linetype=2) +
    labs(
      x = "Horizon",
      y = "Response",
      title = paste0("IRF: Shock from ", ifelse(is.null(shock_name), which_shock, shock_name),
                     " to ", ifelse(is.null(response_name), which_response, response_name))
    ) +
    theme_minimal()
  
  print(p)
}


plot_flip_irf(irf_obj = irf_obj, which_shock=2, which_response=1, periods=10,
              response_name="Global GDP", shock_name="Oil Production")








###################################################Covid-19 Outbreak################################### 

target_index_Covid <- which(macro_data$date == "2020-03-01")
which_irfs_Covid <- target_index_Covid - tau - 2
print(which_irfs_Covid) #328






###################################################Ukraine Invasion##################################### 

target_index_Ukraine <- which(macro_data$date == "2022-03-01")
which_irfs_ukraine <- target_index_Ukraine - tau - 2
print(which_irfs_ukraine) #352














for (i in 1:3) {

  png(
    filename = paste0(
      "Processed_Data/IRF/Ukraine_War_IRF_TVP_",
      shock_names[i],
      "_to_SP500.png"
    ),
    width = 1800,
    height = 1800,
    res = 200
  )

  IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = which_irfs_ukraine,
    var_names = colnames(Kilian_Stock),
    which_shock = i,
    which_response = 4,
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
  )

  dev.off()
}

if (file.exists("IRFs.eps")) file.remove("IRFs.eps")



# Extract IRF
irf <- IRF.Rcpp_bvartvp(...)

# Interpret as negative supply shock
irf_neg_supply <- -1 * irf





    IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = which_irfs_ukraine,
    var_names = colnames(Kilian_Stock),
    shock_scale = -1,
    which_shock = 1,
    which_response = 4,
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
  )




IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = which_irfs_ukraine,
    var_names = colnames(Kilian_Stock),
    shock_scale = -1,
    which_shock = 1,
    which_response = 2:4,
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
)






 IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = which_irfs_ukraine,   # or whatever draws you use
    var_names = colnames(Kilian_Stock),
    shock_scale = 1,                    # positive 1 SD shock
    which_shock = 2,                    # shock to the second variable
    which_response = c(1,3,4),          # responses of 1st, 3rd, and 4th variables
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
)





 IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = which_irfs_ukraine,   # or whatever draws you use
    var_names = colnames(Kilian_Stock),
    shock_scale = 1,                    # positive 1 SD shock
    which_shock = 1,                    # shock to the second variable
    which_response = 3,          # responses of 1st, 3rd, and 4th variables
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
)







 IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = which_irfs_ukraine,   # or whatever draws you use
    var_names = colnames(Kilian_Stock),
    shock_scale = 1,                    # positive 1 SD shock
    which_shock = 3,                    # shock to the second variable
    which_response = c(1,2,4),          # responses of 1st, 3rd, and 4th variables
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
)







###################################################Full Sample########################################### 



# Calculate the correct index for Feb 2022 relative to the 386 slices
target_index_Ukraine <- 352 

# Ensure it's within the 386 limit
if(target_index_Ukraine <= 386){

  IRF.Rcpp_bvartvp(Tvp_Kilian_Stock, 
      periods = 10, 
      which_irfs = target_index_Ukraine, 
      var_names = colnames(Kilian_Stock),
      save = FALSE)
} else {
  print("Index still too high for the 386 available periods.")
}



IRF.Rcpp_bvartvp(Tvp_Kilian_Stock, periods = 10, which_irfs=386, var_names = colnames(Kilian_Stock), save=FALSE)




target_index_Ukraine <- which(macro_data$date == "2022-03-01")

print(target_index_Ukraine)

for (i in 1:3) {

  png(
    filename = paste0(
      "Processed_Data/TestUkraine_War_IRF_TVP_",
      shock_names[i],
      "_to_SP500.png"
    ),
    width = 1800,
    height = 1800,
    res = 200
  )

  IRF.Rcpp_bvartvp(
    obj = Tvp_Kilian_Stock,
    periods = 10,
    which_irfs = 387,
    var_names = colnames(Kilian_Stock),
    which_shock = 1,
    which_response = 4,
    percentiles = c(0.05, 0.5, 0.95),
    save = FALSE
  )

  dev.off()
}

if (file.exists("IRFs.eps")) file.remove("IRFs.eps")


target_index_Ukraine



