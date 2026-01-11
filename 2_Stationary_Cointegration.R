
##########################################################################################################
###                               STATIONARY & COINTEGRATION TEST                                      ### 
##########################################################################################################
# Package installation

# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca", "dynlm")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))


# Read processed macro data
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")

data_xts <- xts(
  macro_data %>% 
     dplyr::select(
      oil_production_growth,
      real_activity,
      real_oil_price,
      real_sp500_return,
      fedfunds
    ),
  order.by = macro_data$date
)

##########################################################################################################
###                                 1. Unit root tests (1986-2024)                                     ### 


#Stationary Test Oil for Production Growth
summary(ur.df(data_xts$oil_production_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$oil_production_growth))
#Value of ADF test-statistic is: (-16.9643)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.0405) < Critical Value -->  Stationary

#Stationary Test for Index of Global Real Economic Activity
summary(ur.df(data_xts$real_activity, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$real_activity))
summary(ur.kpss(data_xts$real_activity, type = "tau"))
#Value of ADF test-statistic is: (-4.2301)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.5176) < Critical Value -->  'Stationary' (5% level)
#Value of KPSS test-statistic with trend is: (0.5111) > Critical Value -->  Not Stationary

#Stationary Test Real Oil Price
summary(ur.df(data_xts$real_oil_price, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$real_oil_price))
#Value of ADF test-statistic is: (-1.0921 ) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (3.5836) > Critical Value -->  Not Stationary 


#Stationary Test Real SP500 Return
summary(ur.df(data_xts$real_sp500_return, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$real_sp500_return))
#Value of ADF test-statistic is: (-16.585 ) < Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.0831) < Critical Value -->  Stationary 

#Stationary Test for Fed Funds [Not Used]
#summary(ur.df(data_xts$fedfunds, type = "none", selectlags = "AIC"))
#summary(ur.kpss(data_xts$fedfunds))
#Value of ADF test-statistic is: (-1.3983) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (4.3156) > Critical Value -->  Not Stationary 

#Start
##########################################################################################################
###                                 2. Unit root tests (1986-2020)                                     ### 


#Stationary Test Oil for Production Growth (Pre Covid)
summary(ur.df(data_xts["1986-01/2020-01"]$oil_production_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["1986-01/2020-01"]$oil_production_growth))
#Value of ADF test-statistic is: (-15.9197)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.0242) < Critical Value -->  Stationary


#Stationary Test for Index of Global Real Economic Activity (Pre Covid)
summary(ur.df(data_xts["1986-01/2020-01"]$real_activity, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["1986-01/2020-01"]$real_activity))
summary(ur.kpss(data_xts["1986-01/2020-01"]$real_activity,type = "tau"))
#Value of ADF test-statistic is: (-3.749)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.6349) < Critical Value -->  'Stationary' (1% level)
#Value of KPSS test-statistic with trend is: (0.6386) > Critical Value -->  Not Stationary


#Stationary Test for Real Oil Price (Pre Covid)
summary(ur.df(data_xts["1986-01/2020-01"]$real_oil_price, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["1986-01/2020-01"]$real_oil_price))
summary(ur.kpss(data_xts["1986-01/2020-01"]$real_oil_price,type = "tau"))
#Value of ADF test-statistic is: (-1.0041) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (3.7288) > Critical Value --> Not Stationary
#Value of KPSS test-statistic with trend is: (0.5849) > Critical Value -->  Not Stationary


#Stationary Test Real SP500 Return (Pre Covid)
summary(ur.df(data_xts["1986-01/2020-01"]$real_sp500_return, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["1986-01/2020-01"]$real_sp500_return))
#Value of ADF test-statistic is: (-15.24 ) < Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.0947) < Critical Value -->  Stationary 


#Stationary Test for Fed Funds (Pre Covid) [Not Used]
#summary(ur.df(data_xts["1986-01/2020-01"]$fedfunds, type = "none", selectlags = "AIC"))
#summary(ur.kpss(data_xts["1986-01/2020-01"]$fedfunds))
#Value of ADF test-statistic is: (-1.5807 ) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (4.7982) > Critical Value -->  Not Stationary 



##########################################################################################################
###                                 3. Unit root tests (1986-2005)                                     ### 

#Stationary Test for Index of Global Real Economic Activity 
summary(ur.df(data_xts["1986-01/2005-06"]$real_activity, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["1986-01/2005-06"]$real_activity))
summary(ur.kpss(data_xts["1986-01/2005-06"]$real_activity,type = "tau"))
#Value of ADF test-statistic is: (-2.3869)< Critical Value --> 'Stationary' (1% level)
#Value of KPSS test-statistic is: (0.8528) > Critical Value -->  Not Stationary
#Value of KPSS test-statistic with trend is: (0.3851) > Critical Value -->  Not Stationary


#Stationary Test for Real Oil Price
summary(ur.df(data_xts["1986-01/2005-06"]$real_oil_price, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["1986-01/2005-06"]$real_oil_price))
summary(ur.kpss(data_xts["1986-01/2005-06"]$real_oil_price,type = "tau"))
#Value of ADF test-statistic is: (-0.9591) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (0.5991) < Critical Value --> 'Stationary' (1%)
#Value of KPSS test-statistic with trend is: (0.5426) > Critical Value -->  Not Stationary


##########################################################################################################
###                                 4. Unit root tests (2007-2024)                                     ### 

#Stationary Test for Index of Global Real Economic Activity (Pre Covid)
summary(ur.df(data_xts["2005-06/2024-12"]$real_activity, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["2005-06/2024-12"]$real_activity))
summary(ur.kpss(data_xts["2005-06/2024-12"]$real_activity,type = "tau"))
#Value of ADF test-statistic is: (-3.1637)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (1.4728) > Critical Value --> Not stationary
#Value of KPSS test-statistic with trend is: (0.6551) > Critical Value -->  Not Stationary


#Stationary Test for Real Oil Price (Pre Covid)
summary(ur.df(data_xts["2005-06/2024-12"]$real_oil_price, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts["2005-06/2024-12"]$real_oil_price))
summary(ur.kpss(data_xts["2005-06/2024-12"]$real_oil_price,type = "tau"))
#Value of ADF test-statistic is: (-0.677) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (1.7359) > Critical Value --> Not Stationary
#Value of KPSS test-statistic with trend is: (0.2658) > Critical Value -->  Not Stationary



################################################ACF############################################################


dir.create("Processed_Data/ACF", showWarnings = FALSE)

png("Processed_Data/ACF/Graph_2_Oil_Production_Growth.png", width = 800, height = 600)
acf(data_xts$oil_production_growth, main = "ACF Plot for Oil Production Growth")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()


png("Processed_Data/ACF/Graph_2_Real_Activity.png", width = 800, height = 600)
acf(data_xts$real_activity, main = "ACF Plot for Real Activity")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()


png("Processed_Data/ACF/Graph_2_Real_Oil_Price.png", width = 800, height = 600)
acf(data_xts$real_oil_price, main = "ACF Plot for Real Oil Price")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()


png("Processed_Data/ACF/Graph_2_Real_Sp500_Return.png", width = 800, height = 600)
acf(data_xts$real_sp500_return, main = "ACF Plot for Real SP500 Return")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()

