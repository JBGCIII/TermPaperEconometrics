
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
    select(
      oil_production_growth,
      real_activity,
      real_oil_price,
      real_sp500_return,
      fedfunds
    ),
  order.by = macro_data$date
)


#Stationary Test Oil for Production Growth
summary(ur.df(data_xts$oil_production_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$oil_production_growth))
#Value of ADF test-statistic is: (-16.9643)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.0405) < Critical Value -->  Stationary


#Stationary Test for Index of Global Real Economic Activity
summary(ur.df(data_xts$real_activity, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$real_activity))
#Value of ADF test-statistic is: (-4.2301)< Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.5176) < Critical Value -->  Stationary (5% level)
summary(ur.kpss(data_xts$real_activity, type = "tau"))
#Value of KPSS test-statistic with trend is: (0.5111) > Critical Value -->  Not Stationary


#Stationary Test Real Oil Price
summary(ur.df(data_xts$real_sp500_return, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$real_sp500_return))
#Value of ADF test-statistic is: (-16.585 ) < Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.0831) < Critical Value -->  Stationary 

#Stationary Test for Fed Funds
summary(ur.df(data_xts$fedfunds, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$fedfunds))
#Value of ADF test-statistic is: (-1.3983) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (4.3156) > Critical Value -->  Not Stationary 
