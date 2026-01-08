
##########################################################################################################
###                               STATIONARY & COINTEGRATION TEST                                      ### 
##########################################################################################################
# Package installation

# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca")

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
      oil_prod_gr,
      real_oil_gr,
      ind_prod_gr,
      inflation,
      sp500_ret,
      fed_funds,
      fedfunds_chg
    ),
  order.by = macro_data$date
)


adf.test(df$log_oil_price)

ur.df

#ADF Test Oil Production Growth
summary(ur.df(data_xts$oil_prod_gr, type = "none", selectlags = "AIC"))
#Value of test-statistic (-16.9643) < Critical Value --> Stationary

#ADF Test Oil Price Growth
summary(ur.df(data_xts$real_oil_gr, type = "none", selectlags = "AIC"))
#Value of test-statistic (-15.12449) < Critical Value --> Stationary

summary(ur.df(data_xts$ind_prod_gr, type = "none", selectlags = "AIC"))
#Value of test-statistic (-15.4) < Critical Value --> Stationary

summary(ur.df(data_xts$inflation, type = "none", selectlags = "AIC"))
#Value of test-statistic (-8.2931) < Critical Value --> Stationary

summary(ur.df(data_xts$sp500_ret, type = "none", selectlags = "AIC"))
#Value of test-statistic (-16.1867) < Critical Value --> Stationary


# FED Funds
summary(ur.df(data_xts$fed_funds, type = "drift", selectlags = "AIC"))
summary(ur.kpss(data_xts$fed_funds))
#Value of ADF test-statistic is: (-1.8082) (1.691) > Critical Value --> Not Stationary
#Value of KPSS test-statistic is: (4.3156) > Critical Value --> Not Stationary

summary(ur.df(data_xts$fedfunds_chg, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_xts$fedfunds_chg))
#Value of ADF test-statistic (-9.1977) < Critical Value --> Stationary
#Value of KPSS test-statistic is: (0.23) < Critical Value -->  Stationary










