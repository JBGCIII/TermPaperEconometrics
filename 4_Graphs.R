##########################################################################################################
#                                         GRAPHS
##########################################################################################################

library(readr)
library(dplyr)
library(xts)
library(zoo)

# 1. Load processed dataset
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")

# 2. Convert to xts
data_xts <- xts(
  macro_data %>% 
    select(real_oil_gr, oil_prod_gr, ind_prod_gr, inflation, sp500_ret, fedfunds_chg),
  order.by = macro_data$date
)

# 3. Save multiple plots in one PNG file
png("Processed_Data/graph_macro_oil_sp500.png", width = 1200, height = 800)
par(mfrow = c(3, 2))  # 3 rows, 2 columns

# Real Oil Growth
plot(data_xts$real_oil_gr, 
     main = "Real Oil Price Growth (%)",
     ylab = "Growth Rate (%)",
     col = "darkgreen",
     lwd = 1)

# Oil Production Growth
plot(data_xts$oil_prod_gr, 
     main = "Global Oil Production Growth (%)",
     ylab = "Growth Rate (%)",
     col = "brown",
     lwd = 1)

# Industrial Production Growth
plot(data_xts$ind_prod_gr, 
     main = "Industrial Production Growth (%)",
     ylab = "Growth Rate (%)",
     col = "blue",
     lwd = 1)

# Inflation
plot(data_xts$inflation, 
     main = "Inflation (%)",
     ylab = "Growth Rate (%)",
     col = "red",
     lwd = 1)

# S&P 500 Returns
plot(data_xts$sp500_ret, 
     main = "S&P 500 Monthly Returns (%)",
     ylab = "Return (%)",
     col = "purple",
     lwd = 1)

# Fed Funds Change
plot(data_xts$fedfunds_chg, 
     main = "Federal Funds Rate Change (%)",
     ylab = "Δ Fed Funds (%)",
     col = "orange",
     lwd = 1)

# Close PNG
dev.off()