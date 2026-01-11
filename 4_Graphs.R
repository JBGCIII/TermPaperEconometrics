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

# 3. Save multiple plots in one PNG file
png("Processed_Data/graph_macro_oil_sp500.png", width = 1200, height = 1000)
par(mfrow = c(2, 2))  # Updated to 3 rows, 2 columns to fit all variables

# 1. Real Oil Price (Logged/Deflated)
plot(data_xts$real_oil_price, 
     main = "Real Oil Price (Log levels)",
     ylab = "Log Price",
     col = "darkgreen",
     lwd = 1)

# 2. Oil Production Growth
plot(data_xts$oil_production_growth, 
     main = "Global Oil Production Growth (%)",
     ylab = "Percent Change",
     col = "brown",
     lwd = 1)

# 3. Global Real Economic Activity
plot(data_xts$real_activity, 
     main = "Global Real Economic Activity",
     ylab = "Index",
     col = "blue",
     lwd = 1)

# 4. Real S&P 500 Returns
# Note: Changed from 'sp500_ret' to 'real_sp500_return' per your list
plot(data_xts$real_sp500_return, 
     main = "Real S&P 500 Monthly Returns (%)",
     ylab = "Real Return (%)",
     col = "purple",
     lwd = 1)



# 5. Fed Funds Rate
#plot(data_xts$fedfunds, 
#     main = "Federal Funds Rate",
#     ylab = "Percent",
#     col = "darkred",
#     lwd = 1)

# Close PNG
dev.off()