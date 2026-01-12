

# Load or install required packages
required_packages <- c("readr", "dplyr", "zoo","xts")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))


# Read processed macro data
macro_data <- read_csv("Processed_Data/Processed_1986_2024.csv")%>%
  mutate(date = as.Date(date))

data_xts <- xts(
  macro_data %>% 
     dplyr::select(           #Sometimes R does not seem to understand a function unless called as such.
      oil_production_growth,
      real_activity,
      real_oil_price,
      real_sp500_return,
      #fedfunds
    ),
  order.by = macro_data$date
)

# Save plots
png("Processed_Data/Graph_01_Plotted_Processed_Variables.png", width = 1200, height = 1000)
par(mfrow = c(2, 2))  # Rows and Colomns

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
