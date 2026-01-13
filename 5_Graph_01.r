

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
      real_sp500_return
      #fedfunds
    ),
  order.by = macro_data$date
)


# Convert xts to data frame for plotting
plot_df <- data.frame(
  date = index(data_xts),
  coredata(data_xts)
)

# Open PNG device
png("Processed_Data/Graph_01_Plotted_Processed_Variables.png", width = 1200, height = 1000)
par(mfrow = c(2, 2))

# Plot using numeric values with dates on X-axis
plot(plot_df$date, plot_df$real_oil_price, type="l",
     main="Real Oil Price (Log levels)",
     ylab="Log Price", col="darkgreen", lwd=1)

plot(plot_df$date, plot_df$oil_production_growth, type="l",
     main="Global Oil Production Growth (%)",
     ylab="Percent Change", col="brown", lwd=1)

plot(plot_df$date, plot_df$real_activity, type="l",
     main="Global Real Economic Activity",
     ylab="Index", col="blue", lwd=1)

plot(plot_df$date, plot_df$real_sp500_return, type="l",
     main="Real S&P 500 Monthly Returns (%)",
     ylab="Percent", col="purple", lwd=1)

# Close device
while(dev.cur() > 1) dev.off()
