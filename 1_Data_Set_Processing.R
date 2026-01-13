

##########################################################################################################
#                                         DATA SET PROCESSING 
##########################################################################################################


# Load or install required packages
required_packages <- c("readr", "dplyr", "zoo","xts")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("Processed_Data", showWarnings = FALSE)


# Load combined raw dataset
df_raw <- read_csv("Raw_Data/Combined_Data_1986_2024.csv")

# Transform variables
df_processed <- df_raw %>%
  arrange(date) %>%
  mutate(
    oil_production_growth   = 100 * (log(oil_prod_global) - lag(log(oil_prod_global))),
    real_activity  = global_real_economic_activity, # Taken as level
    real_oil_price   = log(oil_price / cpi), #Logged and Deflated as in Killian (2009)
    real_sp500_return = 100 * ( (log(SP500) - lag(log(SP500))) - (log(cpi) - lag(log(cpi))) ),
    fedfunds   = fed_funds 

  ) %>%
  dplyr::select(
    date,
    oil_production_growth,
    real_activity,
    real_oil_price,
    real_sp500_return,
    fedfunds

  ) %>%
  na.omit()

#Save processed dataset
write_csv(
  df_processed,
  "Processed_Data/Processed_1986_2024.csv"
)


##########################################################################################################
#                                   Plot of Variable used in the model (Graph 1)                        ##

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
dev.off()
