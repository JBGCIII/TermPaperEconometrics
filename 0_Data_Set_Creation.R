
##########################################################################################################
#                                         DATA SET CREATION FULLL
##########################################################################################################

# Load or install required packages
required_packages <- c("fredr", "dplyr", "purrr", "zoo","xts", "lubridate", "quantmod" )

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))


##########################################################################################################
#                                         OIL (EIA)
##########################################################################################################

#Data for crude oil including lease condensate production was retrieved from
#https://www.eia.gov/international/data/world/petroleum-and-other-liquids/monthly-petroleum-and-other-liquids
#-production?pd=5&p=00000000000000000000000000000000002&u=0&f=M&v=line&a=-&i=none&vo=value&t=C&g=none&l=249
#--249&s=94694400000&e=1754006400000

#Polished in Excel as follows
#1) Replace """" with ""
#2) Text to column (Delimited by "")
#3) Transposed vertically.
#4) Data from 1986-01-01 to 2024-01-01 was copied to a separate excel file and saved as CSV.
#Note Traditionally I would have used R to do this but did not have the time.

##########################################################################################################
#                                         S&P 500 (Yahoo)
##########################################################################################################


# 1. Download daily S&P 500 prices
getSymbols("^GSPC", src = "yahoo", from = "1986-01-01", to = "2024-12-01")

# 2. Convert to data frame
SP500_daily <- data.frame(
  date = index(GSPC),
  SP500 = as.numeric(GSPC$GSPC.Adjusted)
)

# 3. Get first trading day of each month
SP500_monthly_start <- SP500_daily %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  slice_min(date) %>%   # first trading day of the month (note someday it is the 2nd or 3rd due to weekends)
  ungroup() %>%
  select(date, SP500)

# 4. Save CSV
dir.create("Raw_Data", showWarnings = FALSE)
write.csv(SP500_monthly_start, "Raw_Data/SP500_StartMonth_1986_2024.csv", row.names = FALSE)


##########################################################################################################
#                                         Macroeconomic Data (FEDR)
##########################################################################################################

# Set FRED API key from .Renviron
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Define Series for Kilian & Park (2009) Replication
# 1. WCOILWTICO: Crude Oil Price (Monthly)
# 2. INDPRO: Industrial Production (Real Activity Proxy)
# 3. CPIAUCSL: Consumer Price Index (For deflating)
# 4. FEDFUNDS: Effective Federal Funds Rate (For the monetary policy debate)

kilian_series <- c("WCOILWTICO", "INDPRO", "CPIAUCSL", "FEDFUNDS")


# Download Raw Data
raw_data_list <- map(kilian_series, ~fredr(
  series_id = .x,
  observation_start = as.Date("1986-01-01"),
  observation_end = as.Date("2024-12-01"),
  frequency = "m"
))

# Pivot wider to get columns
raw_df <- raw_data_list %>%
  reduce(full_join, by = "date") %>%
  select(date, 
         oil_price = value.x, 
         ind_prod = value.y, 
         cpi = value.x.x, 
         fed_funds = value.y.y,)

# Save Raw CSV
dir.create("Raw_Data", showWarnings = FALSE)
write.csv(raw_df, "Raw_Data/Kilian_Park_Raw_1986_2024.csv")


