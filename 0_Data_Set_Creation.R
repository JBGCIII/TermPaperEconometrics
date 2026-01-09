
##########################################################################################################
#                                         DATA SET CREATION FULLL
##########################################################################################################

# Load or install required packages
required_packages <- c("readr", "fredr", "dplyr", "purrr", "zoo","xts", "lubridate", "quantmod" )

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("Raw_Data", showWarnings = FALSE)

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
#Note Traditionally I would have used R to do this but wanted to focus on the actual paper instead of this.

##########################################################################################################
#                                         S&P 500 (Yahoo)
##########################################################################################################


# 1. Download daily S&P 500 prices
getSymbols("^GSPC", src = "yahoo", from = "1986-01-01", to = "2024-12-03") 

# 2. Convert to data frame
SP500_daily <- data.frame(
  date = index(GSPC),
  SP500 = as.numeric(GSPC$GSPC.Adjusted)
)

# 3. Get first trading day of each month
# Note someday it is the 2nd or 3rd due to weekends but we merge monthly
SP500_monthly_start <- SP500_daily %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  slice_min(date) %>%   
  ungroup() %>%
  select(date, SP500)

# 4. Save CSV
write.csv(SP500_monthly_start, "Raw_Data/SP500_1986_2024.csv", row.names = FALSE)


##########################################################################################################
#                                         Macroeconomic Data (FEDR)
##########################################################################################################

# Set FRED API key from .Renviron
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Define Series for Kilian & Park (2009) Replication
# 1. WCOILWTICO: Crude Oil Price (Monthly)
# 2. IGREA: Index of Global Real Economic Activity (Killian Index)
# 3. CPIAUCSL: Consumer Price Index (For deflating)
# 4. FEDFUNDS: Effective Federal Funds Rate (For the monetary policy debate)

kilian_series <- c("WCOILWTICO", "IGREA", "CPIAUCSL", "FEDFUNDS")


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
         global_real_economic_activity = value.y, 
         cpi = value.x.x, 
         fed_funds = value.y.y,)

# Save Raw CSV

write.csv(raw_df, "Raw_Data/Macro_Data_1986_2024.csv")


##########################################################################################################
#                                         Combining Data Sets
##########################################################################################################

# Read Macro Data
macro <- read_csv("Raw_Data/Macro_Data_1986_2024.csv") %>%
  select(-...1) %>%                      # drop row index column
  mutate(date = floor_date(date, "month"))

# Read Oil Production data
# The bloated code is due to issues with the CSV file.
oil <- read_delim(
  "Raw_Data/Oil_Production_1986_2024.csv",
  delim = ";", #apparently the csv file
  col_names = c("date", "oil_prod_global"),
  col_types = cols(
    date = col_date(format = "%Y-%m-%d"),
    oil_prod_global = col_double()
  )
) %>%
  mutate(date = floor_date(date, "month"))


# Read S&P 500 data
sp500 <- read_csv("Raw_Data/SP500_1986_2024.csv") %>%
  mutate(date = floor_date(date, "month"))

# Merge everything by month
combined_df <- macro %>%
  left_join(oil,   by = "date") %>%
  left_join(sp500, by = "date") %>%
  arrange(date)

# Save final CSV
write_csv(combined_df, "Raw_Data/Combined_Data_1986_2024.csv")
