

The data set is created using APIs and Data donwloaded from
https://www.eia.gov/international/data/world/petroleum-and-other-liquids/monthly-petroleum-and-other-liquids-production?pd=5&p=00000000000000000000000000000000002&u=0&f=M&v=line&a=-&i=none&vo=value&t=C&g=none&l=249--249&s=94694400000&e=1754006400000

VARIABLES FOR PROCESSED DATA SET

[Oil Supply Shock]
oil_production_growth   = 100 * (log(oil_prod_global) - lag(log(oil_prod_global))),
    
[Oil Demand Shock]
real_activity  = global_real_economic_activity, # Taken as level

[Oil Specific Demand Shock]
real_oil_price   = log(oil_price / cpi), #Logged and Deflated as in Killian (2009)
Data is taken from FRED ST.Louis using the Fredr package

[Real Return on S&P500]
real_sp500_return = 100 * ( (log(SP500) - lag(log(SP500))) - (log(cpi) - lag(log(cpi))) )
Data is taken from Yahoo Finance using the Quantmod Package


SCRIPTS

0.Data_Set_Creation.R -> Creates the Combined_Data_1986_2024.csv

1_Data_Set_Processing.R -> Transforms the Combined_Data_1986_2024.csv into Processed_1986_2024.csv

2_Stationary_Cointegration.R -> Runs ADF, KPSS and plots ACF Graphs

3_TVP_BVAR.R -> Runs the TVP-BVAR model and provide the script from graphs inside Processed_Data/IRF

4_BVAR.R -> Runs the BVAR model and provide the script from graphs inside Processed_Data/IRF2, Processed_Data/FEVD.
Can be run before 3_TVP_BVAR.R as well.