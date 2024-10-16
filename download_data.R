library(quantmod)
library(MASS)
library(zoo)
library(mgcv)
library(pls)
library(dplyr)
library(car)
library(forecast)

getSymbols("^VIX", src = "yahoo", from = "2015-01-01", to = Sys.Date())
vix_data = data.frame(VIX)
vix_data$Date = as.Date(rownames(vix_data))
plot(x = vix_data$Date, vix_data$VIX.Close)

getSymbols("CL=F", src = "yahoo", from = "2015-01-01", to = Sys.Date()) #(WTI Crude)
oil_data = `CL=F`
oil_data = data.frame(`CL=F`)
oil_data$Date = as.Date(rownames(oil_data))
plot(x = oil_data$Date, oil_data$CL.F.Close)

getSymbols("GC=F", src = "yahoo", from = "2015-01-01", to = Sys.Date())
gold_data = `GC=F`
gold_data = data.frame(`GC=F`)
gold_data$Date = as.Date(rownames(gold_data))
plot(x = gold_data$Date, gold_data$GC.F.Close)

getSymbols("^GSPC", src = "yahoo", from = "2015-01-01", to = Sys.Date())
SP500_data = data.frame(GSPC)
SP500_data$Date = as.Date(rownames(SP500_data))
plot(x = SP500_data$Date, SP500_data$GSPC.Close)

getSymbols("^FTSE", from = "2015-01-01", to = Sys.Date(), src = "yahoo")
ftse_data <- data.frame(FTSE)  # Extract closing prices
ftse_data$Date = as.Date(rownames(ftse_data))

# Nikkei 225 (^N225)
getSymbols("^N225", from = "2015-01-01", to = Sys.Date(), src = "yahoo")
nikkei_data <- data.frame(N225)  # Extract closing prices
nikkei_data$Date = as.Date(rownames(nikkei_data))

# Shanghai Composite (^SSEC)
getSymbols("000001.SS", from = "2015-01-01", to = Sys.Date(), src = "yahoo")
shanghai_data <- data.frame(`000001.SS`)  # Extract closing prices
shanghai_data$Date = as.Date(rownames(shanghai_data))

external_df = inner_join(vix_data[,c('Date', 'VIX.Close')], oil_data[,c('Date', 'CL.F.Close')], by = 'Date')
external_df = inner_join(external_df, gold_data[, c('Date', 'GC.F.Close')], by = 'Date')
external_df = inner_join(external_df, SP500_data[, c('Date', 'GSPC.Close')], by = 'Date')
external_df = inner_join(external_df, nikkei_data[, c('Date', 'N225.Close')], by = 'Date')
external_df = inner_join(external_df, shanghai_data[, c('Date', 'X000001.SS.Close')], by = 'Date')
external_df = inner_join(external_df, ftse_data[, c('Date', 'FTSE.Close')], by = 'Date')
colnames(external_df) = c('Date', 'VIX', "Oil", 'Gold', 'SP500', 
                          'Nikkei', 'SSE', 'FTSE')

library(timeDate)
start_date = min(external_df$Date)
end_date = max(external_df$Date)
# Generate a sequence of all dates between the range
all_dates <- seq.Date(from = start_date, to = end_date, by = "day")

# Exclude weekends
all_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]

# Generate a list of public holidays for the Shanghai Stock Exchange
nyse_holidays <- holidayNYSE(year = as.numeric(unique(format(all_dates, "%Y"))))

# Remove holidays from the list of all dates
trading_days <- all_dates[!all_dates %in% as.Date(nyse_holidays)]

# Identify the dates present in 'external_df'
present_dates <- external_df$Date

# Find missing trading dates by set difference
missing_dates <- setdiff(trading_days, present_dates)

# Print the missing dates in a readable format
if (length(missing_dates) > 0) {
  print("Missing dates (excluding weekends and public holidays):")
  print(as.Date(missing_dates, origin = "1970-01-01"))
} else {
  print("No missing trading dates.")
}

external_df = na.omit(external_df)
write.csv(external_df, file = 'external_df.csv', row.names = FALSE)
