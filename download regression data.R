library(tidyquant)
library(quantmod)
#S&P 500 Financials: ^SP500-40
#S&P 500 Information Technology: ^SP500-45
#S&P 500 Energy: ^SP500-10
# Define tickers for each sector index
#S&P 500 Healthcare Index: ^SP500-35
#S&P 500 Consumer Discretionary Index: ^SP500-25
#S&P 500 Consumer Staples Index: ^SP500-30
sector_tickers <- c("^SP500-40", "^SP500-45", "^SP500-10")


getSymbols("^SP500-40", src = "yahoo", from = "2020-01-01", to = "2024-01-01")  # Financials
Financials = data.frame(`SP500-40`)
Financials$Date = as.Date(rownames(Financials))

getSymbols("^SP500-45", src = "yahoo", from = "2020-01-01", to = "2024-01-01")  # Information Technology
IT = data.frame(`SP500-45`)
IT$Date = as.Date(rownames(IT))

getSymbols("XLE", src = "yahoo", from = "2020-01-01", to = "2024-01-01")  # Energy ETF
Energy = data.frame(XLE)
Energy$Date = as.Date(rownames(Energy))

getSymbols("^SP500-35", src = "yahoo", from = "2020-01-01", to = "2024-01-01")  # Healthcare
Healthcare = data.frame(`SP500-35`)
Healthcare$Date = as.Date(rownames(Healthcare))

getSymbols("^SP500-25", src = "yahoo", from = "2020-01-01", to = "2024-01-01")  # Consumer Discretionary
Consumer_discretionary = data.frame(`SP500-25`)
Consumer_discretionary$Date = as.Date(rownames(Consumer_discretionary))

getSymbols("^SP500-30", src = "yahoo", from = "2020-01-01", to = "2024-01-01")  # Consumer Staples
Consumer_staples = data.frame(`SP500-30`)
Consumer_staples$Date = as.Date(rownames(Consumer_staples))

sector_df = inner_join(Financials[,c('Date', 'SP500.40.Close')], IT[,c('Date', 'SP500.45.Close')], by = 'Date')
sector_df = inner_join(sector_df, Energy[, c('Date', 'XLE.Close')], by = 'Date')
sector_df = inner_join(sector_df, Healthcare[, c('Date', 'SP500.35.Close')], by = 'Date')
sector_df = inner_join(sector_df, Consumer_discretionary[, c('Date', 'SP500.25.Close')], by = 'Date')
sector_df = inner_join(sector_df, Consumer_staples[, c('Date', 'SP500.30.Close')], by = 'Date')
colnames(sector_df) = c('Date','Financials', 'IT', 'Energy', 'Healthcare', 'Consumer_discretionary', 'Consumer_staples')
head(sector_df)

