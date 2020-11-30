# Financial Market - Petrobras Time Series Analysis


# Defining the working directory
setwd("D:/Documentos/FCD/BigDataRAzure/Cap07")
getwd()


# Loading packages
library(quantmod)
library(xts)
library(moments)


# Selecting the analysis period
startDate = as.Date("2020-01-01")
endDate = as.Date("2020-11-23")


# Download data from Yahoo Finance period
# Petrobras = PETR4.SA
getSymbols("PETR4.SA", src = "yahoo", from = startDate, to = endDate, auto.assign = T)
head(PETR4.SA)


# Analyzing the closing data
PETR4.SA.Close <- PETR4.SA[, "PETR4.SA.Close"]


# Petrobras candlestick chart
candleChart(PETR4.SA)


# Closing Plot
plot(PETR4.SA.Close, main = "Fechamento Diário Ações Petrobras",
     col = "red", xlab = "Data", ylab = "Preço", major.ticks = 'months',
     minor.ticks = FALSE)


# Adding Bollinger bands to the chart, with an average of 20 periods and 2 standard deviations
addBBands(n = 20, sd = 2)


# Adding the ADX indicator with 11 periods
addADX(n = 11, maType = "EMA")


# Calculating daily logs
PETR4.SA.ret <- diff(log(PETR4.SA.Close), lag = 1)


# Removing NA values in prosition 1
PETR4.SA.ret <- PETR4.SA.ret[-1] 


# Plotting return rate
plot(PETR4.SA.ret, main = "Fechamento Diário das Ações da Petrobras",
     col = "red", xlab = "Data", ylab = "Retorno", major.ticks = 'months',
     minor.ticks = FALSE)