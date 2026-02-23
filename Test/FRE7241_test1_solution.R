#################################
### FRE7241 Test #1 Solution Tuesday February 3, 2026
#################################
# Max score 60pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the largest drawdown (drop) of 
# stock prices over any 1 year period.  

## Run all the setup code below.

# Load the package rutils.
library(rutils)

# Calculate the log VTI prices.
pricev <- log(quantmod::Cl(rutils::etfenv$VTI))
# Calculate the VTI dates.
datev <- zoo::index(pricev)

## End of setup code.


# 1. (30pts)
# Calculate the VTI prices lagged by 1 year,
# and the price differences over 1 year.
# Assume that 1 year has 252 business days.
# You can use the function rutils::lagit()
# with pad_zeros=FALSE.

yearb <- 252
pricel <- rutils::lagit(pricev, yearb, pad_zeros=FALSE)
priced <- (pricev - pricel)

# You should get the following price 
# differences over 1 year:
head(priced, 3)
#            VTI.Close
# 2001-05-31 0.000000000
# 2001-06-01 0.006944472
# 2001-06-04 0.011260405
tail(priced, 3)
#            VTI.Close
# 2026-01-14 0.1818289
# 2026-01-15 0.1826471
# 2026-01-16 0.1638871


# Calculate the largest drawdown (drop) of log VTI 
# prices over any 1 year period.  
# That means the largest drawdown between any two 
# dates separated by 1 year.
# You can use the function min().

min(priced)

# You should get the following output:
# [1] -0.6426144

# Calculate the start and end dates of the largest 
# drawdown.
# Your code should work even when the start of the
# drawdown is at the beginning of the series.
# You can use the functions which.min() and max().

indexmin <- which.min(priced)
datev[max(indexmin-yearb, 1)]
datev[indexmin]

# You should get the following outputs:
# [1] "2008-03-05"
# [1] "2009-03-05"


# Calculate the largest rally (increase) of log 
# VTI prices over any 1 year period.  
# Calculate the start and end dates of this rally.
# You can use the functions max() and which.max().

max(priced)
indexmax <- which.max(priced)
datev[max(indexmax-yearb, 1)]
datev[indexmax]

# You should get the following outputs:
# [1] 0.6177547
# [1] "2020-03-23"
# [1] "2021-03-23"



############## Part II
# Summary: Calculate the overnight and daytime returns 
# of a time series of intraday 1-minute returns.

## Run the setup code below

# Load the file SPY_minutes_markets.RData with intraday 1-minute prices for SPY.
# Download the file from the share drive:
# https://drive.google.com/uc?export=download&id=1mfS_Q5V7bDfaZ28dHzCYJN5PHUEYntbp
# Or
# https://drive.google.com/file/d/1mfS_Q5V7bDfaZ28dHzCYJN5PHUEYntbp/view?usp=sharing

load("/Users/jerzy/Develop/data/minutes/SPY_minutes_markets.RData")
pricev <- quantmod::Cl(ohlc)

# The pricev is a time series of intraday SPY minute prices 
# during market hours (09:30:00 to 16:00:00), over several 
# months.
# The first price on each day is at around 09:30, and the 
# last price is at around 16:00.


# Plot a dygraph of pricev.
dygraph(pricev, main="SPY Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

## End of setup code


# 1. (10pts)
# Calculate the end of day points of pricev using  
# the function rutils::calc_endpoints().

endd <- rutils::calc_endpoints(pricev, interval="days")

# You should get the outputs:
NROW(endd)
# [1] 152
head(endd)
# [1] 0  367  722 1065 1397 1761
tail(endd)
# [1] 51496 51825 52156 52539 52914 53270

# Calculate the start of day points by adding 1 to the 
# end points, after removing the last end point. 

startp <- endd[-NROW(endd)] + 1

# You should get the outputs:
NROW(startp)
# [1] 151
head(startp)
# [1] 1  368  723 1066 1398 1762
tail(startp)
# [1] 51141 51497 51826 52157 52540 52915

# Calculate the first and last prices of each trading day.

pricef <- pricev[startp]
pricel <- pricev[endd]

# You should get the outputs:
NROW(pricef)
# [1] 151
head(pricef)
#                     SPY.Close
# 2025-06-02 09:31:00    586.35
# 2025-06-03 09:30:00    589.96
# 2025-06-04 09:30:00    597.21
# 2025-06-05 09:30:00    596.55
# 2025-06-06 09:30:00    595.54
# 2025-06-09 09:32:00    599.84
tail(pricef)
#                     SPY.Close
# 2025-12-29 09:30:00    688.90
# 2025-12-30 09:30:00    688.11
# 2025-12-31 09:32:00    685.28
# 2026-01-02 09:30:00    686.44
# 2026-01-05 09:30:00    684.40
# 2026-01-06 09:30:00    687.11
NROW(pricel)
# [1] 151
head(pricel)
#                     SPY.Close
# 2025-06-02 16:00:00    589.68
# 2025-06-03 16:00:00    595.54
# 2025-06-04 16:00:00    596.88
# 2025-06-05 16:00:00    598.73
# 2025-06-06 16:00:00    598.83
# 2025-06-09 16:00:00    599.78
tail(pricel)
#                     SPY.Close
# 2025-12-29 16:00:00  687.3300
# 2025-12-30 16:00:00  687.6700
# 2025-12-31 16:00:00  684.5564
# 2026-01-02 16:00:00  681.2900
# 2026-01-05 16:00:00  688.0700
# 2026-01-06 16:00:00  689.0100


# 2. (20pts)
# Calculate the overnight returns equal to the difference
# between the first price today minus the last price 
# yesterday.
# Hint: Lag the last price yesterday using the function 
# rutils::lagit().
# You should coerce pricel to numeric using the function 
# as.numeric().
# Ignore the first overnight return.

reton <- pricef - rutils::lagit(as.numeric(pricel))
reton <- reton[-1]

# You should get the outputs:
NROW(reton)
# [1] 150
head(reton)
#                     SPY.Close
# 2025-06-03 09:30:00      0.28
# 2025-06-04 09:30:00      1.67
# 2025-06-05 09:30:00     -0.33
# 2025-06-06 09:30:00     -3.19
# 2025-06-09 09:32:00      1.01
# 2025-06-10 09:30:00      0.15
tail(reton)
#                     SPY.Close
# 2025-12-29 09:30:00   -1.8800
# 2025-12-30 09:30:00    0.7800
# 2025-12-31 09:32:00   -2.3900
# 2026-01-02 09:30:00    1.8836
# 2026-01-05 09:30:00    3.1100
# 2026-01-06 09:30:00   -0.9600


# Plot a dygraph of the cumulative overnight returns.

dygraph(cumsum(reton), main="SPY Overnight Returns") %>%
  dyOptions(colors="blue", strokeWidth=2)

# Your plot should be similar to returns_overnight.png


# Calculate the daytime returns equal to the difference
# between the last price minus the first price of each day.
# You should coerce pricef to numeric using the function 
# as.numeric().

retd <- pricel - as.numeric(pricef)

# You should get the outputs:
NROW(retd)
# [1] 151
head(retd)
#                     SPY.Close
# 2025-06-02 16:00:00      3.33
# 2025-06-03 16:00:00      5.58
# 2025-06-04 16:00:00     -0.33
# 2025-06-05 16:00:00      2.18
# 2025-06-06 16:00:00      3.29
# 2025-06-09 16:00:00     -0.06
tail(retd)
#                     SPY.Close
# 2025-12-29 16:00:00   -1.5700
# 2025-12-30 16:00:00   -0.4400
# 2025-12-31 16:00:00   -0.7236
# 2026-01-02 16:00:00   -5.1500
# 2026-01-05 16:00:00    3.6700
# 2026-01-06 16:00:00    1.9000


# Plot a dygraph of the cumulative daytime returns.

dygraph(cumsum(retd), main="SPY Daytime Returns") %>%
  dyOptions(colors="blue", strokeWidth=2)

# Your plot should be similar to returns_daytime.png


