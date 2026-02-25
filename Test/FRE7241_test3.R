#################################
### FRE7241 Test #3 Tuesday February 24
#################################
# Max score 90pts

# Please write in this file the R code needed to perform 
# the tasks below, rename the file to your_name_test3.R
# and upload it to Brightspace.


############## Part I
# Summary: Simulate a calendar strategy that holds 
# SPY only over the weekends.

## Run the setup code below

# Calculate the daily open and close prices,
# and the daily close-to-close returns.

library(rutils)
symboln <- "SPY"
ohlc <- log(get(symboln, rutils::etfenv))
nrows <- NROW(ohlc)
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)

## End of setup code


# 1. (20pts)
# Calculate the close prices on Fridays (closef) 
# and the open prices on the following business 
# day (openn).
# Note that the business day following some Fridays 
# is not always a Monday.

# Hint: You can use the function weekdays() to 
# determine the day of the week for a given date.
# Handle the case when the last Friday is also 
# the last day in the data.

# You can use the functions zoo::index(), 
# weekdays(), which(), NROW(), and min(). 

### write your code here

# You should get the outputs:
tail(closef)
#             SPY.Close
# 2025-12-12  6.521753
# 2025-12-19  6.522960
# 2025-12-26  6.537141
# 2026-01-02  6.526744
# 2026-01-09  6.542573
# 2026-01-16  6.539095

tail(openn)
#             SPY.Open
# 2025-12-15 6.527574
# 2025-12-22 6.527870
# 2025-12-29 6.533120
# 2026-01-05 6.531664
# 2026-01-12 6.537677
# 2026-01-16 6.541982


# 2. (20pts)
# Calculate the strategy pnls as the differences 
# between the open prices on the following business 
# day (openn), minus the close prices on Fridays 
# (closef). 
# Combine (cbind) the strategy pnls with the daily 
# SPY returns (retp).  Set the NA values to zero.

# Hint: You should apply the function zoo::coredata()
# before subtracting closef from openn.

# You can use the functions zoo::coredata(), 
# xts::xts(), zoo::index(), cbind(), colnames(), 
# is.na(), cumsum(), and dygraphs::dygraph().

### write your code here

# You should get the outputs:
head(wealthv)
#                       SPY      weekend
# 1993-01-29  0.0000000000 0.0000000000
# 1993-02-01  0.0070872019 0.0007098476
# 1993-02-02  0.0021152754 0.0000000000
# 1993-02-03  0.0105164716 0.0000000000
# 1993-02-04  0.0041753714 0.0000000000
# 1993-02-05 -0.0006957976 0.0000000000

tail(wealthv)
#                     SPY      weekend
# 2026-01-09  0.0065916196  0.000000000
# 2026-01-12  0.0015692149 -0.004896200
# 2026-01-13 -0.0020015414  0.000000000
# 2026-01-14 -0.0049272928  0.000000000
# 2026-01-15  0.0027195156  0.000000000
# 2026-01-16 -0.0008382109  0.002887422


# Calculate the Sharpe and Sortino ratios
# of wealthv.

### write your code here


# You should get the outputs:
#               SPY   weekend
# Sharpe  0.5462693 0.3386482
# Sortino 0.6808836 0.1546936

# Plot the cumulative wealths.
# Your plot should be similar to seasonal_weekend.png

### write your code here


