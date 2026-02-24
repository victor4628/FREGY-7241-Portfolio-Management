#################################
### FRE7241 Homework #4 due at 6PM Tuesday February 24
#################################
# Max score 80pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw4.R
# and upload the file to Brightspace


############## Part I
# Summary: Simulate a volatility timing strategy
# using the inverse of the range volatility.

## Run the setup code below

library(rutils)

# The bid-ask spread for the most liquid ETFs
bidask <- 0.0001
# Calculate the SPY daily percentage returns
symboln <- "SPY"
ohlc <- get(symboln, rutils::etfenv)
# ohlc <- rutils::etfenv$SPY[-(1:333)]
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
pricev <- log(quantmod::Cl(ohlc))
colnames(pricev) <- symboln
retp <- rutils::diffit(pricev)
pricev <- cumsum(retp)
# volumv <- quantmod::Vo(ohlc)
# retsc <- retp/volumv

# Calculate the EMA volatility
lambdaf <- 0.9
volma <- HighFreq::run_var(retp, lambda=lambdaf)
volma <- sqrt(volma[, 2])
# Calculate the range volatility
volohlc <- HighFreq::run_var_ohlc(ohlc=log(ohlc[, 1:4]), lambdaf=lambdaf^2)
volohlc <- sqrt(volohlc)

## End of setup code


# 1. (20pts)
# Calculate the positions for two strategies: 
# one using the inverse of the EMA volatility, 
# and one using the range volatility.
# Lag the positions using the function rutils::lagit().
# Hint:
# In HighFreq::run_var_ohlc(), the lambda parameter 
# is squared, to be consistent with HighFreq::run_var().

### write your code here
# EMA volatility positions
posv <- 1/volma
posv <- rutils::lagit(posv)

# Range volatility positions
posrange <- 1/volohlc
posrange <- rutils::lagit(posrange)


# You should get the following outputs for the
# EMA volatility:
NROW(posv)
# [1] 8298
head(posv)
# [1] 0.0000 141.0994 143.7174 159.0696 146.8142 161.2988
tail(posv)
# [1] 231.6756 221.7058 246.2758 257.4201 237.8982 257.1603

# You should get the following outputs for the 
# range volatility:
NROW(posrange)
# [1] 8298
head(posrange)
# [1] 0.0000 229.1834 245.7928 251.2531 238.5191 159.4126
tail(posrange)
# [1] 207.0834 202.2536 191.6900 195.5563 176.7174 168.2157


# Calculate the PnLs minus the transaction costs.
# Scale the PnL volatility to that of SPY.

### write your code here
# EMA strategy PnL
pnls <- retp*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Range strategy PnL
pnlrange <- retp*posrange
costv <- 0.5*bidask*abs(rutils::diffit(posrange))
pnlrange <- (pnlrange - costv)
pnlrange <- pnlrange*sd(retp[retp<0])/sd(pnlrange[pnlrange<0])


# You should get the following outputs:
head(pnls)
#                     SPY
# 1993-01-29  0.000000000
# 1993-02-01  0.009807797
# 1993-02-02  0.003001479
# 1993-02-03  0.016515976
# 1993-02-04  0.006048883
# 1993-02-05 -0.001115717
tail(pnls)
#                     SPY
# 2026-01-09  0.015073012
# 2026-01-12  0.003431492
# 2026-01-13 -0.004881054
# 2026-01-14 -0.012533947
# 2026-01-15  0.006380773
# 2026-01-16 -0.002138650
head(pnlrange)
#                     SPY
# 1993-01-29  0.000000000
# 1993-02-01  0.019724667
# 1993-02-02  0.006348458
# 1993-02-03  0.032311951
# 1993-02-04  0.012172145
# 1993-02-05 -0.001404912
tail(pnlrange)
#                     SPY
# 2026-01-09  0.016686354
# 2026-01-12  0.003878598
# 2026-01-13 -0.004698811
# 2026-01-14 -0.011786745
# 2026-01-15  0.005866044
# 2026-01-16 -0.001729632


# Calculate the Sharpe and Sortino ratios

### write your code here
wealthv <- cbind(retp, pnls, pnlrange)
colnames(wealthv) <- c("SPY", "EMA", "Range")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))


# You should get the following outputs:
#               SPY       EMA     Range
# Sharpe  0.5462693 0.6538808 0.7143018
# Sortino 0.6808836 0.8537811 0.9742983


# 2. (10pts)
# Plot dygraph of the two strategies.
# Your plot should be similar to
# strat_vol_timing_ema_range.png

### write your code here
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
    main="Volatility Timing Strategy (EMA vs Range)") %>%
  dyOptions(colors=c("blue", "green", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# Conclusion:
# The range volatility strategy has slightly higher Sharpe 
# and Sortino ratios than the EMA volatility strategy.
# The range volatility has an advantage over the EMA 
# volatility because it uses more information from the 
# OHLC prices.



############## Part II
# Summary: Create a shiny app which simulates 
# a dual crossover volatility strategy.

# 1. (50pts)
# The strategy calculates the fast and slow EMA 
# volatilities of the percentage returns of the 
# selected ETF, using the function HighFreq::run_var().
# If the fast volatility is above the slow volatility,
# then the strategy goes short the ETF.
# If the fast volatility is below the slow volatility,
# then the strategy goes long the ETF.

# The shiny app should:
# - Load the OHLC prices for the selected ETF.
# - Calculate the volatilities.
# - Calculate the positions of the strategy.
# - Calculate the strategy PnLs.
# - Calculate the strategy Sharpe ratio.
# - Plot the strategy with or without shading 
#     for the positions.


# The shiny app output should be similar to: 
# shiny_dual_cross_vol_strat1.png
# shiny_dual_cross_vol_strat2.png
# shiny_dual_cross_vol_strat3.png
# shiny_dual_cross_vol_strat4.png

# You must upload your shiny app to Brightspace.
# You must submit a complete shiny app, so that 
# the user can hit "run" and run it, without any 
# modifications.

# Hint: 
# Before running the app, delete everything from 
# your workspace, so that the app doesn't use 
# objects from your workspace:
rm(list=ls())

