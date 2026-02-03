#################################
### FRE7241 Homework #2 due at 6PM Tuesday February 3
#################################
# Max score 90pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and upload the file to Brightspace


############## Part I
# Summary: Create a shiny app for the distribution
# of the price z-scores.

# The z-scores are calculated as follows:

# Load the SPY prices
symboln <- "SPY"
pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))
# Calculate the EMA price and volatility
lambdaf <- 0.9
volp <- HighFreq::run_var(pricev, lambdaf=lambdaf)
pricema <- volp[, 1] # EMA price
volp <- sqrt(volp[, 2]) # EMA volatility
# Calculate the z-scores
zscores <- (pricev - pricema)/volp

# Note that the distribution of the z-scores depends
# on the  value of the decay parameter lambdaf.
# For large lambdaf closer to 1, the z-scores have 
# a wider distribution, and for small lambdaf closer 
# to 0, the distribution is narrower and bimodal, 
# between -1 and 1.

# 1. (50pts)
# Modify the file app_zscore_hw.R
# Replace missing code where it says:
#   Write your code here

# You must use the function HighFreq::run_var() to 
# calculate the EMA price and volatility.

# Your app should produce outputs similar to the images:
# price_zscores.jpeg and price_zscores2.jpeg

# Submit your corrected shiny app file to Brightspace.
# You must submit a complete shiny app, so that the 
# user can hit "run" and run it, without any 
# modifications.



############## Part II
# Summary: Simulate the overnight market anomaly
# for different ETFs.

## Run all the setup code below.

# Load the package rutils.
library(rutils)

## End of setup code.


# 1. (20pts)
# Calculate a list of the PnLs of the overnight Close-to-Open 
# strategy for all the ETFs in rutils::etfenv$symbolv.
# You can use the functions lapply(), get(), cumsum(), 
# and names().

### Write your code here

# You should get the following outputs:
is.list(pnll)
# [1] TRUE
NROW(pnll)
# [1] 35
names(pnll)
#  [1] "SPY"  "VTI"  "QQQ"  "VEU"  "EEM"  "XLY"  "XLP"  "XLE"  "XLF"  "XLV" 
# [11] "XLI"  "XLB"  "XLK"  "XLU"  "VYM"  "IVW"  "IWB"  "IWD"  "IWF"  "IEF" 
# [21] "TLT"  "VNQ"  "DBC"  "GLD"  "USO"  "VXX"  "SVXY" "MTUM" "IVE"  "VLUE"
# [31] "QUAL" "VTV"  "USMV" "AIEQ" "DYNF"
tail(pnll[[1]])
#                 SPY
# 2026-01-09  0.0016230241
# 2026-01-12 -0.0048962004
# 2026-01-13  0.0004745982
# 2026-01-14 -0.0040006697
# 2026-01-15  0.0060797484
# 2026-01-16  0.0020492106


# Calculate a named vector of the final cumulative PnLs 
# from the list pnll.  Sort it according to the final 
# cumulative PnLs. 
# You can use the functions sapply(), sum(), 
# and sort(decreasing=TRUE).

### Write your code here


# You should get the following outputs:
round(pnlf, 2)
#  XLK   QQQ   XLE   XLU   VTI   VYM   VTV   XLI   SPY   IVW   VNQ   IWD   XLF 
# 4.35  4.02  3.82  3.73  3.63  3.60  3.41  3.21  3.17  2.82  2.82  2.78  2.71 
#  XLB   IWF   IWB   IVE   XLY   EEM   XLV   GLD  MTUM  QUAL   VEU   XLP  DYNF 
# 2.66  2.66  2.63  2.52  2.46  2.41  2.25  2.24  2.12  2.10  1.62  1.51  1.26 
# USMV  VLUE  SVXY   DBC   IEF  AIEQ   TLT   USO   VXX 
# 1.19  1.13  0.50  0.40  0.32  0.16  0.10 -0.93 -4.47 


# Plot a dygraph of the most profitable overnight strategy, 
# combined with the cumulative returns of the underlying ETF.
# You must extract the strategy PnLs from the list pnll 
# using the functions get() and names(). 
# You can also use the functions, get(), names(),  
# dygraphs::dygraph(), dygraphs::dySeries(), 
# dygraphs::dyLegend(), and paste(). 

### Write your code here


# Plot the cumulative returns
library(dygraphs)

### Write your code here


# Your plot should be similar to strat_overnight_best.png


# Calculate the Sharpe and Sortino ratios of the combined returns

### Write your code here


# You should get the following outputs:
#             daily overnight
# Sharpe  0.3589635  1.004621
# Sortino 0.4760763  1.188695


# 2. (20pts)
# Perform the Treynor-Mazuy market timing skill test 
# for the best overnight strategy.

# Create a design matrix called desm, with a Treynor 
# column equal to XLK returns squared.
# You can use the functions cbind() and colnames().

### Write your code here


# You should get the following outputs:
tail(desm)
#               Overnight           XLK      Treynor
# 2026-01-09  0.0022852404  0.013154913 1.730517e-04
# 2026-01-12 -0.0065902621  0.004369502 1.909255e-05
# 2026-01-13  0.0004086637 -0.002114094 4.469392e-06
# 2026-01-14 -0.0067128138 -0.012226267 1.494816e-04
# 2026-01-15  0.0152936493  0.005238501 2.744189e-05
# 2026-01-16  0.0086248738  0.001099354 1.208580e-06


# Create a formula object from the column names 
# of the design matrix desm. 
# You cannot just type the formula!
# You can use the functions colnames(), paste() 
# with the argument collapse, and as.formula().

### Write your code here


# You should get the following outputs:
class(formulav)
# [1] "formula"
formulav
# Overnight ~ XLK + Treynor

# Perform the Treynor-Mazuy test for the overnight 
# strategy, using formulav, desm, and the function 
# lm().

### Write your code here


# You should get the following output:
summary(regmod)$coefficients
#                 Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)  0.0007037092 0.000105834  6.649180 3.175880e-11
# XLK          0.3547413014 0.006169506 57.499143 0.000000e+00
# Treynor     -0.7382392899 0.127601171 -5.785521 7.549745e-09


# Plot a scatterplot of the Treynor-Mazuy test.

### Write your code here


# Your plot should be similar to strat_overnight_test.png


# The Treynor coefficient is negative, suggesting that 
# the overnight strategy doesn't have market timing skill.
# The reason is that the overnight strategy stopped working
# after the Covid crisis in 2020.

# Repeat the Treynor-Mazuy test for data up to ["/2019"].
# Hint: First subset the desm design matrix.

### Write your code here


# You should get the following output:
summary(regmod)$coefficients

#                 Estimate   Std. Error    t value     Pr(>|t|)
# (Intercept) 0.0006071813 0.0001187511  5.1130602 3.281548e-07
# XLK         0.3294622956 0.0070700028 46.6000231 0.000000e+00
# Treynor     0.0977234591 0.1571505988  0.6218459 5.340700e-01

# The Treynor coefficient is now slightly positive, showing 
# that the market timing skill of the overnight strategy was 
# greater prior to 2020. 

# Plot a scatterplot of the Treynor-Mazuy test.

### Write your code here


# Your plot should be similar to strat_overnight_test2.png


