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
pnll <- lapply(rutils::etfenv$symbolv, function(symbol) {
  # Get the OHLC data for the symbol from etfenv
  ohlc_data <- get(symbol, envir = rutils::etfenv)
  
  # Calculate overnight returns: (Open - Previous Close) / Previous Close
  close_prices <- quantmod::Cl(ohlc_data)
  open_prices <- quantmod::Op(ohlc_data)
  
  # Calculate overnight returns
  overnight_returns <- (open_prices - lag(close_prices)) / lag(close_prices)
  
  # Remove the first NA value
  overnight_returns <- overnight_returns[-1]
  
  return(overnight_returns)
})

# Set the names of the list elements to the ETF symbols
names(pnll) <- rutils::etfenv$symbolv

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
pnlf <- sapply(pnll, function(x) sum(x))
pnlf <- sort(pnlf, decreasing = TRUE)

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
# Get the symbol of the most profitable strategy (first element in sorted pnlf)
best_symbol <- names(pnlf)[1]

# Get the overnight strategy PnLs for the best symbol from pnll
best_pnl <- pnll[[best_symbol]]

# Calculate cumulative PnL for the overnight strategy
cumulative_overnight <- cumsum(best_pnl)

# Get the ETF data
etf_data <- get(best_symbol, envir = rutils::etfenv)

# Calculate daily returns (Close to Close)
close_prices <- quantmod::Cl(etf_data)
daily_returns <- diff(log(close_prices))

# Align the indices - both should start from the same date
# The overnight returns start one day later than the price data
# So we need to align them properly
common_dates <- index(cumulative_overnight)
daily_returns_aligned <- daily_returns[common_dates]

# Calculate cumulative returns for buy-and-hold
cumulative_daily <- cumsum(daily_returns_aligned)

# Combine both series
combined_data <- merge(cumulative_overnight, cumulative_daily)
colnames(combined_data) <- c("overnight", "daily")

# Plot the cumulative returns
library(dygraphs)

### Write your code here
# Create dygraph
dygraph(combined_data, main = paste("Wealth of", best_symbol, "Overnight Strategy")) %>%
  dySeries("overnight", label = "overnight", color = "red") %>%
  dySeries("daily", label = "daily", color = "blue") %>%
  dyLegend(show = "always", width = 400)

# Your plot should be similar to strat_overnight_best.png


# Calculate the Sharpe and Sortino ratios of the combined returns

### Write your code here

# Extract the individual return series (not cumulative)
daily_rets <- daily_returns_aligned
overnight_rets <- best_pnl

# Combine the returns into a matrix
combined_returns <- merge(daily_rets, overnight_rets)
colnames(combined_returns) <- c("daily", "overnight")

# Calculate Sharpe ratios
sharpe_ratios <- apply(combined_returns, 2, function(x) {
  mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(252)
})

# Calculate Sortino ratios
sortino_ratios <- apply(combined_returns, 2, function(x) {
  # Calculate downside deviation (only negative returns)
  downside_returns <- x[x < 0]
  downside_dev <- sqrt(mean(downside_returns^2, na.rm = TRUE))
  mean(x, na.rm = TRUE) / downside_dev * sqrt(252)
})

# Combine into a matrix
ratios <- rbind(Sharpe = sharpe_ratios, Sortino = sortino_ratios)
print(ratios)

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
# Get XLK data
xlk_data <- get("XLK", envir = rutils::etfenv)

# Calculate XLK daily returns (Close to Close)
xlk_close <- quantmod::Cl(xlk_data)
xlk_daily <- diff(log(xlk_close))

# Get the best overnight strategy returns (already calculated)
best_overnight <- best_pnl

# Align the dates
common_dates <- index(best_overnight)
xlk_daily_aligned <- xlk_daily[common_dates]

# Create Treynor column (XLK daily returns squared)
treynor <- xlk_daily_aligned^2

# Combine into design matrix
desm <- cbind(best_overnight, xlk_daily_aligned, treynor)
colnames(desm) <- c("Overnight", "XLK", "Treynor")

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
# Get column names
col_names <- colnames(desm)

# Create formula string: "Overnight ~ XLK + Treynor"
formula_string <- paste(col_names[1], "~", paste(col_names[-1], collapse = " + "))

# Convert to formula object
formulav <- as.formula(formula_string)

# You should get the following outputs:
class(formulav)
# [1] "formula"
formulav
# Overnight ~ XLK + Treynor

# Perform the Treynor-Mazuy test for the overnight 
# strategy, using formulav, desm, and the function 
# lm().

### Write your code here
regmod <- lm(formulav, data = desm)

# You should get the following output:
summary(regmod)$coefficients
#                 Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)  0.0007037092 0.000105834  6.649180 3.175880e-11
# XLK          0.3547413014 0.006169506 57.499143 0.000000e+00
# Treynor     -0.7382392899 0.127601171 -5.785521 7.549745e-09


# Plot a scatterplot of the Treynor-Mazuy test.

### Write your code here
# Calculate residuals from the model
residuals_vec <- residuals(regmod)

# Get the t-value for the Treynor coefficient
treynor_tvalue <- summary(regmod)$coefficients["Treynor", "t value"]

# Extract XLK values
xlk_values <- as.numeric(desm[, "XLK"])

# Calculate fitted values from ONLY the Treynor term (quadratic effect)
treynor_coef <- coef(regmod)["Treynor"]
intercept <- coef(regmod)["(Intercept)"]
treynor_fitted <- intercept + treynor_coef * xlk_values^2

# Create the plot with residuals
plot(xlk_values, residuals_vec,
     xlab = "XLK",
     ylab = "residuals",
     main = "Treynor-Mazuy Test\nfor Overnight Strategy vs XLK",
     pch = 1,
     col = "black",
     cex = 0.8)

# Add horizontal line at y=0
abline(h = 0, col = "blue", lwd = 2)

# Add Treynor fitted values as red filled points
points(xlk_values, treynor_fitted, col = "red", pch = 16, cex = 0.8)

# Add text showing the t-value
text(x = 0, 
     y = max(residuals_vec, na.rm = TRUE) * 0.85,
     labels = paste("Treynor test t-value =", round(treynor_tvalue, 2)),
     cex = 1.2)

# Your plot should be similar to strat_overnight_test.png


# The Treynor coefficient is negative, suggesting that 
# the overnight strategy doesn't have market timing skill.
# The reason is that the overnight strategy stopped working
# after the Covid crisis in 2020.

# Repeat the Treynor-Mazuy test for data up to ["/2019"].
# Hint: First subset the desm design matrix.

### Write your code here
# Subset the design matrix to data up to 2019
desm_subset <- desm["/2019"]

# Perform the regression on the subset
regmod <- lm(formulav, data = desm_subset)

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
# Calculate residuals from the subset model
residuals_vec <- residuals(regmod)

# Get the t-value for the Treynor coefficient
treynor_tvalue <- summary(regmod)$coefficients["Treynor", "t value"]

# Extract XLK values from subset
xlk_values <- as.numeric(desm_subset[, "XLK"])

# Calculate fitted values from ONLY the Treynor term (quadratic effect)
treynor_coef <- coef(regmod)["Treynor"]
intercept <- coef(regmod)["(Intercept)"]
treynor_fitted <- intercept + treynor_coef * xlk_values^2

# Create the plot with residuals
plot(xlk_values, residuals_vec,
     xlab = "XLK",
     ylab = "residuals",
     main = "Treynor-Mazuy Test\nfor Overnight Strategy vs XLK",
     pch = 1,
     col = "black",
     cex = 0.8)

# Add horizontal line at y=0
abline(h = 0, col = "blue", lwd = 2)

# Add Treynor fitted values as red filled points
points(xlk_values, treynor_fitted, col = "red", pch = 16, cex = 0.8)

# Add text showing the t-value
text(x = 0, 
     y = max(residuals_vec, na.rm = TRUE) * 0.85,
     labels = paste("Treynor test t-value =", round(treynor_tvalue, 2)),
     cex = 1.2)

# Your plot should be similar to strat_overnight_test2.png


