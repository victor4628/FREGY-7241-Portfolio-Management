#################################
### FRE7241 Homework #3 due at 6PM Tuesday February 10
#################################
# Max score 80pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and upload the file to Brightspace


############## Part I
# Summary: Simulate a volatility timing strategy
# using the inverse of the EMA volatility.

## Run the setup code below

library(rutils)

# The bid-ask spread for the most liquid ETFs
bidask <- 0.0001
# Calculate the SPY daily percentage returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
# Calculate the EMA volatility
volv <- HighFreq::run_var(retp, lambda=0.5)
volv <- sqrt(volv[, 2])

## End of setup code


# 1. (10pts)
# Calculate the positions as the inverse of the EMA 
# volatility.
# Lag the positions using the function rutils::lagit().

### write your code here
posv <- 1/volv
posv <- rutils::lagit(posv)


# You should get the following outputs:
NROW(posv)
# [1] 8298
head(posv)
# [1] 0.0000 201.1293 304.0789 328.5427 473.3764 329.8407
tail(posv)
# [1] 568.8807 345.5384 605.4911 473.4110 403.9418 397.2903


# Calculate the PnLs minus the transaction costs.
# Scale the PnL volatility to that of SPY.

### write your code here
pnls <- retp*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])


# You should get the following outputs:
head(pnls)
#                     SPY
# 1993-02-01  0.000000e+00
# 1993-02-02  1.437974e-03
# 1993-02-03  1.105102e-02
# 1993-02-04  4.744005e-03
# 1993-02-05 -1.165146e-03
# 1993-02-08 -2.484138e-05
tail(pnls)
#                     SPY
# 2026-01-09  0.012930946
# 2026-01-12  0.001838174
# 2026-01-13 -0.004239854
# 2026-01-14 -0.008096925
# 2026-01-15  0.003790361
# 2026-01-16 -0.001153826

# Calculate the Sharpe and Sortino ratios

### write your code here
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c(symboln, "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))


# You should get the following outputs:
#               SPY  Strategy
# Sharpe  0.5463022 0.6791695
# Sortino 0.6809656 0.8291571


# Plot dygraph of the strategy.
# Your plot should be similar to strat_vol_timing_spy.png

### write your code here
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main=paste("Volatility Timing Strategy for", symboln)) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# 2. (20pts)
# Calculate the Sharpe ratios of the strategy for
# the vector of lambda decay parameters lambdav.
# Use parallel computing with the package parallel.

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
lambdav <- seq(0.1, 0.9, 0.1)

### write your code here
pnlv <- unlist(mclapply(lambdav, function(lambdaf) {
  volv <- HighFreq::run_var(retp, lambda=lambdaf)
  volv <- sqrt(volv[, 2])
  posv <- 1/volv
  posv <- rutils::lagit(posv)
  pnls <- retp*posv
  costv <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costv)
  pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
  sqrt(252)*mean(pnls)/sd(pnls)
}, mc.cores=ncores))


# You should get the following outputs:
round(pnlv, 4)
# [1] 0.5959 0.6973 0.7080 0.6958 0.6792 0.6566 0.6386 0.6363 0.6544

# Calculate the largest Sharpe ratio and its lambda 
# decay parameter.
# You can use the functions max(), which.max().

### write your code here
max(pnlv)
lambdav[which.max(pnlv)]


# You should get the following outputs:
# [1] 0.7080153
# [1] 0.3

# Plot the Sharpe ratios versus lambda decay parameters.
# Your plot should be similar to strat_vol_timing_profile_spy.png

### write your code here
plot(pnlv ~ lambdav, type="l", lwd=2, col="blue",
  xlab="lambda", ylab="Sharpe ratio",
  main="Sharpe Ratios vs Lambda for Volatility Timing Strategy")



############## Part II
# Summary: Calculate the out-of-sample performance
# of the autoregressive strategy in trading time.  

## Run the setup code below

library(rutils)

# The code below is from the lecture slides.

# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
retp <- rutils::diffit(log(closep))
volumv <- quantmod::Vo(ohlc)
# Scale the returns using volume clock to trading time
volumr <- HighFreq::run_mean(volumv, lambda=0.8)
respv <- retp*volumr/volumv
# Calculate the AR coefficients
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% respv)
# Calculate the scaled in-sample forecasts of VTI
fcasts <- predm %*% coeff


## End of setup code


# 1. (10pts)
# Calculate the variance of the residuals and
# the t-values of the AR coefficients.
# Hint: Adapt the code from the lecture slides.

### write your code here
resids <- (fcasts - respv)
varv <- sum(resids^2)/(nrows-NROW(coeff))
pred2 <- crossprod(predm)
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
coefft <- drop(coeff/coefsd)
names(coefft) <- colnames(predm)


# You should get the output:
round(coefft, 3)
#  phi0   lag1   lag2   lag3   lag4   lag5 
# 5.480 -9.093 -3.078 -2.552 -0.570 -1.394

# Note that the t-values are more significant compared 
# to those without volume scaling.


# Plot the t-values of the AR coefficients
# Your plot should be similar to strat_ar_coeff_tradingtime.png

### write your code here
barplot(coefft, xlab="", ylab="t-value", col="grey",
  main="Coefficient t-values of AR Forecasting Model in Trading Time")



# 2. (20pts)
# Calculate the out-of-sample performance of
# the strategy.

# Define in-sample and out-of-sample intervals.
# Run this:
nrows <- NROW(retp)
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows

# Calculate the AR coefficients in-sample and 
# calculate the out-of-sample AR strategy PnLs.

### write your code here
predinv <- MASS::ginv(predm[insample, ])
coeff <- drop(predinv %*% respv[insample])
fcasts <- predm %*% coeff
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])


# Calculate the out-of-sample Sharpe and Sortino ratios.

### write your code here
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get outputs similar to:

#               VTI  Strategy
# Sharpe  0.7234336 0.4604408
# Sortino 0.8615924 0.5369187

# Plot a dygraph of the strategy PnLs.
# Your plot should be similar to strat_ar_tradingtime.png

### write your code here
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
datev <- zoo::index(wealthv)
cutoff <- nrows %/% 2
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="AR Strategy in Trading Time") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)


# 3. (20pts)
# Perform an sapply() loop over lambdav, and calculate
# the out-of-sample Sharpe ratio for each lambda.
# Create a vector of lambda decay parameters.
# Run this:
lambdav <- seq(0.3, 0.99, by=0.1)

### write your code here
sharper <- sapply(lambdav, function(lambdaf) {
  volumr <- HighFreq::run_mean(volumv, lambda=lambdaf)
  respv <- retp*volumr/volumv
  predm <- lapply(1:orderp, rutils::lagit, input=respv)
  predm <- rutils::do_call(cbind, predm)
  predm <- cbind(rep(1, nrows), predm)
  predinv <- MASS::ginv(predm[insample, ])
  coeff <- drop(predinv %*% respv[insample])
  fcasts <- predm %*% coeff
  pnls <- retp*fcasts
  pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
  sqrt(252)*mean(pnls[outsample])/sd(pnls[outsample])
})
names(sharper) <- lambdav


# You should get the output:
round(sharper, 3)
#  0.3   0.4   0.5   0.6   0.7   0.8   0.9
# 0.371 0.411 0.437 0.452 0.461 0.460 0.236

# Calculate the lambda decay parameter with the best 
# out-of-sample performance.

### write your code here
lambdab <- lambdav[which.max(sharper)]
lambdab


# You should get the output:
lambdab
# [1] 0.7


# Plot the out-of-sample Sharpe ratios vs lambda
# decay parameter.
# Your plot should be similar to strat_ar_sharpe_lambda.png

### write your code here
plot(sharper ~ lambdav, type="l", lwd=2, col="blue",
  xlab="lambda", ylab="Sharpe ratio",
  main="Out-of-Sample Sharpe Ratios vs Lambda for AR Strategy")


