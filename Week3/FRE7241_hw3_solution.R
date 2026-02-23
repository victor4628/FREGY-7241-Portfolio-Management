#################################
### FRE7241 Homework #3 due 6PM Tuesday February 10
#################################
# Max score 80pts

# The below solutions are examples,
# Slightly different solutions are also possible.


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

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main="Volatility Timing Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



# 2. (20pts)
# Calculate the Sharpe ratios of the strategy for 
# the vector of lambda decay parameters lambdav.
# Use parallel computing with the package parallel.

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
lambdav <- seq(0.1, 0.9, 0.1)

pnlv <- mclapply(lambdav, function(lambdaf) {
  # Calculate the EMA volatility
  volv <- HighFreq::run_var(retp, lambda=lambdaf)
  volv <- sqrt(volv[, 2])
  # Calculate the positions and PnLs
  posv <- 1/volv
  posv <- rutils::lagit(posv)
  pnls <- retp*posv
  costv <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costv)
  return(sqrt(252)*mean(pnls)/sd(pnls))
}, mc.cores=ncores)  ## end mclapply
pnlv <- unlist(pnlv)

# You should get the following outputs:
round(pnlv, 4)
# [1] 0.5959 0.6973 0.7080 0.6958 0.6792 0.6566 0.6386 0.6363 0.6544

# Calculate the largest Sharpe ratio and its lambda 
# decay parameter.
# You can use the functions max(), which.max().

max(pnlv)
lambdaf <- lambdav[which.max(pnlv)]

# You should get the following outputs:
# [1] 0.7080153
# [1] 0.3

# Plot the Sharpe ratios versus lambda decay parameters.
# Your plot should be similar to strat_vol_timing_profile_spy.png

plot(lambdav, pnlv, type="b",
     main="Volatility Timing Strategy",
     xlab="Lambda Decay Factor", ylab="Sharpe Ratio")



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

resids <- (fcasts - respv)
# Calculate the variance of the residuals
varv <- sum(resids^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
pred2 <- crossprod(predm)
# Calculate the covariance matrix of the AR coefficients
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
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

barplot(coefft, xlab="", ylab="t-value", col="grey", 
        main="Coefficient t-values of AR Trading Time Strategy")



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

predinv <- MASS::ginv(predm[insample, ])
coeff <- drop(predinv %*% respv[insample, ])
fcasts <- predm %*% coeff

# Calculate the AR strategy PnLs
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the out-of-sample Sharpe and Sortino ratios.
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv[outsample], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get outputs similar to:

#               VTI  Strategy
# Sharpe  0.7234336 0.4604408
# Sortino 0.8615924 0.5369187

# Plot a dygraph of the strategy PnLs.
# Your plot should be similar to strat_ar_tradingtime.png

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Autoregressive Strategy Out-of-Sample") %>%
  dyEvent(zoo::index(wealthv[(nrows %/% 2)]), label="in-sample", strokePattern="solid", color="red") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# 3. (20pts)
# Perform an sapply() loop over lambdav, and calculate
# the out-of-sample Sharpe ratio for each lambda.
# Create a vector of lambda decay parameters.
# Run this:
lambdav <- seq(0.3, 0.99, by=0.1)


sharper <- sapply(lambdav, function(lambda) {
  volumr <- HighFreq::run_mean(volumv, lambda=lambda)
  respv <- retp*volumr/volumv
  predm <- lapply(1:orderp, rutils::lagit, input=respv)
  predm <- rutils::do_call(cbind, predm)
  predm <- cbind(rep(1, nrows), predm)
  predinv <- MASS::ginv(predm[insample, ])
  coeff <- drop(predinv %*% respv[insample, ])
  fcasts <- predm %*% coeff
  pnls <- retp*fcasts
  sqrt(252)*mean(pnls[outsample])/sd(pnls[outsample])
}) # end sapply
names(sharper) <- round(lambdav, 2)

# You should get the output:
round(sharper, 3)
#  0.3   0.4   0.5   0.6   0.7   0.8   0.9
# 0.371 0.411 0.437 0.452 0.461 0.460 0.236

# Calculate the lambda decay parameter with the best 
# out-of-sample performance.
lambdab <- lambdav[which.max(sharper)]

# You should get the output:
lambdab
# [1] 0.7


# Plot the out-of-sample Sharpe ratios vs lambda 
# decay parameter.
# Your plot should be similar to strat_ar_sharpe_lambda.png

plot(lambdav, sharper, type="b", 
     xlab="Lambda", ylab="Sharpe Ratio",
     main="Out-of-sample Sharpe Ratios vs Lambda Parameter")




