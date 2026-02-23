#################################
### FRE6871 Homework #3 due at 6PM Monday February 23
#################################
# Max score 80pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and upload the file to Brightspace


############## Part I
# Summary: Calibrate the Vasicek interest rate model on 
# historical data.
# Calibrate the Vasicek IR model parameters to a yield 
# curve.

## Run the setup code below

library(rutils)

# Use this code from the lecture slides.

# Maturities in years
tauv <- seq(1, 30, by=1)

# Set the Vasicek model parameters
ratet <- 2.5; thetav <- 1.0; sigmav <- 0.1; muv <- 4.5

# Calculate the yield curve under the Vasicek model.
# Formula for the yields of zero-coupon bonds with maturities tauv, 
# under the Vasicek model with parameters thetav, sigmav, and muv.
# Where muv is the long-term equilibrium level of the short rate.

B <- (1 - exp(-thetav*tauv))/thetav
A <- (muv - sigmav^2/(2*thetav^2))
A <- A*(B - tauv) - (sigmav^2*B^2)/(4*thetav)
ycurve <- (-A + B*ratet)/tauv

# Plot the yield curve
plot(tauv, ycurve, type="l", lwd=3, col="blue",
     main="Yield Curve Under the Vasicek Model",
     xlab="Maturity (years)", ylab="Yield (%)")

# Load constant maturity Treasury rates
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1abep6n9Jgx4IPhMKXbpYZHAySgLXq1SS
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")

# Set the short rate to the 1-year Treasury yields:
ratev <- ratesenv$DGS1 # Time series of 1-year Treasury yields
nrows <- NROW(ratev)
ratet <- as.numeric(last(ratev)) # Current short rate
# Calculate the lagged rate
retlag <- rutils::lagit(ratev)
# Calculate the rate increments
rated <- rutils::diffit(ratev)

## End of setup code


# 1. (30pts)
# Create a shiny app for the yield curve under the Vasicek model.
# Upload the app to Brightspace.
# Your plots should be similar to vasicek_rate_shiny1.jpeg, 
# and vasicek_rate_shiny2.jpeg


# 2. (10pts)
# Calculate the Vasicek interest rate model parameters
# from the time series of 1-year Treasury yields.
# You must two different methods to calculate the parameters.
# Hint: Copy the code from the lecture slides.

# First method: Use the function lm()

### Write your code here
formulav <- rated ~ retlag
regmod <- lm(formulav)
coeff <- summary(regmod)$coefficients
sigmav <- sd(regmod$residuals)
thetav <- -coeff[2, 1]
muv <- coeff[1, 1] / thetav


# You should get the following outputs:
c(sigma=round(sigmav, 4), theta=round(thetav, 4), muv=round(muv, 4))
#  sigma  theta  muv 
# 0.0789 0.0003 4.9549 

# Second method: Use only the functions cov() and var().
# Calculate regression alpha and beta directly
# using R code, without using the function lm().
datav <- na.omit(cbind(rated, retlag))
x1 <- as.numeric(datav[, 1])
x2 <- as.numeric(datav[, 2])
betac <- cov(x1, x2) / var(x2)
alphac <- mean(x1) - betac * mean(x2)
residv <- x1 - alphac - betac * x2
sigmav <- sd(residv)
thetav <- -betac
muv <- alphac / thetav

# You should get the following outputs:
c(sigma=round(sigmav, 4), theta=round(thetav, 4), muv=round(muv, 4))
#  sigma  theta  muv
# 0.0789 0.0003 4.9549


# 3. (10pts)
# Get the most recent yield curve, calculate the vector 
# of its maturities from its names, and sort it.
# You can use the function eapply(), do.call(), coredata(),
# drop(), substr(), as.numeric(), order(), and sort().

### Write your code here
ycnow <- eapply(ratesenv, xts::last)
ycnow <- do.call(cbind, ycnow)
tauv <- as.numeric(substr(colnames(ycnow), start=4, stop=11))
ordv <- order(tauv)
tauv <- tauv[ordv]
ycnow <- drop(coredata(ycnow[, ordv]))


# You should get the following outputs:
tauv
# [1]  1  2  5 10 20 30
ycnow
# DGS1  DGS2  DGS5 DGS10 DGS20 DGS30 
# 3.54  3.56  3.77  4.17  4.74  4.79 


# 4. (10pts)
# Create a function to calculate the yield curve of zero-coupon 
# bonds under the Vasicek model.
# The arguments of the function should be the current short 
# rate, the Vasicek parameters muv, thetav, sigmav, and and 
# the vector of maturities tauv.

### Write your code here
calc_yieldc <- function(ratet, muv, thetav, sigmav, tauv) {
  B <- (1 - exp(-thetav*tauv))/thetav
  A <- (muv - sigmav^2/(2*thetav^2))
  A <- A*(B - tauv) - (sigmav^2*B^2)/(4*thetav)
  ycurve <- (-A + B*ratet)/tauv
  return(ycurve)
}


# You should get the following output:
calc_yieldc(ratet, muv, thetav, sigmav, tauv)
# [1] 3.539159 3.536243 3.515056 3.438363 3.130377 2.617333


# 5. (20pts)
# Create an objective function equal to the sum 
# of squared differences between the market bond yields 
# minus the Vasicek yields.
# The arguments of the function should be the vector of
# parameters to be calibrated (muv, thetav, sigmav), 
# the short rate ratet, the vector of maturities tauv, 
# and the vector of market yields ycnow.

### Write your code here
objfun <- function(params, ratet, tauv, ycnow) {
  muv <- params[1]
  thetav <- params[2]
  sigmav <- params[3]
  ycurve <- calc_yieldc(ratet, muv, thetav, sigmav, tauv)
  sum((ycnow - ycurve)^2)
}


# You should get the following output:
objfun(c(muv, thetav, sigmav), ratet, tauv, ycnow)
# [1] 7.912223

# Calibrate the Vasicek IR model parameters to the 
# market bond yields. 
# Use the function DEoptim() with:
# control = DEoptim.control(trace=FALSE, storepopfrom = 1, itermax=500)

library(DEoptim)

### Write your code here
optiml <- DEoptim::DEoptim(fn=objfun,
  ratet=ratet,
  tauv=tauv,
  ycnow=ycnow,
  lower=c(0, 0, 0),
  upper=c(30, 10, 10),
  control=DEoptim.control(trace=FALSE, storepopfrom=1, itermax=500))
paroptim <- optiml$optim$bestmem
muv <- paroptim[1]
thetav <- paroptim[2]
sigmav <- paroptim[3]


# You should get outputs similar to:
c(muv=muv, theta=thetav, sigma=sigmav)
#       muv        theta        sigma 
# 6.060516e+00 5.887510e-02 1.913751e-09 

# You should get outputs similar to:
ycurve <- calc_yieldc(ratet, muv, thetav, sigmav, tauv)
all.equal(ycnow, ycurve, check.attributes=FALSE)
# [1] "Mean relative difference: 0.02300469"

# Plot the actual yield curve and the Vasicek DEoptim fit.
# You can use the functions matplot() and legend().
# Your plot should be similar to vasicek_rate_fit.png

### Write your code here
colorv <- c("blue", "red")
matplot(tauv, cbind(ycnow, ycurve),
  type="l", lty=1, lwd=3, col=colorv,
  main="Actual Yield Curve and Vasicek DEoptim Fit",
  xlab="Maturity (years)", ylab="Yield (%)")
legend("bottomright", legend=c("Actual", "Vasicek"),
  bty="n", col=colorv, lty=1, lwd=6, inset=0.05, cex=1.0)




