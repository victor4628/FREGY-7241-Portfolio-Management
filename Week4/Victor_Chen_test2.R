#################################
### FRE7241 Test #2 Tuesday, February 10
#################################
# Max score 80pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and upload the file to Brightspace

############## Part I
# Summary: Calculate the intraday seasonality of
# volatility and trading volumes in high frequency data.
# Intraday seasonality means how the trading volumes and
# volatility depend on the time of day.
# Regress the trading volumes versus the volatility,
# and create plots.

## Run the setup code below

# Load package HighFreq
library(HighFreq)

# The package HighFreq contains an xts time series of
# one minute bars of OHLC prices and trading volumes
# for the SPY ETF, called HighFreq::SPY.
# The SPY column "SPY.Volume" contains the trading
# volumes.

tail(HighFreq::SPY)

#                     SPY.Open SPY.High SPY.Low SPY.Close SPY.Volume
# 2014-05-19 15:56:00  188.885  188.885 188.855   188.865     321321
# 2014-05-19 15:57:00  188.865  188.865 188.835   188.855     299474
# 2014-05-19 15:58:00  188.855  188.855 188.835   188.855     261357
# 2014-05-19 15:59:00  188.855  188.885 188.855   188.885     435886
# 2014-05-19 16:00:00  188.885  188.885 188.745   188.745    1824185
# 2014-05-19 16:01:00  188.745  188.745 188.745   188.745      63920

## End of setup code


# 1. (20pts)
# Subset the SPY data to the year "2011" and call it
# ohlc.  This will reduce the time of the calculations.

### write your code here
ohlc <- HighFreq::SPY["2011"]


# You should get the output:
dim(ohlc)
# [1] 98507   5


# Extract the POSIXct date-time index of ohlc.
# Next, format the date-time index into a vector 
# of strings representing hours and minutes.
# 
# For example, the first date-time index is:
index(first(ohlc))
# [1] "2011-01-03 09:31:00 EST"
# You can format it into a string representing 
# hours and minutes:
# [1] "09:31"
# Using the functions zoo::index() and format().

### write your code here
timev <- format(zoo::index(ohlc), "%H:%M")


# You should get the output:
NROW(timev)
# [1] 98507
head(timev)
# [1] "09:31" "09:32" "09:33" "09:34" "09:35" "09:36"
tail(timev)
# [1] "15:56" "15:57" "15:58" "15:59" "16:00" "16:01"


# Calculate a vector of the unique elements of timev.
# You can use the function unique().

### write your code here
timu <- unique(timev)


# You should get the output:
NROW(timu)
# [1] 391
head(timu)
# [1] "09:31" "09:32" "09:33" "09:34" "09:35" "09:36"
tail(timu)
# [1] "15:56" "15:57" "15:58" "15:59" "16:00" "16:01"

# Create a vector of POSIXct date-times by adding the 
# current date to timu.
# You can use the functions paste0(), Sys.Date(), 
# and as.POSIXct() with tz="America/New_York".

### write your code here
datev <- as.POSIXct(paste0(Sys.Date(), " ", timu), tz="America/New_York")


# You should get the outputs:
class(datev)
# [1] "POSIXct" "POSIXt" 
NROW(datev)
# [1] 391
head(datev)
# [1] "2026-02-10 09:31:00 EST" "2026-02-10 09:32:00 EST" "2026-02-10 09:33:00 EST"
# [4] "2026-02-10 09:34:00 EST" "2026-02-10 09:35:00 EST" "2026-02-10 09:36:00 EST"
tail(datev)
# [1] "2026-02-10 15:56:00 EST" "2026-02-10 15:57:00 EST" "2026-02-10 15:58:00 EST"
# [4] "2026-02-10 15:59:00 EST" "2026-02-10 16:00:00 EST" "2026-02-10 16:01:00 EST"

# The day of datev may be different, but the time 
# should be the same as above.


# 2. (20pts)
# Calculate the xts time series of trading volumes
# and volatilities for each minute bar of ohlc.
# Use the difference between the High price minus
# the Low price as a proxy for volatility.
# You can use the functions quantmod::Hi(), 
# quantmod::Lo(), quantmod::Vo(), and colnames().

### write your code here
volumv <- quantmod::Vo(ohlc)
colnames(volumv) <- "volume"
volv <- quantmod::Hi(ohlc) - quantmod::Lo(ohlc)
colnames(volv) <- "volat"


# You should get the output:
dim(volumv); dim(volv)
# [1] 98507     1
# [1] 98507     1
tail(volumv, 3)
#                         volume
# 2011-12-30 15:59:00    1419117
# 2011-12-30 16:00:00    3880568
# 2011-12-30 16:01:00     203540
tail(volv, 3)
#                     volat
# 2011-12-30 15:59:00 0.080
# 2011-12-30 16:00:00 0.125
# 2011-12-30 16:01:00 0.000


# Calculate the average trading volumes at "09:40"
# for all the different days.
# Hint: timu[10] is equal to "09:40".
# You can use the function mean() and 
# the "==" logical operator.

### write your code here
mean(volumv[timev == "09:40"])


# You should get the output:
# [1] 791416.1


# Calculate the average volatilities and trading 
# volumes for each minute bar (each value of timu), 
# by aggregating the volatilities and volumes for 
# the same minute in all the days.
# You can use the functions sapply(), mean(), and t().

### write your code here
volav <- t(sapply(timu, function(ti) {
  c(volume=mean(volumv[timev == ti]), volat=mean(volv[timev == ti]))
}))


# You should get the output:
dim(volav)
# [1] 391   2
head(volav)
#         volume     volat
# 09:31 1689061.9 0.1237897
# 09:32  945105.1 0.1127183
# 09:33  870628.0 0.1037897
# 09:34  800578.1 0.1026190
# 09:35  821750.5 0.1034921
# 09:36  949212.7 0.1131746
tail(volav)
#         volume      volat
# 15:56 1299969.1 0.08863095
# 15:57 1277908.9 0.07561508
# 15:58 1322715.8 0.07045635
# 15:59 1700209.4 0.07111111
# 16:00 2857035.8 0.09335317
# 16:01  175761.5 0.00000000

# The first column of volav contains the average 
# trading volumes, and the second column contains 
# the average volatilities.


# Coerce volav into an xts time series using datev 
# and the function xts::xts().

### write your code here
volav <- xts::xts(volav, order.by=datev)


# You should get the outputs:
class(volav)
# [1] "xts" "zoo"
dim(volav)
# [1] 391   2
head(volav)
#                       volume     volat
# 2026-02-10 09:31:00 1689061.9 0.1237897
# 2026-02-10 09:32:00  945105.1 0.1127183
# 2026-02-10 09:33:00  870628.0 0.1037897
# 2026-02-10 09:34:00  800578.1 0.1026190
# 2026-02-10 09:35:00  821750.5 0.1034921
# 2026-02-10 09:36:00  949212.7 0.1131746
tail(volav)
#                       volume     volat
# 2026-02-10 15:56:00 1299969.1 0.08863095
# 2026-02-10 15:57:00 1277908.9 0.07561508
# 2026-02-10 15:58:00 1322715.8 0.07045635
# 2026-02-10 15:59:00 1700209.4 0.07111111
# 2026-02-10 16:00:00 2857035.8 0.09335317
# 2026-02-10 16:01:00  175761.5 0.00000000


# Plot a dygraph plot of volumv and volag with two y axes.
# Your plot should be similar to volume_volat_daily.png

### write your code here
library(dygraphs)
dygraphs::dygraph(volav, main="Intraday Seasonality of Volume and Volatility") %>%
  dySeries("volume", axis="y") %>%
  dySeries("volat", axis="y2") %>%
  dyAxis("y", label="Volume") %>%
  dyAxis("y2", label="Volatility")

# The plot shows that the volatility and trading volumes 
# are higher at the market open (09:30 to 10:00) and the
# market close (15:30 to 16:00), and lower during the day.


# 3. (20pts)
# Create a formula from the column names of volav.
# You can use the functions colnames(), as.formula(),
# and paste().

### write your code here
formulav <- as.formula(paste(colnames(volav)[1], "~", colnames(volav)[2]))


# You should get the output:
formulav
# volume ~ volat

# Perform a regression of the volume column versus
# volat of volav.
# You can use the function lm().

### write your code here
regmod <- lm(formulav, data=volav)


# Extract from the regression summary, the regression
# statistics for the slope coefficient: t-value,
# p-value, and adj.r.squared.
# You can use the functions with() and summary().

### write your code here
regsumm <- summary(regmod)
with(regsumm, c(tval=coefficients[2, 3], pval=coefficients[2, 4], rsqadj=adj.r.squared))


# You should get output similar to this:
#        tval         pval      rsqadj
# 2.053862e+01 5.074768e-64 5.190142e-01

# Plot a scatterplot of volav.
# Add a regression line to the scatterplot.
# Add text with the t-value of the slope.
# You can use the functions plot(), abline(), and text().
# Hint: Pass the formulav and volav into plot().
# Pass the regression object into abline().
# Your plot should be similar to volume_volat_scatter2.png

### write your code here
plot(formulav, data=volav, main="Volume vs Volatility")
abline(regmod, col="blue", lwd=2)
text(x=volav[, "volat"][which.max(volav[, "volat"])],
     y=volav[, "volume"][which.max(volav[, "volume"])],
     labels=paste("t-value =", round(regsumm$coefficients[2, 3], 2)),
     pos=2)


# Perform the Durbin-Watson test for the autocorrelations
# of the regression residuals.
# Use the function dwtest() from package lmtest.

### write your code here
library(lmtest)
lmtest::dwtest(regmod)


# You should get output similar to this:
#     DW = 0.45102, p-value < 2.2e-16
# alternative hypothesis: true autocorrelation is greater than 0

# Is the null hypothesis of the Durbin-Watson test valid?
# Yes or No?

# Answer: The p-value of the Durbin-Watson test
# is very small so the null hypothesis of zero
# autocorrelations of the residuals can be rejected.
# The regression model fails the Durbin-Watson test.
# Most likely the variables aren't stationary, or the
# model is incomplete, and the level of volatility 
# doesn't explain the level of the trading volume.


# 4. (20pts)
# Calculate the row differences of volav.
# Use the function rutils::diffit().

### write your code here
voldiff <- rutils::diffit(volav)


# You should get the output:
dim(voldiff)
# [1] 391   2
tail(voldiff, 3)
#                         volume        volat
# 2026-02-10 15:59:00   377493.6  0.0006547619
# 2026-02-10 16:00:00  1156826.4  0.0222420635
# 2026-02-10 16:01:00 -2681274.4 -0.0933531746

# Repeat the regression analysis from p.3 above for
# voldiff, including the Durbin-Watson test.

### write your code here
regmod_diff <- lm(formulav, data=voldiff)


# Extract from the regression summary, the regression
# statistics for the slope coefficient: t-value,
# p-value, and adj.r.squared.
# You can use the functions with() and summary().

### write your code here
regsumm_diff <- summary(regmod_diff)
with(regsumm_diff, c(tval=coefficients[2, 3], pval=coefficients[2, 4], rsqadj=adj.r.squared))


# You should get output similar to this:
#       tval         pval       rsqadj
# 2.503051e+01 4.514586e-83 6.159628e-01


# Plot a scatterplot of voldiff.
# Add a regression line to the scatterplot.
# Add text with the t-value of the slope.
# You can use the functions plot(), abline(), and text().
# Hint: Pass the formulav and volav into plot().
# Pass the regression object into abline().
# Your plot should be similar to volume_volat_scatter3.png

### write your code here
plot(formulav, data=voldiff, main="Volume vs Volatility (Differences)")
abline(regmod_diff, col="blue", lwd=2)
text(x=voldiff[, "volat"][which.max(voldiff[, "volat"])],
     y=voldiff[, "volume"][which.max(voldiff[, "volume"])],
     labels=paste("t-value =", round(regsumm_diff$coefficients[2, 3], 2)),
     pos=2)


# Perform the Durbin-Watson test for the autocorrelations
# of regression residuals.
# Use the function dwtest() from package lmtest.

### write your code here
lmtest::dwtest(regmod_diff)


# You should get output similar to this:
#     DW = 2.2534, p-value = 0.9941
# alternative hypothesis: true autocorrelation is greater than 0

# Is the null hypothesis of the Durbin-Watson test valid?
# Yes or No?

# Answer: The p-value of the Durbin-Watson test is very 
# large so the null hypothesis of zero autocorrelations 
# of the residuals cannot be rejected.
# The regression model for the differences passes the 
# Durbin-Watson test, while the regression model for the 
# levels fails the test.



