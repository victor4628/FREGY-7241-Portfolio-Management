#################################
### FRE6871 Homework #2 Solution due 6PM Monday February 9
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the standard error of the importance
# sampling estimate of the crossing probability of
# Brownian motion using bootstrap simulation.

## Run the setup code below:
# This is just the lecture code from the slide:
#   Importance Sampling of Brownian Motion

library(rutils)
# Define Brownian motion parameters
sigmav <- 1.0  ##  Volatility
drift <- 0.0  ##  Drift
nsteps <- 100  ##  Number of simulation steps
npaths <- 1000  ##  Number of simulation paths
# Calculate matrix of normal variables
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
datav <- rnorm(npaths*nsteps, mean=drift, sd=sigmav)
datav <- matrix(datav, nc=npaths)
# Simulate paths of Brownian motion
pathm <- matrixStats::colCumsums(datav)
# Tilt the datav
lambdaf <- 0.1  ##  Tilt parameter
datat <- datav + lambdaf  ##  Tilt the random numbers
patht <- matrixStats::colCumsums(datat)
# Calculate path weights
weightv <- exp(-lambdaf*colSums(datat) + nsteps*lambdaf^2/2)
# Calculate option payout using naive MC
strikep <- 10  ##  Strike price
payouts <- (pathm[nsteps, ] - strikep)
sum(payouts[payouts > 0])/npaths
# Calculate option payout using importance sampling
payouts <- (patht[nsteps, ] - strikep)
sum((weightv*payouts)[payouts > 0])/npaths
# Calculate crossing probability using naive MC
barl <- 20
crossi <- (colSums(pathm > barl) > 0)
sum(crossi)/npaths
# Calculate crossing probability using importance sampling
crossi <- colSums(patht > barl) > 0
sum(weightv*crossi)/npaths

## End of setup code.


# 1. (20pts)
# Calculate the probability of the paths crossing the
# level barl but then dropping and at maturity ending
# up below the barl.

crossi <- (colSums(pathm > barl) > 0) & (pathm[nsteps, ] < barl)
sum(crossi)/npaths

# You should get the output:
# [1] 0.021

# Calculate the same probability but using the tilted 
# paths patht.

crossi <- (colSums(patht > barl) > 0) & (patht[nsteps, ] < barl)
sum(weightv*crossi)/npaths

# You should get the output:
# [1] 0.01960079


# 2. (20pts)
# Calculate the standard errors of the crossing
# probabilities from p.1 above, using parallel bootstrap
# simulation.
# Calculate the standard errors for both naive (standard)
# Monte Carlo simulation and for importance sampling.
#
# Perform a loop over 1:nboots, and inside the loop
# calculate the crossing probabilities by repeating the
# calculations from p.1.
# You must resample from the matrices pathm, patht, and
# weightv, without generating new paths.
#
# You cannot use the function rnorm() inside the loop.
# You can use the functions sd(), mean(), detectCores(),
# makeCluster(), clusterSetRNGStream(), clusterExport(),
# sample.int(), and either parSapply() or mclapply().

# Write only the parallel code for your computer system.

# Define the simulation parameters:
# Number of bootstrap simulations:
nboots <- 1e3

library(parallel)  # Load the package parallel

ncores <- detectCores() - 1  # Number of cores

# Perform bootstrap for Windows
cluster <- makeCluster(ncores)  # initialize compute cluster
# Export variables to CPU cores
clusterExport(cluster, list("pathm", "patht", "weightv", "nsteps", "npaths", "barl"))
clusterSetRNGStream(cluster, 1121)
bootd <- parSapply(cluster, 1:nboots, function(it) {
  samplev <- sample.int(npaths, replace=TRUE)
  pathss <- pathm[, samplev]
  crossi <- (colSums(pathss > barl) > 0) & (pathss[nsteps, ] < barl)
  naivemc <- sum(crossi)/npaths
  pathss <- patht[, samplev]
  weightss <- weightv[samplev]
  crossi <- (colSums(pathss > barl) > 0) & (pathss[nsteps, ] < barl)
  importmc <- sum(weightss*crossi)/npaths
  c(naivemc=naivemc, importance=importmc)
})  # end parSapply

# Stop R processes over Windows cluster
stopCluster(cluster)

# Perform bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboots, FUN=function(it) {
  samplev <- sample.int(npaths, replace=TRUE)
  pathss <- pathm[, samplev]
  crossi <- (colSums(pathss > barl) > 0) & (pathss[nsteps, ] < barl)
  naivemc <- sum(crossi)/npaths
  pathss <- patht[, samplev]
  weightss <- weightv[samplev]
  crossi <- (colSums(pathss > barl) > 0) & (pathss[nsteps, ] < barl)
  importmc <- sum(weightss*crossi)/npaths
  c(naivemc=naivemc, importmc=importmc)
})  # end mclapply

bootd <- do.call(cbind, bootd)

# You should get the output:
dim(bootd)
# [1]   2 1000

# Perform an apply() loop over bootd, and calculate
# the means and standard errors of the naive and importance
# estimates of the crossing probability.

apply(bootd, MARGIN=1, function(x) c(mean=mean(x), stderr=sd(x)))

# You should get output similar to this:
#          naivemc     importmc
# mean   0.021197000 0.019630205
# stderr 0.004447243 0.002442845

# The above shows that the standard error of the importance
# estimate is about 55% that of the naive MC estimate.
# The standard error of the importance estimate could be
# reduced further by a better choice of the tilt parameter.



############## Part II
# Summary: Calculate the standard errors of VaR and
# CVaR under the Vasicek model, for different values 
# of correlation, using parallel bootstrap simulation.

## Run the setup code below.

# Define the model parameters
nbonds <- 300
nsimu <- 1000
lgd <- 0.4
# Vector of correlation values
rhov <- seq(0.05, 0.25, 0.05)
# Confidence level
confl <- 0.95

# Calculate the default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)

## End of setup code.

# 1. (30pts)
# Create a function called calc_var() which simulates
# losses under the Vasicek model, using a vector of
# correlations, and calculates a vector of VaR and
# CVaR values.
#
# The function calc_var() should be similar to the one
# in the lecture notes, but instead of performing a loop
# over levels, it should loop over the rhov argument.
#
# The function calc_var() should accept the following
# arguments:
#  threshv - vector of default thresholds,
#  lgd - loss given default,
#  rhov - vector of asset correlations,
#  nsimu - number of simulations,
#  confl - a confidence level.
#
# The function calc_var() should return a vector that
# is double the length of rhov, with its first elements
# equal to the VaR values, and the last elements equal
# to the CVaR values.
#
# The function calc_var() should pre-calculate the vector
# of sysv random numbers and the matrix of idiosyncratic
# random numbers outside the sapply() loop over
# the rhov vector.
# The function calc_var() should not reset the random
# number generator.

calc_var <- function(threshv,
                     lgd=0.6,
                     rhov=seq(0.1, 0.2, 0.02),
                     nsimu=1000,
                     confl=0.95) {
  # Define the model parameters
  nbonds <- NROW(threshv)
  # Calculate the sysv and idiosyncratic random numbers
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  
  cvarv <- sapply(rhov, function(rho) {
    # Define correlation parameters
    rhos <- sqrt(rho)
    rhosm <- sqrt(1-rho)
    # Simulate asset values and losses under Vasicek model
    assetm <- t(rhos*sysv + t(rhosm*assets))
    lossv <- lgd*colSums(assetm < threshv)/nbonds
    # Calculate the VaR and CVaR
    varisk <- quantile(lossv, probs=confl)
    cvarisk <- mean(lossv[lossv >= varisk])
    c(var= varisk, cvar=cvarisk)
  })  # end sapply
  
  cvarv <- c(cvarv[1, ], cvarv[2, ])
  sufs <- seq_along(rhov)
  names(cvarv) <- c(paste0("var", sufs), paste0("cvar", sufs))
  cvarv
}  # end calc_var

# Run calc_var() as follows:

set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
calc_var(threshv,
         lgd=lgd,
         rhov=rhov,
         nsimu=nsimu,
         confl=confl)

# You should get the following output:
#       var1       var2       var3       var4       var5
# 0.07073333 0.08400000 0.09733333 0.10933333 0.11866667
#      cvar1      cvar2      cvar3      cvar4      cvar5
# 0.07960000 0.09613559 0.11282051 0.12862745 0.14175163


# 2. (30pts)
# Calculate the standard errors of VaR and
# CVaR using parallel bootstrap simulation.
# Perform bootstrap by calling calc_var() in a loop
# and collecting the results in a list called bootd.

# You should use the function calc_var() from p.1

# Write only the parallel code for your computer system.

# Define the number of bootstrap simulations.
library(parallel)  # Load the package parallel
nboot <- 500
# Get number of cores.
ncores <- detectCores() - 1

# Windows version

# Create a compute cluster and initialize it.
# Reset the random number generator in all the cores.
# If you run Windows, then you can use the functions
# 
# You can use the functions makeCluster(), parLapply(),
# and clusterSetRNGStream(), and stopCluster().

cluster <- makeCluster(ncores)
clusterSetRNGStream(cluster, 1121)

bootd <- parLapply(cluster, rep(lgd, nboot),
                   fun=calc_var,
                   threshv=threshv,
                   rhov=rhov,
                   nsimu=nsimu,
                   confl=confl)  # end parLapply

# Stop R processes over cluster
stopCluster(cluster)


# Mac-OSX version

bootd <- mclapply(rep(lgd, nboot),
                  FUN=calc_var,
                  threshv=threshv,
                  rhov=rhov,
                  nsimu=nsimu,
                  confl=confl)  # end mclapply

# Bind the bootstrap list into a matrix.
# You can use the functions rutils::do_call() and rbind().

bootd <- rutils::do_call(rbind, bootd)

# You should get output similar to the following:
round(head(bootd), 3)
#       var1  var2  var3  var4  var5 cvar1 cvar2 cvar3 cvar4 cvar5
# [1,] 0.069 0.079 0.091 0.099 0.107 0.078 0.094 0.110 0.126 0.139
# [2,] 0.071 0.081 0.095 0.103 0.112 0.079 0.096 0.113 0.128 0.143
# [3,] 0.069 0.083 0.095 0.108 0.120 0.078 0.098 0.118 0.133 0.148
# [4,] 0.067 0.079 0.089 0.099 0.111 0.079 0.098 0.113 0.131 0.146
# [5,] 0.069 0.081 0.091 0.100 0.107 0.078 0.096 0.112 0.126 0.140
# [6,] 0.071 0.083 0.096 0.105 0.116 0.082 0.101 0.119 0.134 0.148


# Calculate the vectors of means and standard errors
# of VaR and CVaR from the bootd data.
# You can use the functions apply(), mean(), and sd().

varsd <- apply(bootd[, seq_along(rhov)], MARGIN=2,
               function(x) c(mean=mean(x), stderror=sd(x)))

cvarsd <- apply(bootd[, 5+seq_along(rhov)], MARGIN=2,
                function(x) c(mean=mean(x), stderror=sd(x)))

# You should get output similar to the following:
round(varsd, 3)
#            var1  var2  var3  var4  var5
# mean      0.070 0.083 0.094 0.104 0.114
# stderror 0.002 0.002 0.003 0.004 0.005

# You should get the following output:
round(cvarsd, 3)
#           cvar1 cvar2 cvar3 cvar4 cvar5
# mean      0.080 0.099 0.115 0.130 0.145
# stderror 0.002 0.003 0.004 0.005 0.006

# Scale the standard errors of VaR and CVaR,
# by dividing them by their means.

varsd[2, ] <- varsd[2, ]/varsd[1, ]
cvarsd[2, ] <- cvarsd[2, ]/cvarsd[1, ]


# Plot the scaled standard errors of VaR and CVaR.
# You should use the functions plot(), lines(), and legend().

# Your plot should be similar to vasicek_stderror_corr.png
# The plot shows that VaR and CVaR increase with higher
# correlation, because the distribution of losses becomes wider.

plot(x=rhov, y=cvarsd[2, ], t="l", col="red", lwd=2,
     ylim=range(c(varsd[2, ], cvarsd[2, ])),
     xlab="Correlation", ylab="CVaR",
     main="Scaled Standard Errors of VaR and CVaR
     as a Function of Correlation")
# Add VaR
lines(x=rhov, y=varsd[2, ], lwd=2)
# Add legend with title.
legend(x="topleft", legend=c("CVaR", "VaR"), bty="n",
       title=NULL, inset=0.05, cex=0.8, bg="white",
       lwd=6, lty=1, col=c("red", "black"))




