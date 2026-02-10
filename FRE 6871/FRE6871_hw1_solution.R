#################################
### FRE6871 Homework #1 Solution due at 6PM Monday February 2
#################################
# Max score 150pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Perform for() and sapply() loops over a vector,
# and then perform the equivalent vectorized operations
# over the vector, and measure the increase in speed.

# First create a vector of random numbers as follows:

set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vecv <- sample(1:10)

# You should get the following output:
vecv
# [1]  1  6  9  5  4  3  8  2 10  7


# 1. (20pts) 
# Perform a for() loop to replace those elements 
# of vecv that are greater than "5" with the 
# number "5".
# You can use the functions for() and seq_along(),

for (i in seq_along(vecv)) {
  if (vecv[i] > 5)  {vecv[i] <- 5}
}  # end for

# You should get the following output:
vecv
# [1] 1 5 5 5 4 3 5 2 5 5


# 2. (20pts) 
# Perform exactly the same calculations as in p.1. using 
# an sapply() loop.
# You must use either functions apply() lapply(), or sapply().
# You can also use the functions NROW() and seq_along(),
# and an anonymous function.
# This is an example of sapply() with an anonymous function:

sapply(vecv, function(x) (x^2))

# Run this code first
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vecv <- sample(1:10)

vecv <- sapply(vecv, function(x) {
  if (x > 5) 5 else x
})  # end sapply
# or
vecv <- sapply(seq_along(vecv), function(i) {
  if (vecv[i] > 5) {vecv[i] <- 5} else vecv[i]
})  # end sapply


# 3. (20pts) 
# Perform the same calculations as in p.1, but only 
# using vectorized operations (logical operators and 
# subsetting).
# You cannot use any for() or apply() loops.

# Run this code first
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vecv <- sample(1:10)

vecv[vecv > 5] <- 5


# 4. (20pts) 
# Perform the same calculations as in p.1, but using 
# the function ifelse().
# You cannot use any for() or apply() loops.
# You must use the function ifelse().

# Run this code first
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vecv <- sample(1:10)

vecv <- ifelse(vecv > 5, 5, vecv)


# 5. (10pts) 
# Benchmark the CPU time used by the code from p.2 with 
# the code from p.3, using the function microbenchmark().
# Assign the names "sapply" and "vectorized" to each method.

library(microbenchmark)
summary(microbenchmark(
  sapply=sapply(vecv, function(x) {
    if (x > 5) 5 else x
  }),  # end sapply
  vectorized=(vecv[vecv>5] <- 5),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# You should get output similar to this:
#          expr    mean median
# 1     sapply 500114.9  39099
# 2 vectorized   3910.4   2933



############## Part II
# Summary: Calculate the optimal tilt parameter for 
# the importance sampling of CVaR.

## Run the setup code below.

nsimu <- 1e3 # Number of simulation steps
nboot <- 100 # Number of bootstrap simulations
varisk <- -2 # VaR quantile
lambdaf <- -1.5 # Tilt parameter

# Calculate a matrix of random data
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
datav <- matrix(rnorm(nboot*nsimu), ncol=nboot)

## End of setup code.


# 1. (20pts)
# Create a function called boot_cvar(), which
# performs bootstrap simulation, and calculates
# the standard error of CVaR, using importance 
# sampling.
# 
# The function boot_cvar() should accept two 
# arguments:
#   lambdaf = tilt parameter,
#   varisk = VaR value.
# The CVaR value corresponds to the VaR value.
# 
# Hint: adapt the code from the slides:
#  Calculating CVaR Using Importance Sampling
#  The Optimal Tilt Parameter for Importance Sampling

boot_cvar <- function(lambdaf, varisk) {
  datat <- datav + lambdaf  ##  Tilt the random numbers
  weightv <- exp(-lambdaf*datat + lambdaf^2/2) # Weights
  # Bootstrap of CVaR
  cvarv <- sapply(1:NCOL(datav), function(coln) {
    datac <- datat[, coln] # Column of data
    weightc <- weightv[, coln] # Column of weights
    weightm <- (datac <= varisk)*weightc # Truncated weights
    sum(datac*weightm)/sum(weightm) # CVaR estimate
  }) # end sapply
  return(sd(cvarv))
}  # end boot_cvar

# Call boot_cvar() to check that it works correctly.
# You should get output similar to this:
boot_cvar(lambdaf, varisk)
# [1] 0.01615007

# Define a vector of tilt parameters:
lambdav <- seq(-3.0, -1.2, by=0.2)


# Perform an sapply() loop over the vector lambdav 
# and call the function boot_cvar().

stderr <- sapply(lambdav, boot_cvar, varisk=varisk)

# You should get output similar to this:
stderr
# [1] 0.01804857 0.01675801 0.01603641 0.01449753 0.01463759 0.01542356 0.01541681
# [8] 0.01621652 0.01719773 0.02536473

# Plot the lambdav and the standard deviation 
# of CVaR stderr using plot().

plot(lambdav, stderr, type="b", 
     xlab="Tilt parameter", ylab="Standard error",
     main="Standard Error of CVaR")

# Your plot should be similar to boot_tilt_cvar.png

# The plot shows that the optimal tilt parameter
# which minimizes the standard deviation of CVaR
# is close to the varisk value.

# Calculate the optimal tilt parameter which
# minimizes the standard deviation of CVaR.
# You can use the function which.min().

# You should get output similar to this:
lambdav[which.min(stderr)]
# [1] -2.4

# The optimal tilt parameter is close to the 
# varisk value.


# 2. (20pts)
# Create a function called boot_cvar2(), which
# performs the same bootstrap simulation as 
# boot_cvar(), but only using vectorized operations, 
# without using for() or sapply() loops.
# Hint: use the function colSums().

boot_cvar2 <- function(lambdaf, varisk) {
  datat <- datav + lambdaf  ##  Tilt the random numbers
  weightv <- exp(-lambdaf*datat + lambdaf^2/2) # Weights
  # Truncate the weights
  weightv <- (datat <= varisk)*weightv
  # Calculate the CVaR values
  cvarv <- colSums(datat*weightv)/colSums(weightv)
  return(sd(cvarv))
}  # end boot_cvar2


# Call boot_cvar() and boot_cvar2() to check that 
# they give the same results.
# You should get the output:
all.equal(boot_cvar(lambdaf, varisk), 
          boot_cvar2(lambdaf, varisk))
# [1] TRUE


# Benchmark the CPU times used by boot_cvar() and 
# boot_cvar2(), using the function microbenchmark().

library(microbenchmark)
summary(microbenchmark(
  boot_cvar=boot_cvar(lambdaf, varisk),
  boot_cvar2=boot_cvar2(lambdaf, varisk),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# You should get output similar to the following:

#         expr     mean   median
# 1  boot_cvar 2.087663 1.802524
# 2 boot_cvar2 1.964523 1.255420

# This shows that boot_cvar2() is slightly faster
# than boot_cvar() because it avoids loops.


# 3. (20pts)
# Define a vector of VaR values:

varv <- seq(-2.7, -0.8, by=0.2)

# Calculate the optimal tilt parameters for the 
# vector of VaR values.
# Perform an sapply() loop over the vector varvv, 
# and for each VaR value, calculate the optimal 
# tilt parameter.
# You must use the function boot_cvar2().
# Hint: You should call boot_cvar2() inside a 
# second sapply() loop.

lambopt <- sapply(varv, function(varisk) {
  stderr <- sapply(lambdav, boot_cvar2, varisk=varisk)
  return(lambdav[which.min(stderr)])
}) # end sapply


# You should get output similar to this:
lambopt
# [1] -3.0 -2.2 -2.0 -1.8 -1.6 -1.4 -1.2 -1.2 -1.2 -1.4

# Plot the varv and lambopt using plot().

plot(varv, lambopt, type="b", 
     xlab="VaR", ylab="Tilt parameter",
     main="Optimal Tilt Parameter for CVaR")

# Your plot should be similar to boot_tilt_cvar_opt.png

# The plot shows that the optimal tilt parameter
# is more negative (more tilting is needed) with
# a more negative VaR value (higher confidence level).


