#################################
### FRE6871 Test #1 Solution Monday February 9
#################################
# Max score 70pts

# The below solutions are examples.
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the indices of the TRUE elements
# of a Boolean vector, similar to the integer vector
# produced by the function which().
# Implement functions which replicate function which(),
# using several different methods.
# First using a for() loop, second using an lapply()
# loop, and third using vectorized operations.

# 1. (20pts)
# First method:
# Create a function called which_for(), that produces
# the same result as function which(), when applied to
# Boolean vectors.
# You must perform a for() loop.
# Hint: When performing the for() loop, you can first
# allocate an integer vector, and then perform a
# for() loop to populate it with the index values.
# You can use the functions integer(), seq_along(), and c().

# Any of the solutions below are correct and will receive
# full credit.

# First method with memory allocation:
which_for <- function(vecv) {
  indeks <- integer(sum(vecv))
  j <- 1
  for (i in seq_along(vecv)) {
    if (vecv[i]) {
      indeks[j] <- i
      j <- j + 1
    }  # end if
  }  # end for
  indeks
}  # end which_for

# Second method without memory allocation:
which_for2 <- function(vecv) {
  indeks <- integer()
  for (i in seq_along(vecv)) {
    if (vecv[i])
      indeks <- c(indeks, i)
  }  # end for
  indeks
}  # end which_for2

# Third method without memory allocation, and
# is more complicated:
which_for3 <- function(vecv) {
  indeks <- integer()
  j <- 1
  for (i in seq_along(vecv)) {
    if (vecv[i]) {
      indeks[j] <- i
      j <- j + 1
    }  # end if
  }  # end for
  indeks
}  # end which_for3

# Verify that which_for() applied to vecv produces
# the same result as function which().
# Use the function all.equal().

vecv <- sample(c(TRUE, FALSE), size=10, replace=TRUE)

all.equal(
  which(vecv),
  which_for(vecv),
  which_for2(vecv),
  which_for3(vecv)
)

# 2. (20pts)
# Second method:
# Create a function called which_apply(), that produces
# the same result as function which(), when applied to
# Boolean vectors.
# You must perform an lapply() loop.
# You can use the functions lapply(), seq_along(),
# and unlist().

which_apply <- function(vecv) {
  unlist(lapply(seq_along(vecv), function(x) {
    if (vecv[x])
      x
    else
      NULL
  }))}  # end which_apply

# Verify that which_apply() applied to vecv produces
# the same result as function which().
# Use the function all.equal().

vecv <- sample(c(TRUE, FALSE), size=10, replace=TRUE)

all.equal(
  which(vecv),
  which_apply(vecv)
)


# 3. (20pts)
# Third method:
# Create a function called which_vec(), that produces
# the same result as function which(), when applied to
# Boolean vectors.
# You cannot perform any type of loop, only vectorized functions.
# Hint: You can use the functions NROW() or seq_along(),
# and then perform vector subsetting.

which_vec <- function(vecv) (seq_along(vecv))[vecv]
# or
which_vec <- function(vecv) (1:NROW(vecv))[vecv]

# Verify that which_vec() applied to vecv produces
# the same result as function which().
# Use the function all.equal().

vecv <- sample(c(TRUE, FALSE), size=10, replace=TRUE)

all.equal(
  which(vecv),
  which_vec(vecv)
)


# 4. (10pts)
# Benchmark the speed of which_for(), which_apply(),
# and which_vec(), versus which(), using the function microbenchmark().

library(microbenchmark)
summary(microbenchmark(
  which_for=which_for(vecv == 18),
  which_for2=which_for2(vecv == 18),
  which_for3=which_for3(vecv == 18),
  which_apply=which_apply(vecv == 18),
  which_vec=which_vec(vecv == 18),
  which_r=which(vecv == 18),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# You should get output similar to this (only
# the ratios of the times matter):
#          expr     mean median
# 1   which_for  3744.35   3422
# 2  which_for2  2727.80   2445
# 3  which_for3  2635.02   2444
# 4 which_apply 30497.25  29813
# 5   which_vec  1696.68   1467
# 6     which_r  2283.00   1467


