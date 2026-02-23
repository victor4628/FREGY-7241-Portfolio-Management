#################################
### FRE6871 Test #1 Monday February 9
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and upload it to Brightspace.


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

### Write your code here
which_for <- function(boolv) {
  indx <- integer(0)
  for (i in seq_along(boolv)) {
    if (boolv[i])
      indx <- c(indx, i)
  }
  indx
}

# Verify that which_for() applied to vecv produces
# the same result as function which().
# Use the function all.equal().

vecv <- sample(c(TRUE, FALSE), size=10, replace=TRUE)

### Write your code here
all.equal(which_for(vecv), which(vecv))


# 2. (20pts)
# Second method:
# Create a function called which_apply(), that produces
# the same result as function which(), when applied to
# Boolean vectors.
# You must perform an lapply() loop.
# You can use the functions lapply(), seq_along(),
# and unlist().

### Write your code here
which_apply <- function(boolv) {
  indx <- lapply(seq_along(boolv), function(i) {
    if (boolv[i]) i
  })
  unlist(indx)
}

# Verify that which_apply() applied to vecv produces
# the same result as function which().
# Use the function all.equal().

### Write your code here
all.equal(which_apply(vecv), which(vecv))


# 3. (20pts)
# Third method:
# Create a function called which_vec(), that produces
# the same result as function which(), when applied to
# Boolean vectors.
# You cannot perform any type of loop, only vectorized functions.
# Hint: You can use the functions NROW() or seq_along(),
# and then perform vector subsetting.

### Write your code here
which_vec <- function(boolv) {
  seq_along(boolv)[boolv]
}

# Verify that which_vec() applied to vecv produces
# the same result as function which().
# Use the function all.equal().

### Write your code here
all.equal(which_vec(vecv), which(vecv))



# 4. (10pts)
# Benchmark the speed of which_for(), which_apply(),
# and which_vec(), versus which(), using the function 
# microbenchmark().

library(microbenchmark)

### Write your code here
vecv <- sample(c(TRUE, FALSE), size=1000, replace=TRUE)

microbenchmark(
  which_for = which_for(vecv),
  which_apply = which_apply(vecv),
  which_vec = which_vec(vecv),
  which_r = which(vecv),
  times = 100
)


# You should get output similar to this (only
# the ratios of the times matter):
#          expr     mean median
# 1   which_for  3744.35   3422
# 2  which_for2  2727.80   2445
# 3  which_for3  2635.02   2444
# 4 which_apply 30497.25  29813
# 5   which_vec  1696.68   1467
# 6     which_r  2283.00   1467
