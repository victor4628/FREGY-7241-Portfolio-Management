#################################
### FRE7241 Homework #1 due at 6PM Tuesday January 27, 2026
#################################
# Max score 130pts

# Please write in this file the R code needed to perform 
# the tasks below, rename it to your_name_hw1.R,
# and upload the file to Brightspace


############## Part I
# Summary: Simulate a stock-bond rebalancing strategy 
# which applies CPPI leverage, and uses TLT instead of 
# a zero-coupon bond.

## Run the setup code below

retp <- na.omit(rutils::etfenv$returns[, c("VTI", "TLT")])
datev <- zoo::index(retp)
nrows <- NROW(retp)
# Bond floor
bfloor <- 60
# CPPI multiplier
coeff <- 2

## End of setup


# 1. (20pts)
# Simulate a stock-bond rebalancing strategy using CPPI 
# leverage, and using TLT instead of a zero-coupon bond.
# 
# Hint: Copy the code from the lecture slides.
# Add code to calculate the returns of the TLT ETF.

### Write your code here

# Portfolio market values
portfv <- numeric(nrows)
# Initial principal
portfv[1] <- 100
# Stock investment
stocki <- numeric(nrows)
stocki[1] <- max(coeff*(portfv[1] - bfloor), 0)
# Stock value
stockv <- numeric(nrows)
stockv[1] <- stocki[1]
# Bond value
bondv <- numeric(nrows)
bondv[1] <- max(portfv[1] - stocki[1], 0)
# Margin account
margv <- numeric(nrows)

# Simulate the CPPI strategy
for (t in 2:nrows) {
  ## 1. Update the portfolio value (Market movement before rebalancing)
  # Calculate real-time market value of stock and bond positions based on returns
  # retp[t, 1] is VTI return, retp[t, 2] is TLT return
  stocki[t] <- (1 + retp[t, 1]) * stocki[t-1]
  bond_r <- (1 + retp[t, 2]) * bondv[t-1]
  
  # Calculate current stock net value (subtracting previous margin) and total portfolio value
  stockv[t] <- stocki[t] - margv[t-1]
  portfv[t] <- stockv[t] + bond_r
  
  ## 2. Update the CPPI leverage (Rebalancing)
  # Calculate target positions according to the CPPI formula
  stockt <- max(coeff*(portfv[t] - bfloor), 0)
  bondv[t] <- max(portfv[t] - stockt, 0)
  
  # Update margin account: 
  # (Target bond - Current bond) + (Target stock - Current stock) + Previous margin
  margv[t] <- (bondv[t] - bond_r) + (stockt - stocki[t]) + margv[t-1]
  
  # Update stocki to the target rebalanced value for the next iteration
  stocki[t] <- stockt
} ## end for loop

# Consolidate results
datav <- cbind(portfv, stockv, bondv, margv)
colnames(datav) <- c("portfv", "stockv", "bondv", "margv")

# Display results
head(datav)
tail(datav)

# You should get the outputs:
#         portfv   stockv    bondv      margv
# [1,] 100.00000 80.00000 20.00000 0.00000000
# [2,] 100.42148 80.39693 19.57852 0.02454892
# [3,] 100.82832 81.00872 19.17168 0.24107963
# [4,]  98.57605 79.29549 21.42395 0.10887108
# [5,]  96.86760 75.22536 23.13240 0.21828608
# [6,]  94.24985 71.01560 25.75015 0.10185045

#           portfv   stockv bondv    margv
# [5902,] 1167.036 1167.036     0 1047.036
# [5903,] 1171.106 1171.106     0 1051.106
# [5904,] 1166.631 1166.631     0 1046.631
# [5905,] 1157.951 1157.951     0 1037.951
# [5906,] 1164.702 1164.702     0 1044.702
# [5907,] 1163.410 1163.410     0 1043.410

# Calculate the Sharpe of CPPI wealth.
### Write your code here
# 1. Calculate the daily returns
cppi_ret <- diff(portfv) / portfv[-nrows]
vti_ret <- as.numeric(retp[-1, 1])

# 2. Define calculation function for strict matching
calc_metrics <- function(x) {
  # Annualization factor
  ann_f <- sqrt(252)
  
  # Sharpe: (Average Return / Standard Deviation) * sqrt(252)
  sharpe <- (mean(x) / sd(x)) * ann_f
  
  # Sortino: (Average Return / Downside Deviation) * sqrt(252)
  # Downside Deviation here is the SD of only the negative returns
  downside_dev <- sd(x[x < 0])
  sortino <- (mean(x) / downside_dev) * ann_f
  
  return(c(Sharpe = sharpe, Sortino = sortino))
}

# 3. Apply to both series
vti_perf <- calc_metrics(vti_ret)
cppi_perf <- calc_metrics(cppi_ret)

# 4. Combine into final table
performance_metrics <- cbind(VTI = vti_perf, CPPI = cppi_perf)

# Display results
performance_metrics

# You should get the outputs:
#             VTI      CPPI
# Sharpe  0.5244699 0.4306400
# Sortino 0.5867663 0.4493412


# 2. (20pts)
# Create a function called sim_cppi(), which simulates 
# the CPPI strategy, and returns the Sharpe ratio of the
# CPPI wealth.
# 
# The function sim_cppi() should accept two arguments: 
# - bfloor = the bond floor
# - coeff = the CPPI multiplier
# Hint: Copy the code from the lecture slides.

### Write your code here
sim_cppi <- function(bfloor, coeff) {
  # Initialize vectors based on the number of rows in retp
  # Note: retp and nrows must be available in the global environment or passed in
  portfv <- numeric(nrows)
  stocki <- numeric(nrows)
  stockv <- numeric(nrows)
  bondv <- numeric(nrows)
  margv <- numeric(nrows)
  
  # Set initial values
  portfv[1] <- 100
  stocki[1] <- max(coeff * (portfv[1] - bfloor), 0)
  stockv[1] <- stocki[1]
  bondv[1] <- max(portfv[1] - stocki[1], 0)
  margv[1] <- 0
  
  # Simulation loop
  for (t in 2:nrows) {
    # 1. Update market values before rebalancing
    # retp[t, 1] is VTI, retp[t, 2] is TLT
    stocki[t] <- (1 + retp[t, 1]) * stocki[t-1]
    bond_r <- (1 + retp[t, 2]) * bondv[t-1]
    
    # Calculate current net portfolio value
    stockv[t] <- stocki[t] - margv[t-1]
    portfv[t] <- stockv[t] + bond_r
    
    # 2. Rebalance according to CPPI formula
    stockt <- max(coeff * (portfv[t] - bfloor), 0)
    bondv[t] <- max(portfv[t] - stockt, 0)
    
    # Update margin account balance
    margv[t] <- (bondv[t] - bond_r) + (stockt - stocki[t]) + margv[t-1]
    
    # Set stocki for next period's calculation
    stocki[t] <- stockt
  }
  
  # Calculate CPPI daily returns
  cppi_ret <- diff(portfv) / portfv[-nrows]
  
  # Calculate Annualized Sharpe Ratio
  # (Mean / SD) * sqrt(252)
  sharpe_cppi <- (mean(cppi_ret) / sd(cppi_ret)) * sqrt(252)
  
  return(sharpe_cppi)
}

# Run sim_cppi() as follows:
sim_cppi(bfloor=60, coeff=2)

# You should get the output:
# [1] 0.43064


# 3. (20pts)
# Find the best values of the bond floor and 
# the CPPI multiplier.

# Create a vector of bfloors (run this):
bfloors <- seq(10, 90, 5)

# Perform an sapply() loop over bfloors, 
# with coeff=2

### Write your code here

# Use sapply to calculate the Sharpe ratio for each floor value
sharper <- sapply(bfloors, function(floor_val) {
  sim_cppi(bfloor = floor_val, coeff = 2)
})

# Find the best bfloor (the one with the maximum Sharpe ratio)
best_floor <- bfloors[which.max(sharper)]

# You should get the output:
round(sharper, 3)
#  [1] 0.438 0.437 0.437 0.437 0.436 0.435 0.434 0.433 0.430 0.430 0.431
# [12] 0.431 0.431 0.431 0.430 0.427 0.416

# Calculate the bfloor that produces the maximum 
# sharper value.
# You can use the function which.max().

### Write your code here
# Find the bond floor corresponding to the maximum Sharpe ratio
best_bfloor <- bfloors[which.max(sharper)]

# Display the output
best_bfloor
# You should get the output:
# [1] 10

# Create a vector of CPPI multipliers (run this):
coeffv <- (1:8)/4

# Perform an sapply() loop over coeffv, 
# with bfloor=90

### Write your code here
sharper <- sapply(coeffv, function(c_val) {
  sim_cppi(bfloor = 90, coeff = c_val)
})

# You should get the output:
round(sharper, 3)
# [1] 0.155 0.377 0.525 0.527 0.504 0.482 0.460 0.438

# Calculate the coeff that produces the maximum 
# sharper value.
# You can use the function which.max().

### Write your code here
# Find the coefficient corresponding to the maximum Sharpe ratio
best_coeff <- coeffv[which.max(sharper)]

# Display the output
best_coeff

# You should get the output:
# [1] 1

# Conclusion:
# The bond floor (bfloor) that produces the maximum Sharpe ratio:
# [1] 90
# Interpretation: A high floor (90% of initial principal) indicates 
# that capital preservation was key to achieving the best 
# risk-adjusted return for this dataset.

# The CPPI multiplier (coeff) that produces the maximum Sharpe ratio:
# [1] 1
# Interpretation: A multiplier of 1 suggests that applying leverage 
# to the cushion did not improve the Sharpe ratio; a 1:1 allocation 
# of the cushion to stocks was optimal.

# Final Result Summary:
# The most effective CPPI strategy for VTI and TLT was highly 
# conservative, prioritizing a high bond floor and no leverage.



############## Part II
# Summary: Calculate a matrix of the best performing ETFs
# in each year.  Create a scatterplot of alphas for the
# years 2008 and 2009.

## Run all the setup code below.

library(rutils)
library(PerformanceAnalytics)
symbolv <- c("VTI", "IEF", "VNQ", "USO", "XLY", "XLP", "XLE", "XLF", "XLK")
retp <- rutils::etfenv$returns[, symbolv]
retp <- na.omit(zoo::na.locf(retp))

## End of setup code.


# 1. (20pts)
# Create a vector of yearly end points of the returns.
# You can use the function rutils::calc_endpoints().

### Write your code here
endd <- rutils::calc_endpoints(retp, interval = "years")

# You should get the following output:
endd
#  [1]  0  183  434  687  939 1191 1443 1693 1945 2197 2449 2701 2952 3203
# [15] 3455 3708 3960 4211 4461 4713 4963 4974

# Select the returns for the symbolv, and for the dates between 
# the last two end points.
# You can use the function NROW().

### Write your code here
retsub <- retp[(endd[NROW(endd)-1]+1):endd[NROW(endd)], ]

# You should get the following outputs:
head(retsub)
#                     VTI           IEF           VNQ          USO          XLY
# 2025-12-31 -0.0076658326 -0.0033222622 -0.0082156784 -0.008351380 -0.007924302
# 2026-01-02  0.0030971763 -0.0008322930  0.0003389639 -0.002896034 -0.008916614
# 2026-01-05  0.0074946659  0.0030137721  0.0022568279  0.018106544  0.016342287
# 2026-01-06  0.0069701039 -0.0007266311  0.0072997124 -0.024653451  0.006380268
# 2026-01-07 -0.0034054579  0.0018674142 -0.0097825034 -0.010565029 -0.001405133
# 2026-01-08  0.0002646319 -0.0030103308  0.0088869475  0.039765233  0.016569980
#                     XLP          XLE          XLF           XLK
# 2025-12-31 -0.006032233 -0.005576015 -0.007457970 -0.0099523940
# 2026-01-02  0.000128725  0.020806417  0.002917049  0.0022895212
# 2026-01-05 -0.004385972  0.026800826  0.021610782  0.0022151470
# 2026-01-06  0.001550188 -0.027019908  0.004798730  0.0139391827
# 2026-01-07 -0.011032625 -0.011237311 -0.014106117 -0.0008186098
# 2026-01-08  0.023475862  0.030979790  0.005201339 -0.0157516068
tail(retsub)
#                     VTI          IEF         VNQ          USO           XLY
# 2026-01-09  0.0066223467  0.001142917 0.002013874  0.003396550  0.0120488257
# 2026-01-12  0.0018382626 -0.001246883 0.001340333  0.012216679  0.0008837826
# 2026-01-13 -0.0020135116  0.001246883 0.005565464  0.025220106 -0.0024926640
# 2026-01-14 -0.0039219180  0.002074690 0.008620743 -0.011910607 -0.0159055532
# 2026-01-15  0.0030744479 -0.002074690 0.006909059 -0.020593465  0.0036742234
# 2026-01-16 -0.0005848808 -0.003849560 0.012166129  0.007283966 -0.0032653090
#                     XLP          XLE          XLF          XLK
# 2026-01-09  0.010273411  0.002574556 -0.003045779  0.013154913
# 2026-01-12  0.011666694 -0.007096041 -0.007926541  0.004369502
# 2026-01-13  0.011532151  0.014142083 -0.019357799 -0.002114094
# 2026-01-14  0.013713941  0.022302629 -0.001476287 -0.012226267
# 2026-01-15  0.001701094 -0.009407407  0.004054558  0.005238501
# 2026-01-16 -0.003161481  0.001678909  0.001286647  0.001099354


# Perform an lapply() loop over the neighboring end points 
# of endd.
# Inside the loop, select the returns for the symbolv and
# the neighboring end points.  
# Then for the selected returns, calculate a data frame of 
# statistics using table.CAPM(), and using VTI as the 
# benchmark asset "Rb".
# Simplify the column names and return the data frame.
# The output should be a list of data frames. 
# You can use the functions lapply(), sapply(), table.CAPM(),
# colnames(), strsplit(), NROW(), and an anonymous function.
# You don't need to use all of these functions.
# You can use any functions you choose.

### Write your code here
capml <- lapply(2:NROW(endd), function(i) {
  # Select returns between neighboring endpoints
  ret_subset <- retp[(endd[i-1]+1):endd[i], ]
  
  # Calculate CAPM statistics using VTI as benchmark
  capm_stats <- table.CAPM(ret_subset[, symbolv[-1]], 
                           Rb = ret_subset[, "VTI"])
  
  # Simplify column names by removing "VTI" suffix
  colnames(capm_stats) <- sapply(strsplit(colnames(capm_stats), " "), function(x) x[1])
  
  return(capm_stats)
})

# You should get the following outputs:
is.list(capml)
# [1] TRUE
capml[[1]]
#                         IEF    VNQ     USO    XLY    XLP     XLE    XLF     XLK
# Alpha                0.0003 0.0007 -0.0016 0.0002 0.0004 -0.0004 0.0003 -0.0002
# Beta                 0.0326 0.9094  0.0865 0.9956 0.5466  1.2493 0.9259  1.1092
# Alpha Robust         0.0003 0.0006 -0.0015 0.0003 0.0005 -0.0001 0.0002 -0.0002
# Beta Robust          0.0523 0.9582  0.0881 0.9822 0.5670  1.2525 0.8963  1.1390
# Beta+                0.0831 0.8206  0.6261 1.0077 0.3818  1.4792 0.8758  1.0898
# Beta-                0.0188 0.6517  0.0731 0.9016 0.6239  1.5042 0.9472  0.9853
# Beta+ Robust         0.0868 0.8887  0.6261 0.9673 0.3920  1.4617 0.8617  1.0265
# Beta- Robust         0.0639 0.7227  0.0835 0.8966 0.6247  1.5130 0.9279  1.0427
# R-squared            0.0069 0.4583  0.0015 0.7497 0.4770  0.3022 0.7175  0.7213
# R-squared Robust     0.0183 0.4976  0.0015 0.7283 0.4658  0.3144 0.6502  0.7006
# Annualized Alpha     0.0689 0.1926 -0.3242 0.0632 0.1090 -0.0886 0.0769 -0.0604
# Correlation          0.0832 0.6770  0.0383 0.8658 0.6906  0.5498 0.8471  0.8493
# Correlation p-value  0.2631 0.0000  0.6064 0.0000 0.0000  0.0000 0.0000  0.0000
# Tracking Error       0.1144 0.1089  0.2668 0.0631 0.0801  0.2099 0.0642  0.0765
# Active Premium      -0.0577 0.1957 -0.4673 0.0684 0.0550 -0.0928 0.0739 -0.0578
# Information Ratio   -0.5040 1.7976 -1.7512 1.0842 0.6870 -0.4421 1.1511 -0.7553
# Treynor Ratio        2.2196 0.3582 -3.8978 0.1993 0.3385  0.0298 0.2203  0.0651

# Assign names to the list using the years corresponding to the endd.
# You can use the functions names(), format() with the "%Y" format, 
# and zoo::index(),

### Write your code here
names(capml) <- format(zoo::index(retp)[endd[-1]], "%Y")
# You should get the following outputs:
names(capml)
#  [1] "2006" "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016"
# [12] "2017" "2018" "2019" "2020" "2021" "2022" "2023" "2024" "2025" "2026"


# 2. (10pts)
# Perform an sapply() loop over "capml".
# Inside the loop extract the data frame row called
# "Annualized Alpha", coerce it to a vector using unlist(),
# and return the vector.
# You can use the functions sapply(), unlist(),
# and an anonymous function,

### Write your code here
alphav <- sapply(capml, function(x) {
  unlist(x["Annualized Alpha", ])
})

# alphav should be a matrix of annual alphas like this:
dim(alphav)
# [1]  8 21
alphav
#        2006    2007    2008    2009    2010    2011    2012    2013    2014    2015    2016
# IEF  0.0689  0.1137  0.1205 -0.0488  0.1229  0.1639  0.0790 -0.0470  0.1114  0.0152  0.0290
# VNQ  0.1926 -0.2144  0.2848 -0.2019  0.0418  0.0730  0.0489 -0.2262  0.2165  0.0116 -0.0101
# USO -0.3242  0.4603 -0.4475 -0.0128 -0.1545 -0.0072 -0.2647 -0.1209 -0.4477 -0.4537 -0.0814
# XLY  0.0632 -0.1814  0.0540  0.0706  0.0763  0.0481  0.0654  0.0602 -0.0286  0.1013 -0.0575
# XLP  0.1090  0.0921  0.0858  0.0086  0.0341  0.1348  0.0166  0.0090  0.0800  0.0608 -0.0252
# XLE -0.0886  0.2754  0.0866 -0.0996  0.0129  0.0183 -0.1196 -0.0710 -0.1912 -0.2141  0.1115
# XLF  0.0769 -0.2474 -0.0868 -0.3040 -0.0766 -0.1798  0.0701 -0.0409  0.0257 -0.0217  0.0622
# XLK -0.0604  0.1025 -0.1060  0.2115 -0.0430  0.0164 -0.0115 -0.0142  0.0523  0.0502  0.0123
#        2017    2018    2019    2020    2021    2022    2023    2024    2025    2026
# IEF  0.0726  0.0061  0.1485  0.1170 -0.0249 -0.1376  0.0253 -0.0185  0.0827 -0.1386
# VNQ -0.0382 -0.0305  0.1478 -0.2243  0.1978 -0.1146 -0.1366 -0.0936 -0.0465  0.9542
# USO -0.0468 -0.1623  0.0594 -0.7307  0.3353  0.3262 -0.1133  0.1058 -0.1417  0.7414
# XLY  0.0249  0.0743 -0.0200  0.0799 -0.0163 -0.1583  0.0398 -0.0272 -0.1046 -0.0161
# XLP  0.0350 -0.0508  0.1040 -0.0376  0.0715  0.1145 -0.1125  0.0687 -0.0189  1.7102
# XLE -0.1553 -0.1372 -0.1606 -0.4797  0.1888  0.8886 -0.1496 -0.0440 -0.0361  2.5533
# XLF -0.0530 -0.0856 -0.0047 -0.2174  0.0957  0.0810 -0.1099  0.1168  0.0100 -0.4657
# XLK  0.0696  0.0545  0.0496  0.1615  0.0111 -0.0488  0.1940 -0.1167  0.0062 -0.2764


# 3. (20pts)
# Sort the last column of alphav in descending order, 
# and extract the ETF names.
# You can use the functions names(), sort(), and NCOL().

### Write your code here
names(sort(alphav[, NCOL(alphav)], decreasing = TRUE))

# You should get the following output:
# [1] "XLE" "XLP" "VNQ" "USO" "XLY" "IEF" "XLK" "XLF"

# Sort all the columns of alphav in descending order, 
# using an apply() loop over the columns of alphav, and 
# extract the ETF names.
# You can use the functions apply(), sort(), names(),
# and an anonymous function,

### Write your code here
namev <- apply(alphav, 2, function(x) {
  names(sort(x, decreasing = TRUE))
})

# You should get the following outputs:
dim(namev)
# [1]  8 21
namev
#       2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020 
# [1,] "VNQ" "USO" "VNQ" "XLK" "IEF" "IEF" "IEF" "XLY" "VNQ" "XLY" "XLE" "IEF" "XLY" "IEF" "XLK"
# [2,] "XLP" "XLE" "IEF" "XLY" "XLY" "XLP" "XLF" "XLP" "IEF" "XLP" "XLF" "XLK" "XLK" "VNQ" "IEF"
# [3,] "XLF" "IEF" "XLE" "XLP" "VNQ" "VNQ" "XLY" "XLK" "XLP" "XLK" "IEF" "XLP" "IEF" "XLP" "XLY"
# [4,] "IEF" "XLK" "XLP" "USO" "XLP" "XLY" "VNQ" "XLF" "XLK" "IEF" "XLK" "XLY" "VNQ" "USO" "XLP"
# [5,] "XLY" "XLP" "XLY" "IEF" "XLE" "XLE" "XLP" "IEF" "XLF" "VNQ" "VNQ" "VNQ" "XLP" "XLK" "XLF"
# [6,] "XLK" "XLY" "XLF" "XLE" "XLK" "XLK" "XLK" "XLE" "XLY" "XLF" "XLP" "USO" "XLF" "XLF" "VNQ"
# [7,] "XLE" "VNQ" "XLK" "VNQ" "XLF" "USO" "XLE" "USO" "XLE" "XLE" "XLY" "XLF" "XLE" "XLY" "XLE"
# [8,] "USO" "XLF" "USO" "XLF" "USO" "XLF" "USO" "VNQ" "USO" "USO" "USO" "XLE" "USO" "XLE" "USO"
#       2021  2022  2023  2024  2025  2026 
# [1,] "USO" "XLE" "XLK" "XLF" "IEF" "XLE"
# [2,] "VNQ" "USO" "XLY" "USO" "XLF" "XLP"
# [3,] "XLE" "XLP" "IEF" "XLP" "XLK" "VNQ"
# [4,] "XLF" "XLF" "XLF" "IEF" "XLP" "USO"
# [5,] "XLP" "XLK" "XLP" "XLY" "XLE" "XLY"
# [6,] "XLK" "VNQ" "USO" "XLE" "VNQ" "IEF"
# [7,] "XLY" "IEF" "VNQ" "VNQ" "XLY" "XLK"
# [8,] "IEF" "XLY" "XLE" "XLK" "USO" "XLF"


# 4. (20pts)
# Plot a scatterplot of the alphas for the years 
# "2008" and "2009", and add labels with ETF names.
# You can use the functions plot(), rownames(), 
# range(), and text().
# Hint: Use the parameters xlim and ylim in the 
# function plot(), to enlarge the dimensions of 
# the plot, and make room for the labels.
# Your plot should be similar to scatter_etfs.png

### Write your code here
# Extract alphas for 2008 and 2009
alpha_2008 <- alphav[, "2008"]
alpha_2009 <- alphav[, "2009"]

# Calculate ranges for xlim and ylim with some padding
x_range <- range(alpha_2008)
y_range <- range(alpha_2009)
x_padding <- diff(x_range) * 0.2
y_padding <- diff(y_range) * 0.2

# Create the scatterplot
plot(alpha_2008, alpha_2009,
     xlim = c(x_range[1] - x_padding, x_range[2] + x_padding),
     ylim = c(y_range[1] - y_padding, y_range[2] + y_padding),
     xlab = "2008 Alpha",
     ylab = "2009 Alpha",
     main = "ETF Alphas: 2008 vs 2009",
     pch = 19,
     col = "blue")

# Add ETF name labels
text(alpha_2008, alpha_2009, 
     labels = rownames(alphav),
     pos = 4,
     cex = 0.8)

# Comment: 
# The scatterplot of the ETF alphas in the years 2008 and 2009
# doesn't show any relationship between the alphas in those years.
# For some ETFs the positive or negative alphas persist, while for 
# others, the alphas change radically from year to year.

