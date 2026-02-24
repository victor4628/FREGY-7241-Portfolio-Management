#################################
### FRE6871 Test #2 Monday February 23
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below.
# Rename the file to your_name_test2.R
# and upload it to Brightspace.


############## Part I
# Summary: Remove penny stocks from a time series
# of prices.

## Run the setup code below

# Load the daily S&P500 stock prices.
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1abep6n9Jgx4IPhMKXbpYZHAySgLXq1SS
load(file=here::here("Data", "sp500_prices.RData"))

# Select prices from 2000 to the present.
pricestock <- pricestock["2000/"]

# The time series called pricestock contains penny stocks.
# A penny stock is a stock with a price less than $1.

# Some of these stocks have very low adjusted prices
# in the past because they had many stock splits as
# their prices grew rapidly over 20 years.

## End of setup code


# 1. (20pts)
# Find the names of the stocks with prices that
# were less than $1 at any time.
# You can use the functions lapply(), sum(),
# na.omit(), unlist(), and names().

### Write your code here
penny_list <- lapply(colnames(pricestock), function(col) {
  x <- na.omit(pricestock[, col])
  sum(x < 1) > 0
})
names(penny_list) <- colnames(pricestock)
namev <- names(which(unlist(penny_list)))


# You should get the outputs:
namev
#  [1] "RIG"   "EP"    "DNR"   "VTR"   "TPL"   "KMX"   "BIGGQ" "FOSL"  "F"     "ROL"  
# [11] "DVA"   "CTRA"  "RRC"   "MO"    "RRD"   "ORLY"  "ANDV"  "GILD"  "CTSH"  "DECK" 
# [21] "CCI"   "WMB"   "GME"   "ON"    "GLW"   "POM"   "SBNY"  "BTU"   "JCI"   "WYND" 
# [31] "ATVI"  "GNW"   "BLDR"  "AMZN"  "WRB"   "TSCO"  "HBI"   "TGNA"  "QEP"   "AES"  
# [41] "FITB"  "CPRT"  "FLIR"  "CMG"   "ENPH"  "LUMN"  "AIV"   "DXCM"  "PENN"  "ILMN" 
# [51] "SHLDQ" "MNST"  "AMT"   "NVDA"  "CSX"   "CTL"   "LVS"   "APH"   "RAI"   "SMCI" 
# [61] "ISRG"  "SWN"   "AKAM"  "AAPL"  "MSTR"  "AXON"  "HBAN"  "SBAC"  "ODFL"  "FRCB" 
# [71] "URBN"  "FAST"  "NFLX"  "LRCX" 


# 2. (30pts)
# Find the names of the stocks with the last (most
# recent) non-NA prices that are less than $1.
# There are at least two ways of doing it.
# You can use the functions lapply(), xts::last(),
# na.omit(), zoo::na.locf(), unlist(), names(),
# drop(), and coredata().

### Write your code here
last_price_list <- lapply(colnames(pricestock), function(col) {
  x <- na.omit(zoo::na.locf(pricestock[, col], na.rm = FALSE))
  if (length(x) == 0) return(NA)
  as.numeric(coredata(drop(xts::last(x))))
})
names(last_price_list) <- colnames(pricestock)
namev <- names(which(unlist(last_price_list) < 1))


# You should get the outputs:
namev
# [1] "DNR"   "BIGGQ" "POM"   "SHLDQ" "FRCB"


# 3. (20pts)
# Calculate the column numbers in pricestock,
# corresponding to the vector namev.
# You can use the functions match() and colnames().

### Write your code here
colnum <- match(namev, colnames(pricestock))


# You should get the output:
colnum
# [1] 18  61 223 458 671
colnames(pricestock)[colnum]
# [1] "DNR"   "BIGGQ" "POM"   "SHLDQ" "FRCB"

# Remove the stocks in namev from the time series
# called pricestock.

### Write your code here
pricestock <- pricestock[, -colnum]


# You should get the output:
dim(pricestock)
# [1] 6550  712

