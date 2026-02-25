# ==============================================================================
# Worst Performing Stocks vs All Stocks (Index) - Out-of-Sample Comparison
# Based on FRE7241 Lecture 5 - Stock Portfolio Selection Out-of-Sample
# ==============================================================================
# Original strategy: select 10 BEST  performing stocks in-sample, test OOS
# This version:      select 10 WORST performing stocks in-sample, test OOS
#
# Key question: do worst in-sample stocks stay worst out-of-sample?
# ==============================================================================

library(rutils)
library(dygraphs)

# ---- 1. Load data (same as Lecture 5) ----
# Use VTI dates as the date range
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
nrows <- NROW(retvti)

# Load full S&P500 stock prices from local data file
# Build path relative to this script's location (works regardless of working directory)
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
load(file = file.path(script_dir, "..", "Data", "sp500_prices.RData"))

# Select stock prices aligned to VTI date range
pricestock <- pricestock[datev]

# Keep only stocks with zero NA values (no gaps)
numna <- sapply(pricestock, function(x) sum(is.na(x)))
pricestock <- pricestock[, numna == 0]

# Drop penny stocks (last price > $1)
pricel <- last(pricestock)
pricel <- drop(coredata(pricel))
pricestock <- pricestock[, pricel > 1]

# Calculate dollar and percentage stock returns
retd <- rutils::diffit(pricestock)
retp <- retd / rutils::lagit(pricestock)
retp[1, ] <- 0
nstocks <- NCOL(retp)

cat("Stock universe:", nstocks, "stocks\n")
cat("Date range:", format(min(datev)), "to", format(max(datev)),
    "|", nrows, "trading days\n")

# ---- 2. Index: fixed-share equal-weight portfolio (all stocks) ----
# rowMeans(cumprod(1 + retp)) = equal number of shares in every stock
wealthfs <- rowMeans(cumprod(1 + retp))

# ---- 3. Define in-sample / out-of-sample split (50 / 50) ----
cutoff    <- nrows %/% 2
insample  <- 1:cutoff
outsample <- (cutoff + 1):nrows
cat("Cutoff date:", format(datev[cutoff]),
    "| In-sample:", cutoff, "days | OOS:", nrows - cutoff, "days\n")

# ---- 4. Identify 10 WORST performing stocks in-sample ----
pricev <- cumprod(1 + retp)           # cumulative price relatives
pricet <- pricev[cutoff, ]            # snapshot at cutoff
pricet <- drop(coredata(pricet))
pricet <- sort(pricet, decreasing = FALSE)   # ascending: worst first
symbolw <- names(head(pricet, 10))           # 10 worst in-sample stocks

cat("\n10 WORST performing stocks in-sample (cum. return at cutoff):\n")
print(round(head(pricet, 10), 3))

# ---- 5. Wealth of worst-stock portfolio (fixed shares, same as lecture) ----
# In-sample: equal-weight of those 10 stocks
wealthw_is <- rowMeans(pricev[insample, symbolw])

# Out-of-sample: carry forward terminal in-sample value, grow OOS
wealthw_os <- wealthw_is[cutoff] *
              rowMeans(cumprod(1 + retp[outsample, symbolw]))

# Splice in-sample and out-of-sample together
wealthw <- c(wealthw_is, wealthw_os)

# ---- 6. Combine into xts of log-wealth (same as lecture) ----
wealthv <- cbind(wealthfs, wealthw)
wealthv <- xts::xts(log(wealthv), order.by = datev)
colnames(wealthv) <- c("All stocks", "Worst performing")

# ---- 7. Sharpe & Sortino ratios ----
cat("\n---- In-sample Sharpe / Sortino ----\n")
print(round(
  sqrt(252) * sapply(rutils::diffit(wealthv[insample, ]),
    function(x) c(Sharpe  = mean(x) / sd(x),
                  Sortino = mean(x) / sd(x[x < 0]))),
  4))

cat("\n---- Out-of-sample Sharpe / Sortino ----\n")
print(round(
  sqrt(252) * sapply(rutils::diffit(wealthv[outsample, ]),
    function(x) c(Sharpe  = mean(x) / sd(x),
                  Sortino = mean(x) / sd(x[x < 0]))),
  4))

# ---- 8. Plot: Out-of-Sample Log Prices (same style as lecture image) ----
endw <- rutils::calc_endpoints(wealthv, interval = "weeks")

dygraphs::dygraph(wealthv[endw],
  main = "Out-of-Sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors = c("blue", "red"), strokeWidth = 2) %>%
  dyEvent(datev[cutoff], label = "cutoff",
          strokePattern = "solid", color = "green") %>%
  dyLegend(show = "always", width = 300)
