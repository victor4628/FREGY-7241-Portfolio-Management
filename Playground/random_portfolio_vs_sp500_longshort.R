# ==============================================================================
# Random Portfolio vs S&P500: Long-Short Strategy & Sharpe Ratio
# Based on FRE7241 Lecture 5 - Random Sub-Portfolio Comparison
# ==============================================================================
# Strategy:
#   Long  leg : Equal-weight randomly selected 100 stocks (from full universe)
#   Short leg : SPY (S&P500 ETF proxy)
#   PnL       : long_return - short_return
# ==============================================================================

library(quantmod)
library(xts)
library(dygraphs)
library(PerformanceAnalytics)

# ---- 1. Download SPY (S&P500 benchmark) ----
getSymbols("SPY", from = "2010-01-01", to = Sys.Date(), auto.assign = TRUE)
retspy <- na.omit(dailyReturn(Cl(SPY)))
colnames(retspy) <- "SPY"
cat("SPY data: from", format(start(retspy)), "to", format(end(retspy)),
    "->", NROW(retspy), "rows\n")

# ---- 2. Define the full stock universe (~150 S&P500 stocks across all sectors) ----
symbolv <- c(
  # Technology
  "AAPL", "MSFT", "NVDA", "AVGO", "ORCL", "AMD",  "INTC", "QCOM", "TXN",
  "AMAT", "ADI",  "KLAC", "LRCX", "MU",   "CSCO", "IBM",  "HPQ",  "DELL",
  "ACN",  "INTU", "ADBE", "CRM",  "NOW",  "SNOW", "PANW",
  # Communication Services
  "META", "GOOGL","NFLX", "DIS",  "CMCSA","T",    "VZ",   "TMUS", "CHTR",
  # Consumer Discretionary
  "AMZN", "TSLA", "HD",   "MCD",  "NKE",  "SBUX", "TJX",  "LOW",  "TGT",
  "BKNG", "MAR",  "HLT",  "F",    "GM",   "ORLY", "AZO",
  # Consumer Staples
  "PG",   "KO",   "PEP",  "WMT",  "COST", "CL",   "GIS",  "MO",   "PM",
  "KHC",  "HSY",  "CHD",
  # Financials
  "JPM",  "BAC",  "WFC",  "GS",   "MS",   "C",    "BLK",  "AXP",  "COF",
  "USB",  "PNC",  "TFC",  "SCHW", "CB",   "MET",  "PRU",  "AFL",  "ALL",
  "BK",   "STT",  "NTRS",
  # Healthcare
  "JNJ",  "PFE",  "MRK",  "ABT",  "UNH",  "CVS",  "ABBV", "AMGN", "GILD",
  "BMY",  "LLY",  "MDT",  "BSX",  "SYK",  "ISRG", "REGN", "VRTX", "ZBH",
  # Energy
  "XOM",  "CVX",  "COP",  "EOG",  "SLB",  "HAL",  "VLO",  "MPC",  "PSX",
  "OXY",  "DVN",  "KMI",
  # Industrials
  "HON",  "GE",   "BA",   "CAT",  "DE",   "MMM",  "UPS",  "FDX",  "LMT",
  "RTX",  "NOC",  "GD",   "ITW",  "EMR",  "ETN",  "PH",   "ROK",  "CTAS",
  # Materials
  "LIN",  "APD",  "SHW",  "ECL",  "PPG",  "NEM",  "FCX",  "NUE",  "VMC",
  # Utilities
  "NEE",  "DUK",  "SO",   "D",    "EXC",  "AEP",  "XEL",  "SRE",
  # Real Estate
  "AMT",  "PLD",  "CCI",  "EQIX", "PSA",  "EQR",  "SPG"
)
cat("Full universe size:", length(symbolv), "stocks\n")

# ---- 3. Download stock data ----
stockenv <- new.env()
# Suppress individual ticker messages
invisible(suppressWarnings(
  getSymbols(symbolv, env = stockenv,
             from = "2010-01-01", to = Sys.Date(),
             auto.assign = TRUE)
))
cat("Successfully downloaded:", length(ls(stockenv)), "stocks\n")

# Extract closing prices
pricev <- do.call(cbind, eapply(stockenv, Cl))
colnames(pricev) <- gsub("\\.Close$", "", colnames(pricev))

# Drop stocks with more than 10% missing values
pct_na <- colMeans(is.na(pricev))
pricev  <- pricev[, pct_na <= 0.10]
cat("Stocks passing NA filter:", NCOL(pricev), "\n")

# Forward-fill remaining NAs, then drop rows still missing
pricev <- zoo::na.locf(pricev, na.rm = FALSE)
pricev <- zoo::na.locf(pricev, fromLast = TRUE)
pricev <- na.omit(pricev)

# Calculate daily log returns
retp <- diff(log(pricev))
retp[1, ] <- 0
nstocks <- NCOL(retp)
cat("Final stock universe used:", nstocks, "stocks\n")

# ---- 4. Align dates with SPY ----
datev  <- as.Date(intersect(as.character(index(retp)),
                            as.character(index(retspy))))
retp   <- retp[datev]
retspy <- retspy[datev]
nrows  <- NROW(retp)
cat("Aligned date range:", format(min(datev)), "to", format(max(datev)),
    "->", nrows, "trading days\n")

# ---- 5. Randomly select 100 stocks from the full universe ----
set.seed(1121, "Mersenne-Twister", sample.kind = "Rejection")
nselect <- 100
samplev <- sample.int(n = nstocks, size = nselect, replace = FALSE)
cat("\nRandomly selected", nselect, "stocks (seed=1121):\n")
cat(paste(colnames(retp)[samplev], collapse = ", "), "\n")

# Equal-weight random portfolio daily returns
retrand <- xts(rowMeans(retp[, samplev]), order.by = datev)
colnames(retrand) <- "RandomPortfolio"

# ---- 6. Long-Short Strategy ----
# Long: equal-weight random 100-stock portfolio
# Short: SPY (S&P500)
# retls = random_portfolio_return - SPY_return
retls_raw <- retrand - retspy
# Scale long-short PnL to SPY's volatility (level playing field)
retls <- retls_raw * as.numeric(sd(retspy)) / as.numeric(sd(retls_raw))
colnames(retls) <- "LongShort"

# ---- 7. Performance Statistics ----
wealthall <- cbind(retspy, retrand, retls)
colnames(wealthall) <- c("SPY", "RandomPortfolio", "LongShort")

perf_stats <- sqrt(252) * sapply(wealthall, function(x)
  c(Sharpe  = mean(x) / sd(x),
    Sortino = mean(x) / sd(x[x < 0])))

cat("\n==== Annualised Performance Statistics ====\n")
print(round(perf_stats, 4))

# Maximum drawdown
dd_stats <- sapply(wealthall, function(x) {
  cum_ret <- cumsum(x)
  round(min(cum_ret - cummax(cum_ret)), 4)
})
cat("\nMaximum Drawdown (cumulative log returns):\n")
print(dd_stats)

# Correlation matrix
cat("\nCorrelation matrix:\n")
print(round(cor(wealthall), 3))

# ---- 8. Visualise Cumulative Returns ----
endw <- xts::endpoints(wealthall, on = "weeks")
colorv <- c("blue", "red", "green")

dygraphs::dygraph(cumsum(wealthall)[endw],
  main = "Random 100-Stock Portfolio vs S&P500: Long-Short Strategy") %>%
  dyOptions(colors = colorv, strokeWidth = 1) %>%
  dySeries(name = "LongShort",       strokeWidth = 2) %>%
  dySeries(name = "SPY",             strokeWidth = 2) %>%
  dySeries(name = "RandomPortfolio", strokeWidth = 2) %>%
  dyLegend(show = "always", width = 450)

# ---- 9. Multi-seed simulation: Sharpe ratio distribution across 200 draws ----
# Each draw randomly picks 100 stocks from the full universe
cat("\n==== Simulating 200 random portfolios (100 stocks each) ====\n")

sharpe_sim <- sapply(1:200, function(s) {
  set.seed(s)
  sv     <- sample.int(n = nstocks, size = nselect, replace = FALSE)
  ret_r  <- rowMeans(retp[, sv])
  ret_ls <- ret_r - as.numeric(retspy)
  ret_ls <- ret_ls * as.numeric(sd(retspy)) / sd(ret_ls)
  sqrt(252) * mean(ret_ls) / sd(ret_ls)
})

cat("Long-Short Sharpe ratio summary across 200 random draws:\n")
print(round(summary(sharpe_sim), 3))

# Plot distribution of Sharpe ratios
hist(sharpe_sim, breaks = 25, col = "lightblue",
     main = "Distribution of Long-Short Sharpe Ratios\n(200 Random 100-Stock Portfolios vs SPY)",
     xlab = "Annualised Sharpe Ratio", ylab = "Count")
abline(v = median(sharpe_sim), lwd = 2, col = "blue",  lty = 2)
abline(v = mean(sharpe_sim),   lwd = 2, col = "red",   lty = 1)
abline(v = 0,                  lwd = 1, col = "black", lty = 3)
legend("topright", inset = 0.02, bty = "n",
       legend = c(paste("Median =", round(median(sharpe_sim), 3)),
                  paste("Mean   =", round(mean(sharpe_sim),   3))),
       col = c("blue", "red"), lwd = 2, lty = c(2, 1))
