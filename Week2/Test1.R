# Load the file SPY_minutes_markets.RData with intraday 1-minute prices for SPY
SPY <- ohlc  # Adjust according to actual data structure
# Calculate returns from the Close prices
close_prices <- SPY$SPY.Close
returns <- diff(log(close_prices))
returns <- returns[-1]  # Remove first NA

# Calculate cumulative returns
cumulative_returns <- cumsum(returns)

# Calculate running maximum (peak)
running_max <- cummax(cumulative_returns)

# Calculate drawdown at each point
drawdown <- cumulative_returns - running_max

# Find endpoints for largest drawdowns
# Get the index of maximum drawdown
max_dd_index <- which.min(drawdown)
max_drawdown_value <- as.numeric(drawdown[max_dd_index])

# Find the peak before the maximum drawdown
peak_before_trough <- which.max(cumulative_returns[1:max_dd_index])

# Calculate drawdown statistics
cat("Maximum Drawdown:", max_drawdown_value, "\n")
cat("Peak Time:", index(cumulative_returns)[peak_before_trough], "\n")
cat("Trough Time:", index(cumulative_returns)[max_dd_index], "\n")
cat("Drawdown Duration:", max_dd_index - peak_before_trough, "minutes\n")
