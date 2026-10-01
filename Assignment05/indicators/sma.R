# Simple Moving Average
sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  sma_values <- numeric(length(data) - period + 1)
  for (i in seq_len(length(data) - period + 1)) {
    sma_values[i] <- mean(data[i:(i + period - 1)])
  }
  return(sma_values)
}
