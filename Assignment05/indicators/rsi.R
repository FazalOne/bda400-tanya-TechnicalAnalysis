rsi <- function(data, period) {
  if (length(data) <= period) {
    stop("data length must be greater than period")
  }
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  avg_gain <- mean(gains[1:period], na.rm = TRUE)
  avg_loss <- mean(losses[1:period], na.rm = TRUE)
  rsi_values <- rep(NA_real_, length(data))
  rs <- if (is.na(avg_loss) || avg_loss == 0) Inf else avg_gain / avg_loss
  rsi_values[period + 1] <- if (is.infinite(rs)) 100 else 100 - (100 / (1 + rs))
  for (i in (period + 2):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    rs <- if (is.na(avg_loss) || avg_loss == 0) Inf else avg_gain / avg_loss
    rsi_values[i] <- if (is.infinite(rs)) 100 else 100 - (100 / (1 + rs))
  }
  return(rsi_values)
}
