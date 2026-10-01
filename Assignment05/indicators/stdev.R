stdev <- function(data) {
  mean_value <- mean(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values^2
  variance <- mean(squared_diff)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
