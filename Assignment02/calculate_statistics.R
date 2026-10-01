calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

calculate_statistics <- function(stock_xts) {
  closes <- as.numeric(Cl(stock_xts))
  closes <- closes[!is.na(closes)]
  data.frame(
    mean = mean(closes),
    median = median(closes),
    mode = calc_mode(round(closes, 2)),
    stdev = sd(closes),
    sma_20 = mean(tail(closes, 20))
  )
}
