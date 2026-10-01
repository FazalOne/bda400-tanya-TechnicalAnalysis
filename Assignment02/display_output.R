source("load_stock_data.R")
source("calculate_statistics.R")

portfolio <- load_stock_data("portfolio.txt")
for (symbol in names(portfolio)) {
  cat("\n===", symbol, "===\n")
  print(head(portfolio[[symbol]]))
  print(calculate_statistics(portfolio[[symbol]]))
}
