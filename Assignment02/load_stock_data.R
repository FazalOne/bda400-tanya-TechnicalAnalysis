# Load stock data from portfolio.txt using quantmod
load_stock_data <- function(portfolio_file = "portfolio.txt") {
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    install.packages("quantmod")
  }
  library(quantmod)
  symbols <- trimws(readLines(portfolio_file))
  symbols <- symbols[nzchar(symbols)]
  stock_list <- list()
  for (symbol in symbols) {
    stock_list[[symbol]] <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE,
      warnings = FALSE
    )
  }
  return(stock_list)
}
