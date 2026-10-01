# Portfolio Dashboard - Tanya Romero
# Run with: shiny::runApp("portfolio_dashboard_app.R")

library(shiny)
library(ggplot2)
library(quantmod)

default_symbol <- "JNJ"

ui <- fluidPage(
  titlePanel("Portfolio Dashboard - Tanya Romero"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", value = default_symbol),
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      selectInput(
        "chart_type",
        "Chart Type:",
        choices = c("Line", "Candlestick", "Area")
      ),
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD")
      )
    ),
    mainPanel(plotOutput("stock_chart"))
  )
)

server <- function(input, output, session) {
  stock_data <- reactive({
    getSymbols(
      input$symbol,
      src = "yahoo",
      from = input$date_range[1],
      to = input$date_range[2],
      auto.assign = FALSE
    )
  })

  output$stock_chart <- renderPlot({
    data_xts <- stock_data()
    df <- data.frame(
      Date = as.Date(index(data_xts)),
      Close = as.numeric(Cl(data_xts)),
      Open = as.numeric(Op(data_xts)),
      High = as.numeric(Hi(data_xts)),
      Low = as.numeric(Lo(data_xts))
    )

    p <- ggplot(df, aes(x = Date, y = Close))
    if (input$chart_type == "Line") {
      p <- p + geom_line(color = "steelblue")
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(fill = "steelblue", alpha = 0.4)
    } else {
      p <- p + geom_line(color = "black")
    }

    if ("Moving Averages" %in% input$technical_indicators) {
      df$MA20 <- TTR::SMA(df$Close, 20)
      df$MA50 <- TTR::SMA(df$Close, 50)
      p <- p +
        geom_line(data = df, aes(y = MA20), color = "orange") +
        geom_line(data = df, aes(y = MA50), color = "purple")
      df$Signal <- ifelse(df$MA20 > df$MA50, "Buy", ifelse(df$MA20 < df$MA50, "Sell", "Hold"))
      idx <- which(df$Signal != "Hold")
      if (length(idx) > 0) {
        p <- p + geom_point(data = df[idx, ], aes(color = Signal), size = 2)
      }
    }

    p + labs(
      title = paste(input$symbol, "Price Chart"),
      x = "Date",
      y = "Close Price"
    ) + theme_minimal()
  })
}

shinyApp(ui, server)
