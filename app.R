
source("global.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("TradeR"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 actionButton(inputId = "abRefresh", 
                              label = "Refresh"),
                 selectInput(inputId = "siCurrencyPair", 
                             label = "Currency pair", 
                             choices = c(), 
                             selected = NULL, 
                             multiple = FALSE),
                 selectInput(inputId = "siStrategy", 
                             label = "Trade strategy", 
                             choices = c("A", "B", "C"), 
                             selected = "A", 
                             multiple = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Overview", 
                 wellPanel(
                   plotlyOutput(outputId = "poOverview")
                 )
                 
        ),
        tabPanel(title = "Strategy",
                 uiOutput(outputId = "uioStrategy")
        )
      )
    )
  )
)


# one module per trade strategy


server <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$refresh <- Sys.time()
  rv$dfCurrencies <- NULL
  rv$dfPairs <- NULL
  rv$dfTradeLog <- NULL
  
  observeEvent(input$abRefresh, {
    rv$refresh <- Sys.time()
  })
  
  observeEvent(rv$refresh, handlerExpr = {
    withProgress({
      
      rv$dfCurrencies <- getCurrencyList(all = TRUE)
      
      dfPairs <- getPairList(all = TRUE)
      idxx <- match(x = dfPairs$currency_id_from, table = rv$dfCurrencies$currency_id)
      dfPairs$currency_iso_from <- rv$dfCurrencies$iso[idxx]
      idxx <- match(x = dfPairs$currency_id_to, table = rv$dfCurrencies$currency_id)
      dfPairs$currency_iso_to <- rv$dfCurrencies$iso[idxx]
      dfPairs$label <- sprintf("%s / %s", dfPairs$currency_iso_from, dfPairs$currency_iso_to)
      dfPairs <- dfPairs[order(dfPairs$label, decreasing = FALSE),]
      rownames(dfPairs) <- 1:nrow(dfPairs)
      rv$dfPairs <- dfPairs
      
    }, message = "Getting currencies list...")
    
  })
  
  observeEvent(rv$dfCurrencies, {
    chcs <- rv$dfPairs$pair_id
    names(chcs) <- rv$dfPairs$label
    updateSelectInput(session = session, inputId = "siCurrencyPair", choices = chcs, selected = 26)
  })
  
  observeEvent(input$siCurrencyPair, {
    if (length(input$siCurrencyPair) == 0) { return(NULL) }
    if (is.na(as.integer(input$siCurrencyPair))) { return(NULL) }
    if (nchar(input$siCurrencyPair) == 0) { return(NULL) }
    
    withProgress({
      dfTradeLog <- rbind(getTradeLog(pairId = input$siCurrencyPair, type = "buy", 
                                      dateFrom = Sys.time() - lubridate::days(7), 
                                      all = TRUE, convertDates = TRUE),
                          getTradeLog(pairId = input$siCurrencyPair, type = "sell", 
                                      dateFrom = Sys.time() - lubridate::days(7), 
                                      all = TRUE, convertDates = TRUE))
      dfTradeLog <- dfTradeLog[order(as.integer(dfTradeLog$trade_id), decreasing = TRUE),]
      rownames(dfTradeLog) <- 1:nrow(dfTradeLog)
      rv$dfTradeLog <- dfTradeLog
    }, message = "Getting trade logs...")
  })
  
  output$poOverview <- renderPlotly({
    if (length(rv$dfTradeLog) == 0) { return(NULL) } 
    
    dfTradeLog <- rv$dfTradeLog
    
    idxx <- which(dfTradeLog$type == "buy")
    fig <- plot_ly(data = dfTradeLog[idxx,], 
                   x = ~created, y = ~price, 
                   name = 'buy', 
                   type = 'scatter', 
                   mode = 'lines+markers') 
    idxx <- which(dfTradeLog$type == "sell")
    fig <- fig %>% add_trace(data = dfTradeLog[idxx,], 
                             y = ~price, 
                             name = 'sell', 
                             mode = 'lines+markers') 
    
    return(fig)
  })
  
  output$uioStrategy <- renderUI({
    modelBUI(id = "modStrat")
  })
  
  observeEvent(input$siStrategy, handlerExpr = {
    
    #stop("This now only happens once input$siStrategy gets selected AGAIN.")
    
    if (length(input$siCurrencyPair) == 0) { return(NULL) }
    if (is.na(as.integer(input$siCurrencyPair))) { return(NULL) }
    if (nchar(input$siCurrencyPair) == 0) { return(NULL) }
    
    callModule(module = modelB, id = "modStrat", pairId = input$siCurrencyPair)
  })
  
  rv$refresh <- Sys.time()
}

# Run the application 
shinyApp(ui = ui, server = server)
