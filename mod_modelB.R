
modelBUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      
      fluidRow(
        column(width = 3,
               wellPanel(
                 textOutput(outputId = ns("toCurrentPrice")),
                 actionButton(inputId = ns("abRefresh"), 
                              label = "Refresh")
               )
        ),
        column(width = 3,
               wellPanel(
                 numericInput(inputId = ns("niProfitMargin"), 
                              label = "Targeted profit margin per unit", 
                              value = 0.1, 
                              min = 0, 
                              step = 0.01)
               )
        ),
        column(width = 3,
               wellPanel(
                 actionButton(inputId = ns("abEvaluate"), 
                              label = "Evaluate current price")
               )
        ),
        column(width = 3)
      ), 
      
      fluidRow(
        column(width = 2,
               wellPanel(
                 
                 helpText("Show volume"),
                 numericInput(inputId = ns("niPriceAtOrAbove"), 
                              label = "traded at or above", 
                              value = 3.30, 
                              min = 0, 
                              step = 0.01)
               )
        ),
        column(width = 5,
               wellPanel(
                 fluidRow(
                   helpText("Over the past [n days] and [n hours]")
                 ),
                 fluidRow(
                   column(width = 5,
                          numericInput(inputId = ns("niNdays"), 
                                       label = "n days", 
                                       value = 3, 
                                       min = 0, 
                                       max = 365, 
                                       step = 1)
                   ),
                   column(width = 2),
                   column(width = 5,
                          numericInput(inputId = ns("niNhours"), 
                                       label = "n hours", 
                                       value = 0, 
                                       min = 0, 
                                       max = 23, 
                                       step = 1)
                   )
                 )
               )
        ),
        
        
        column(width = 5,
               wellPanel(
                 fluidRow(
                   helpText("OR between [date since] and [date until]")
                 ),
                 fluidRow(
                   column(width = 5,
                          dateInput(inputId = ns("diSince"), 
                                    label = "date since", 
                                    value = Sys.time() - lubridate::days(1), 
                                    min = "1970-01-01", 
                                    max = Sys.Date(), 
                                    autoclose = TRUE)
                   ),
                   column(width = 2),
                   column(width = 5,
                          dateInput(inputId = ns("diUntil"), 
                                    label = "date until", 
                                    value = NULL, 
                                    min = "1970-01-01", 
                                    max = Sys.Date(), 
                                    autoclose = TRUE)
                   )
                 )
               )
        )
      ),
      
      fluidRow(
        wellPanel(
          textOutput(outputId = ns("toVolume"))
        )
      ),
      fluidRow(
        DT::dataTableOutput(outputId = ns("dtoTradeLog"))
      )
    )
  )
  
}

modelB <- function(input, output, session, pairId) {
  
  rv <- reactiveValues()
  rv$dateFrom <- NULL
  rv$dateTo <- NULL
  rv$dfTradeLog <- NULL
  rv$currentPrice <- NULL
  rv$refresh <- Sys.time()
  
  observeEvent(input$abRefresh, { 
    rv$refresh <- Sys.time()
  })
  
  observeEvent(rv$refresh, {
    
    withProgress({
      
      rv$currentPrice <- getCurrentPrice(pairId = pairId)[["ask_price"]]
      incProgress(0.1)
      
      dfBuy <- getTradeLog(pairId = pairId, type = "buy", all = TRUE, 
                           dateFrom = rv$dateFrom, 
                           dateTo = rv$dateTo, convertDates = FALSE)
      incProgress(0.3)
      
      dfSell <- getTradeLog(pairId = pairId, type = "sell", all = TRUE, 
                            dateFrom = rv$dateFrom, 
                            dateTo = rv$dateTo, convertDates = FALSE)
      incProgress(0.3)
      
      dfTradeLog <- rbind(dfBuy, dfSell)
      incProgress(0.1)
      
      dfTradeLog <- dfTradeLog[order(as.integer(dfTradeLog$trade_id), decreasing = TRUE),]
      rownames(dfTradeLog) <- 1:nrow(dfTradeLog)
      incProgress(0.1)
      
      rv$dfTradeLog <- dfTradeLog
    }, message = "Refreshing...")
  })
  
  output$toCurrentPrice <- renderText({
    sprintf("Current price per unit: %s", rv$currentPrice)
  })
  
  observeEvent(input$abEvaluate, {
    updateNumericInput(session = session, inputId = "niPriceAtOrAbove", value = (rv$currentPrice + input$niProfitMargin))
  })
  
  observeEvent({ c(input$niNdays, input$niNhours) }, handlerExpr = {
    rv$dateFrom <- as.integer(Sys.time() - lubridate::days(input$niNdays) - lubridate::hours(input$niNhours))
    rv$dateTo <- as.integer(Sys.time())
  })
  
  observeEvent(input$diSince, handlerExpr = {
    if (length(input$diSince) == 0) { return(invisible(NULL)) }
    rv$dateFrom <- dateToInt(input$diSince)
  })
  
  observeEvent(input$diUntil, handlerExpr = {
    if (length(input$diUntil) == 0) { return(invisible(NULL)) }
    rv$dateTo <- dateToInt(input$diUntil)
  })
  
  observeEvent({ c(rv$dateFrom, rv$dateTo) }, handlerExpr = {
    
    # check if we need to update the trade log
    
    if (length(rv$dateFrom) == 0 | length(rv$dateTo) == 0) { return(invisible(NULL)) }
    
    if (length(rv$dfTradeLog) > 0) {
      #message("min(rv$dfTradeLog$created): ", min(rv$dfTradeLog$created))
      #message("rv$dateFrom: ", rv$dateFrom)
      #message("max(rv$dfTradeLog$created): ", max(rv$dfTradeLog$created))
      #message("rv$dateTo: ", rv$dateTo)
      if ((min(rv$dfTradeLog$created) <= rv$dateFrom) & 
          (max(rv$dfTradeLog$created) >= rv$dateTo)) {
        #message("No need to update rv$dfTradeLog")
        return(invisible(NULL))
      }
    }
    
    rv$refresh <- Sys.time()
    
  })
  
  output$toVolume <- renderText({
    if (length(rv$dfTradeLog) == 0) { return(NA_character_) }
    idxx <- which(rv$dfTradeLog[["price"]] >= input$niPriceAtOrAbove &
                    rv$dfTradeLog$created >= rv$dateFrom &
                    rv$dfTradeLog$created <= rv$dateTo)
    return(sprintf("%s units traded at or above %s between %s and %s across %s trades.", 
                   sum(rv$dfTradeLog[["volume"]][idxx]),
                   input$niPriceAtOrAbove, 
                   as.POSIXct(rv$dateFrom, origin = "1970-01-01 00:00:00", tz = "GMT"),
                   as.POSIXct(rv$dateTo, origin = "1970-01-01 00:00:00", tz = "GMT"),
                   length(idxx)))
  })
  
  output$dtoTradeLog <- DT::renderDataTable({
    if (length(rv$dfTradeLog) == 0) { return(NULL) }
    
    df <- rv$dfTradeLog
    df$created <- as.POSIXct(df$created, origin = "1970-01-01 00:00:00")
    return(df)
  })
  
  rv$refresh <- Sys.time()
} 

modelBtest <- function() {
  
  uiX <- function() {
    modelBUI(id = "mbui1")
  }
  
  serverX <- function(input, output, session) {
    callModule(module = modelB, id = "mbui1", pairId = 26)
  }
  
  shinyApp(ui = uiX, server = serverX)
}
