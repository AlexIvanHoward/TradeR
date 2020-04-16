
#'@title getCurrencyList
#'@description Retrieves the list of available currencies from the iCE3x platform.
#'@param all Logical. If TRUE, will retrieve the full list of currencies from the iCE3x platform; 
#'  if FALSE, will retrieve only the first n currencies (where n = 'itemsPerPage') from the specified page.
#'
getCurrencyList <- function(orderBy = c("iso", "name", "currency_id")[3],
                            orderDir = c("asc", "desc")[1],
                            all = TRUE,
                            itemsPerPage = 100,
                            page = 1) {
  
  if (all) { itemsPerPage <- 100 }
  res <- list()
  
  q <- sprintf("%sorder_by=%s&order_direction=%s&items_per_page=%s&page=%s", 
               "https://ice3x.com/api/v1/currency/list?",
               orderBy, orderDir, itemsPerPage, page)
  
  res[[1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
  
  if (all) {
    while(res[[length(res)]]$pagination$current_page < res[[length(res)]]$pagination$total_pages) {
      
      q <- sprintf("%sorder_by=%s&order_direction=%s&items_per_page=%s&page=%s", 
                   "https://ice3x.com/api/v1/currency/list?",
                   orderBy, orderDir, itemsPerPage, res[[length(res)]]$pagination$current_page +1)
      
      res[[length(res)+1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
    }
  }
  
  res <- lapply(X = res, FUN = function(x) x$response$entities) 
  res <- do.call("rbind", res)
  
  return(res)
}

#'@title getTradeLog
#'@param all Logical. If TRUE, will return all trades between the specified dates; 
#'  if FALSE, will return only the first n trades (where n = 'itemsPerPage') from the specified page.
#'@param convertDates Logical. If TRUE, dates (returned as integers by the iCE3x platform) will be returned in POSIXct format;
#'  if FALSE, integer dates will be returned as integers.
#'
getTradeLog <- function(pairId,
                        type = c("buy","sell")[1],
                        dateFrom = NULL,
                        dateTo = NULL,
                        orderBy = c("trade_id", "pair_id", "type", "price", "volume", "created")[1],
                        orderDir = c("asc","desc")[1],
                        all = TRUE,
                        itemsPerPage = 100,
                        page = 1,
                        convertDates = TRUE) {
  
  args <- check_args_getTradeLog(pairId = pairId, type = type, dateFrom = dateFrom, dateTo = dateTo, 
                                 orderBy = orderBy, orderDir = orderDir, all = all, itemsPerPage = itemsPerPage, 
                                 page = page, convertDates = convertDates, debug = TRUE)
  #assign("args", args, .GlobalEnv)

  res <- list()
  
  q <- sprintf("%spair_id=%s&type=%s&date_from=%s&date_to=%s&order_by=%s&order_direction=%s&items_per_page=%s&page=%s", 
               "https://ice3x.com/api/v1/trade/list?",
               args$pairId, args$type, args$dateFrom, args$dateTo, args$orderBy, args$orderDir, args$itemsPerPage, args$page)
  
  res[[1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
  
  if (args$all) {
    while(res[[length(res)]]$pagination$current_page < res[[length(res)]]$pagination$total_pages) {
      
      q <- sprintf("%spair_id=%s&type=%s&date_from=%s&date_to=%s&order_by=%s&order_direction=%s&items_per_page=%s&page=%s", 
                   "https://ice3x.com/api/v1/trade/list?",
                   args$pairId, args$type, args$dateFrom, args$dateTo, args$orderBy, args$orderDir, args$itemsPerPage, 
                   res[[length(res)]]$pagination$current_page +1)
      
      res[[length(res)+1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
    }
  }
  
  res <- lapply(X = res, FUN = function(x) x$response$entities) 
  res <- do.call("rbind", res)
  if (length(res) == 0) { return(NULL) }
  if (length(res[[1]]) == 0) { return(NULL) }
  
  res$price <- as.numeric(res$price)
  res$volume <- as.numeric(res$volume)
  res$created <- as.integer(res$created)
  if (args$convertDates) { res$created <- as.POSIXct(x = res$created, origin = "1970-01-01 00:00:00") }
  
  return(res)
}

#'@title check_args_getTradeLog
#'@description Checks and modifies arguments received by function getTradeLog.
#'
check_args_getTradeLog <- function(pairId,
                                   type,
                                   dateFrom,
                                   dateTo,
                                   orderBy,
                                   orderDir,
                                   all,
                                   itemsPerPage,
                                   page,
                                   convertDates,
                                   debug = FALSE) {
  
  ftag <- "check_args_getTradeLog(...):"
  
  # pairId
  if (debug) { 
    message(sprintf("%s 'pairId' = %s", ftag, paste(pairId, sep = ",", collapse = ","))) 
  }
  pairId <- as.integer(pairId)
  if (length(pairId) == 0 | length(pairId) > 1) { 
    stop("'pairId' must be non-NULL and of length = 1 exactly.") 
  }
  if (is.na(pairId)) { 
    stop("'pairId' must be a non-NA integer.") 
  }
  
  # type
  if (debug) { 
    message(sprintf("%s 'type' = %s", ftag, paste(type, sep = ",", collapse = ","))) 
  }
  if (length(type) == 0 | length(type) > 1) { 
    stop("'type' must be one of c('buy','sell').") 
  }
  if (!(type %in% c("buy","sell"))) { 
    stop("'type' must be one of c('buy','sell').") 
  }
  
  # dateFrom, dateTo
  if (debug) { 
    message(sprintf("%s 'dateFrom' = %s", ftag, paste(dateFrom, sep = ",", collapse = ","))) 
    message(sprintf("%s 'dateTo' = %s", ftag, paste(dateTo, sep = ",", collapse = ","))) 
  }
  if (is.null(dateFrom)) { dateFrom <- as.integer(Sys.time() - lubridate::days(1)) }
  dateFrom <- dateToInt(dateFrom)
  if (is.null(dateTo)) { dateTo <- as.integer(Sys.time()) }
  dateTo <- dateToInt(dateTo)
  if (dateFrom == dateTo) {
    dateFrom <- as.integer(as.POSIXct(dateTo, origin = "1970-01-01 00:00:00") - days(1))
  }
  
  # orderBy
  if (debug) { 
    message(sprintf("%s 'orderBy' = %s", ftag, paste(orderBy, sep = ",", collapse = ","))) 
  }
  if (length(orderBy) == 0 | length(orderBy) > 1) { 
    stop("'orderBy' must be of length = 1.") 
  }
  if (!(orderBy %in% c("trade_id", "pair_id", "type", "price", "volume", "created"))) {
    stop("'orderBy' must be one of c('trade_id', 'pair_id', 'type', 'price', 'volume', 'created').")
  }
  
  # orderDir
  if (debug) { 
    message(sprintf("%s 'orderDir' = %s", ftag, paste(orderDir, sep = ",", collapse = ","))) 
  }
  if (length(orderDir) == 0 | length(orderDir) > 1) { 
    stop("'orderDir' must be of length = 1.") 
  }
  if (!(orderDir %in% c("asc", "desc"))) {
    stop("'orderDir' must be one of c('asc','desc').")
  }
  
  # all
  if (debug) { 
    message(sprintf("%s 'all' = %s", ftag, paste(all, sep = ",", collapse = ","))) 
  }
  all <- as.logical(all)
  if (length(all) == 0 | length(all) > 1) { 
    stop("'all' must be a logical of length = 1.") 
  }
  
  if (all) { itemsPerPage <- 100 }
  
  # itemsPerPage
  if (debug) { 
    message(sprintf("%s 'itemsPerPage' = %s", ftag, paste(itemsPerPage, sep = ",", collapse = ","))) 
  }
  itemsPerPage <- as.integer(itemsPerPage)
  if (length(itemsPerPage) == 0 | length(itemsPerPage) > 1) { 
    stop("'itemsPerPage' must be non-NULL and of length = 1 exactly.") 
  }
  if (is.na(itemsPerPage)) { 
    stop("'itemsPerPage' must be a non-NA integer.") 
  }
  if (itemsPerPage < 1 | itemsPerPage > 100) {
    stop("'itemsPerPage' must be greater than 0 and less than or equal to 100.")
  }
 
  # page
  if (debug) { 
    message(sprintf("%s 'page' = %s", ftag, paste(page, sep = ",", collapse = ","))) 
  }
  page <- as.integer(page)
  if (length(page) == 0 | length(page) > 1) { 
    stop("'page' must be non-NULL and of length = 1 exactly.") 
  }
  if (is.na(page)) { 
    stop("'page' must be a non-NA integer.") 
  }
  if (page < 1) {
    stop("'page' must be an integer greater than 0.")
  }
  
  # convertDates
  if (debug) { 
    message(sprintf("%s 'convertDates' = %s", ftag, paste(convertDates, sep = ",", collapse = ","))) 
  }
  convertDates <- as.logical(convertDates)
  if (length(convertDates) == 0 | length(convertDates) > 1) { 
    stop("'convertDates' must be a logical of length = 1.") 
  }
  
  return(list(pairId = pairId,
              type = type,
              dateFrom = dateFrom,
              dateTo = dateTo,
              orderBy = orderBy,
              orderDir = orderDir,
              all = all,
              itemsPerPage = itemsPerPage,
              page = page,
              convertDates = convertDates))
}

getPairList <- function(orderBy = c("pair_id", "currency_id_from", "currency_id_to")[1],
                        orderDir = c("asc","desc")[1],
                        all = TRUE,
                        itemsPerPage = 100,
                        page = 1) {
  
  if (all) { itemsPerPage <- 100 }
  res <- list()
  
  q <- sprintf("%sorder_by=%s&order_direction=%s&items_per_page=%s&page=%s", 
               "https://ice3x.com/api/v1/pair/list?",
               orderBy, orderDir, itemsPerPage, page)
  
  res[[1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
  
  if (all) {
    while(res[[length(res)]]$pagination$current_page < res[[length(res)]]$pagination$total_pages) {
      
      q <- sprintf("%sorder_by=%s&order_direction=%s&items_per_page=%s&page=%s", 
                   "https://ice3x.com/api/v1/pair/list?",
                   orderBy, orderDir, itemsPerPage, res[[length(res)]]$pagination$current_page +1)
      
      res[[length(res)+1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
    }
  }
  
  res <- lapply(X = res, FUN = function(x) x$response$entities) 
  res <- do.call("rbind", res)
  names(res) <- gsub(pattern = "filters.", replacement = "", x = names(res), fixed = TRUE)
  names(res) <- gsub(pattern = ".", replacement = "_", x = names(res), fixed = TRUE)
  
  for(vnm in c("pair_id", "currency_id_from", "currency_id_to")) {
    res[[vnm]] <- as.character(res[[vnm]])
  }
  for (vnm in c("price_min", "price_step", "amount_min", "amount_step")) {
    res[[vnm]] <- as.numeric(res[[vnm]])
  }
  
  return(res)
  
}

# not a very helpful API call.
getOrderBook <- function(pairId,
                         dateFrom = NULL,
                         dateTo = NULL,
                         type = c("bid", "ask")[1],
                         convertDates = TRUE) {
  
  if (is.null(dateFrom)) { dateFrom <- as.integer(Sys.time() - lubridate::days(1)) }
  dateFrom <- dateToInt(dateFrom)
  if (is.null(dateTo)) { dateTo <- as.integer(Sys.time()) }
  dateTo <- dateToInt(dateTo)
  if (dateFrom == dateTo) {
    dateFrom <- as.integer(as.POSIXct(dateTo, origin = "1970-01-01 00:00:00") - days(1))
  }
  
  #if (all) { itemsPerPage <- 100 }
  
  res <- list()
  
  q <- sprintf("%spair_id=%s&date_from=%s&date_to=%s&type=%s", 
               "https://ice3x.com/api/v1/orderbook/info?",
               pairId, dateFrom, dateTo, type)
  
  res[[1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
  
  # if (all) {
  #         while(res[[length(res)]]$pagination$current_page < res[[length(res)]]$pagination$total_pages) {
  # 
  #                 q <- sprintf("%spair_id=%s&date_from=%s&date_to=%s&type=%s", 
  #                              "https://ice3x.com/api/v1/orderbook/info?",
  #                              pairId, dateFrom, dateTo, type)
  # 
  #                 res[[length(res)+1]] <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
  #         }
  # }
  
  res <- lapply(X = res, FUN = function(x) x$response$entities) 
  res <- do.call("rbind", res)
  
  res$price <- as.numeric(res$price)
  res$amount <- as.numeric(res$amount)
  if (convertDates) { res$created <- as.POSIXct(x = res$created, origin = "1970-01-01 00:00:00") }
  
  return(res)
  
}

getCurrentPrice <- function(pairId) {
  
  q <- sprintf("%spair_id=%s", 
               "https://ice3x.com/api/v1/orderbook/ticker?",
               pairId)
  res <- jsonlite::fromJSON(txt = q, simplifyDataFrame = TRUE, flatten = TRUE)
  res <- res$response$entities
  
  names(res) <- gsub(pattern = ".", replacement = "_", x = names(res), fixed = TRUE)
  
  for (vnm in c("ask_price", "ask_amount", "bid_price", "bid_amount")) {
    res[[vnm]] <- as.numeric(res[[vnm]])
  }
  
  return(res)
}

dateToInt <- function(x) {

  if (is.Date(x)) { x <- sprintf("%s 00:00:00", x) }
  if (is.POSIXct(x)) { x <- as.integer(x) }
  if (!is.integer(x)) { x <- as.integer(lubridate::ymd_hms(x)) }
  
  return(x)
}
