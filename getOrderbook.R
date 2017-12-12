################################################################
# set  　　　　　　　　　　　　　　　　　　　　　　　
#################################################################
get_orderbook <- function(){
  source("helper.R")
  test.period <- c(paste0(Sys.Date() - 7, "::")) # set test period
  
  #################################################################
  # apply test data
  #################################################################
  # set the agent
  validResult <- readRDS(paste0("./data/USDJPYhour_bestGeneration_16384.rds"))
  load("./data/realtimeData.RData")
  test.data <- na.omit(realtimeData[["USDJPYhour"]][test.period])    # extract data of test period
  
  # calclate technical indicators
  indicators <- get_indicators(test.data)                          
  
  # initialize classifers
  gtype <- gtype.ini(validResult)                                  # gtype
  signal.table <- signal.table.ini(validResult, data = test.data,indicators)  # signal.table
  position.table <- position.table.ini(data = test.data)           # position.table
  
  orderbook.org <- readRDS("./data/USDJPYhour_16384_orderbook.rds")
  orderbook.org <- orderbook.org[paste0("::", Sys.Date() -4)]
  temp <- orderbook.org[which(orderbook.org$exit.flag == 1),]
  test.data <- test.data[paste0(index(temp[nrow(temp),]),"::")]
  signal.table <- tail(signal.table, nrow(test.data) + 1)
  position.table <- tail(position.table, nrow(test.data) +1 )
  indicators[[4]] <- tail(indicators[[4]], nrow(test.data))
  
  # apply test data: combine the output of agents
  if(nrow(test.data) == 1) return()
  for(step in 1:(nrow(test.data) -1)){
    # generates the trading action each agent
    step.sig <- apply(gtype, 1, function(x){
      ID <- x[length(x)]
      x <- as.numeric(x[-length(x)])
      ind.sig(gtype = x,
              curr.price  = test.data[step,],
              curr.vola   = indicators[[4]][step, 1],
              signal      = signal.table[step, ID],
              posi.price  = position.table[step, c("open", "high", "low", "close")],
              posi.flag   = position.table[step,"posi.flag"]
      )  
    })
    
    # combine the output of agents
    ense.sig <- as.integer(mode(step.sig))
    
    # update the position
    position.table[step + 1,] <- updata.position(ense.sig,
                                                 posi.price = position.table[step, c("open", "high", "low", "close")],
                                                 posi.flag  = position.table[step,"posi.flag"],
                                                 next.price = test.data[step +1,]
    )
  }
  ##############################################################
  #  generates the orderbook
  ##############################################################
  position.table <- position.table[-nrow(position.table),]
  orderbook <- cbind(test.data, position.table)
  orderbook$trade.flag <- abs(diff(orderbook$posi.flag))
  orderbook$exit.flag  <- ifelse(orderbook$trade.flag == 1 & orderbook$posi.flag  == 0, 1, 0)
  orderbook$entry.flag <- ifelse(orderbook$trade.flag == 1 & orderbook$exit.flag  == 0, 1, 0)
  
  orderbook.temp <- orderbook[orderbook$trade.flag == 1,]
  orderbook.temp$tradeID <- 0
  orderbook.temp$diff.price <- diff(orderbook.temp$close)
  orderbook.temp$posi.temp  <- c(0, orderbook.temp$posi.flag[-length(orderbook.temp$posi.flag)])
  orderbook.temp$return <- orderbook.temp$diff.price * orderbook.temp$posi.temp
  
  orderbook.temp$diff.price[1] <- 0
  orderbook.temp$return[1] <- 0
  orderbook.temp$return_cumsum <- 0
  
  orderbook.temp <- rbind(orderbook.org, orderbook.temp[-1,])
  orderbook.temp$tradeID <- cumsum(orderbook.temp$entry.flag)
  orderbook.temp$return_cumsum <- cumsum(orderbook.temp$return)
  
  saveRDS(orderbook.temp, file = "./data/USDJPYhour_16384_orderbook.rds")
}