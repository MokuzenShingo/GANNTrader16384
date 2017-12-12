library(GA)
library(quantmod)
library(nnet)
library(xts)
library(compiler)
library(RcppRoll)
library(ggplot2)
library(dplyr)

##############################################################
#  add reverse trend
##############################################################
# mktdata  : 1:Open, 2:High, 3:Low, 4:Close 
revers_trend <- function(mktdata){
  # revers OHLC
  newClose <- -1 * mktdata$close
  newOpen <- -1 * mktdata$open
  newHigh <- -1 * mktdata$low
  newLow <- -1 * mktdata$high
  mktdata.rev <- cbind(newOpen, newHigh, newLow, newClose)
  
  # ajust value 
  mktdata.ajust <- mktdata.rev + as.numeric(last(mktdata$close)) - as.numeric(first(mktdata.rev$open))
  
  # fix index
  index(mktdata.ajust) <- seq.POSIXt(from = last(index(mktdata)) + 3600, by = 3600, length.out = nrow(mktdata))  # hour:3600, day:86400
  
  # Synthesis mktdata
  rbind(mktdata, mktdata.ajust)
}
revers_trend <- cmpfun(revers_trend)

##############################################################
#  calculates the technical index
##############################################################
# mktdata  : 1:Open, 2:High, 3:Low, 4:Close 
get_indicators <- function(mktdata){
  range.rsi <- 3:8  # length
  range.adx <- 8:35 # length
  
  rsi <- lapply(range.rsi, function(x) RSI(mktdata[,"close"], n = x))
  adx <- lapply(range.adx, function(x) diff(ADX(mktdata[, c("high", "low", "close")], n = x)$ADX))
  adx <- do.call(cbind, adx)
  
  dmi <- lapply(range.adx, function(x) ADX(mktdata[,2:4], n = x)$DIp - ADX(mktdata[,c("high", "low", "close")], n = x)$DIn)
  dmi <- do.call(cbind, dmi)
  
  vol <- roll_sdr(mktdata[, "close"], n = 7)  # volatility
  
  return(list(rsi, adx, dmi, vol))
}
get_indicators <- cmpfun(get_indicators)

##############################################################
# evalute market condition
##############################################################
# gtype       : optimized parameters
# mktdata     : 1:Open, 2:High, 3:Low, 4:Close 
# nn.model    : difine input, output layer (Neural Network model)
# hidden.size : number of hidden units (Neural Network model)
# indicators  : technical indicators
get_signal <- function(gtype, mktdata, nn.model, hidden.size, indicators){

  # set technical indicator 
  rsi <- indicators[[1]][[round(gtype[6]) - 2]]
  adx <- indicators[[2]][,round(gtype[5]) - 7]
  dmi <- indicators[[3]][,round(gtype[5]) - 7]
  
  # preprocessing for Neural Network
  dat.ind <- cbind(rsi, adx * 25, dmi * 2)  # fixed 0 - 100
  colnames(dat.ind) <- c("RSI","diffADX","uniDMI")
  dat.ind <- na.omit(dat.ind)

  # generate signals
  wts <- gtype[7:length(gtype)]
  nnet.model <- nnet(Signal~., size = hidden.size, Wts = wts, data = nn.model,  maxit = 0)

  signal <- as.integer(predict(nnet.model, coredata(dat.ind), type = "class"))
  c(rep(0, nrow(mktdata) - length(signal) + 1), signal)
}  
get_signal <- cmpfun(get_signal)

##############################################################
#  apply the trading strategy
##############################################################
# mktdata      : 1:Open, 2:High, 3:Low, 4:Close
# signal       : marked condition signal (-1:short, 0: do-nothing, 1:long)
# buy_benefit  : optimized parameters
# buy_losscut  : optimized parameters
# sell_benefit : optimized parameters
# sell_losscut : optimized parameters
# vol          : volatility (length = 7)
get_rule_signal <- function(mktdata, signal, buy_benefit, buy_losscut, sell_benefit, sell_losscut, vol) {
  temp.index <- index(mktdata)
  mktdata <- as.data.frame(mktdata)
  steps <- nrow(mktdata) 
  
  rule.sig <- numeric(steps + 1)
  position <- numeric(steps + 1)
  
  for(i in 1:steps){
    # entry strategy
    if(position[i] == 0 && signal[i] != 0){
      rule.sig[i+1] <- signal[i]
      position[i+1] <- signal[i]
      t <- i + 1  
    }
    
    ### exit strategy
    # long position
    if(position[i] == 1){
      if(.subset2(mktdata,"high")[i] > as.numeric(.subset2(mktdata, "close")[t] + buy_benefit * vol[i]) || 
         .subset2(mktdata,"low")[i]  < as.numeric(.subset2(mktdata, "close")[t] - buy_losscut * vol[i]) ||
         signal[i] == -1)
      {
        rule.sig[i+1] <- -1
      } else {
        position[i+1] <-  1
      }
    }
    
    # short position
    if(position[i] == -1){
      if(.subset2(mktdata, "high")[i] > as.numeric(.subset2(mktdata, "close")[t] + sell_losscut * vol[i]) || 
         .subset2(mktdata, "low")[i]  < as.numeric(.subset2(mktdata, "close")[t] - sell_benefit * vol[i]) ||
         signal[i] == 1){
        rule.sig[i+1] <- 1
      } else {
        position[i+1] <- -1
      }
    }
    
  }
  xts(rule.sig, order.by = as.POSIXct(c(temp.index, last(temp.index) + 3600)))
}
get_rule_signal <- cmpfun(get_rule_signal)

##############################################################
# get total return
##############################################################
# close       : closing price each step
# rule.signal : generated signals according to the trading strategy
get_return <- function(close, rule.signal){
  is.trade.close  <- close[which(rule.signal[-length(rule.signal)] != 0)]
  is.trade.signal <- rule.signal[which(rule.signal != 0)]
  n.trade <- length(is.trade.close)
  
  if(n.trade > 1){
    if(n.trade %% 2 != 0) {
      is.trade.close  <- is.trade.close[-n.trade]
      is.trade.signal <- is.trade.signal[-n.trade]
    }
    returns <- (-1 * diff(is.trade.close) * is.trade.signal )[which(1:n.trade %% 2 ==0)]
    sum(returns)
  } else {  # no exit trading
    -100
  }
}
get_return <- cmpfun(get_return)

##############################################################
#  difine fitness function for ga()
##############################################################
# gtype       : optimized parameters
# mktdata     : 1:Open, 2:High, 3:Low, 4:Close 
# nn.model    : difine input, output layer (Neural Network model)
# hidden.size : number of hidden units (Neural Network model)
# indicators  : technical indicators
fitness <- function(gtype, mktdata, hidden.size, nn.model, indicators){
  # evalute market condition  
  signal <- get_signal(gtype, mktdata, nn.model, hidden.size, indicators)
  
  # apply the trading strategy
  rule.signal <- get_rule_signal(mktdata, signal, 
                                 buy_benefit  = round(gtype[1]), buy_losscut  = round(gtype[2]),
                                 sell_benefit = round(gtype[3]), sell_losscut = round(gtype[4]), 
                                 vol = indicators[[4]][,1])
  
  # find return
  get_return(mktdata$close, rule.signal)
}
fitness <- cmpfun(fitness)

##############################################################
#  initialize (ensemble)
##############################################################
# gtype
gtype.ini <- function(validResult) {
  gtype <- select(validResult, starts_with("X"))
  gtype <- as.data.frame(gtype)
  gtype$trainID <- validResult$trainID
  gtype
}

### signal.table
signal.table.ini <- function(validResult, data, indicators){
  gtype <- select(validResult, starts_with("X"))
  ### Neural Network model (evalute market condition)
  hidden.size <- 6
  nn.model <- data.frame(RSI = rep(0, 3), diffADX = rep(0, 3), uniDMI = rep(0, 3),  # input  layer
                         Signal = c("-1", "0", "1")                                 # output layer
  )                                
  wts.size <- ncol(nn.model) * hidden.size + (hidden.size + 1) * 3  # number of connection
  
  signal.table <- apply(gtype, 1, get_signal, data, nn.model, hidden.size, indicators)
  colnames(signal.table) <- validResult$trainID
  as.data.frame(signal.table)
}

# trade.signal.table
trade.signal.table.ini <- function(signal.table) {
  trade.signal.table <- signal.table
  trade.signal.table[,] <- 0
  trade.signal.table$ense_sig <- 0
  trade.signal.table
}

# position.table
position.table.ini <- function(data) {
  position.table <- data
  position.table[,1:4] <- 0
  position.table$posi.flag <- 0
  coredata(position.table)
}

##############################################################
#  each classifier predict action
##############################################################
ind.sig <- function(gtype, curr.price, curr.vola, signal, posi.price, posi.flag) {
  ### entry strategy
  if(posi.flag ==  0) out.sig <- signal
  
  ### exit strategy
  if(posi.flag ==  1){
    out.sig <- ifelse(    coredata(curr.price[, "high"]) > posi.price["close"] + curr.vola * round(gtype[1])  # profit_gain 
                          || coredata(curr.price[, "low"])  < posi.price["close"] - curr.vola * round(gtype[2])  # loss_cut
                          || signal == -1, 
                          -1, 0)
  }
  
  if(posi.flag == -1){
    out.sig <- ifelse(   coredata(curr.price[, "low"])  < posi.price["close"] - curr.vola * round(gtype[3])  # profit_gain
                         || coredata(curr.price[, "high"]) > posi.price["close"] + curr.vola * round(gtype[4])  # loss_cut
                         || signal == 1, 
                         
                         1, 0)
  }
  out.sig
}
##############################################################
#  calculate mode
##############################################################
mode <- function(x) names(which.max(table(x)))  
##############################################################
#  updata position information
##############################################################
updata.position <- function(ense.sig, posi.price, posi.flag, next.price){
  if(posi.flag == 0){
    if(ense.sig == 0) new.position <- c(0, 0, 0, 0, ense.sig)  # do nothing
    if(ense.sig != 0) new.position <- c(coredata(next.price), ense.sig)  # entry
  }
  
  if(posi.flag != 0){
    if(ense.sig == 0) new.position <- c(coredata(posi.price), posi.flag) # hold
    if(ense.sig != 0) new.position <- c(0, 0, 0, 0, 0)         # close  
  }
  names(new.position) <- c("open", "high", "low", "close", "posi.flag") 
  new.position
}
##############################################################
#  delete unclosed trade
##############################################################
is.exit <- function(x) if(nrow(x) %% 2 != 0) x[-nrow(x),]
#################################################################
# bind training results
#################################################################
train_bind <- function(trainDir, outputFile.name){
  trainFiles <- dir(trainDir)  
  temp <- lapply(trainFiles, function(files) { 
    dat <- readRDS(paste0(trainDir, files))
    dat$trainID <- files  
    dat
  })
  trainResults <- do.call(rbind, temp)
  saveRDS(trainResults, outputFile.name)
}
#################################################################
# bind validation results
#################################################################
valid_bind <- function(validDir, outputFile.name){
  validFiles <- dir(validDir)  
  temp <- lapply(validFiles, function(files) readRDS(paste0(validDir, files)))
  validResults <- do.call(rbind, temp)
  saveRDS(validResults, outputFile.name)
}