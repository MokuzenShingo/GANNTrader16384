library(TFX)
library(xts)
library(dplyr)
library(quantmod)
library(RcppRoll)
library(nnet)
library(DT)

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
#  initialize
##############################################################
### signal.table
signal.table.ini <- function(validResult, data,indicators){
  gtype <- select(validResult, starts_with("X"))
  signal.table <- apply(gtype, 1, get_signal, data, nn.model, hidden.size, indicators)
  colnames(signal.table) <- validResult$trainID
  as.data.frame(signal.table)
}

#### gtype
gtype.ini <- function(validResult) {
  gtype <- select(validResult, starts_with("X"))
  gtype <- as.data.frame(gtype)
  gtype$trainID <- validResult$trainID
  gtype
}

### NN model
hidden.size <- 6
nn.model <- data.frame( RSI = rep(0, 3), diffADX = rep(0, 3), uniDMI = rep(0, 3), Signal = c("-1", "0", "1"))
wts.size <-  ncol(nn.model) * hidden.size + (hidden.size + 1) * 3 

##############################################################
#  generate market condition signal
##############################################################
get_signal <- function(x, mktdata, dat, hidden.size, indicators){
  ### calculate technical indicator
  rsi <- indicators[[1]][[round(x[6]) - 2]]
  adx <- indicators[[2]][,round(x[5]) - 7]
  dmi <- indicators[[3]][,round(x[5]) - 7]
  
  # preprocessing technical indicator
  dat.ind <- cbind(rsi, adx * 25, dmi * 2)
  colnames(dat.ind) <- c("RSI","diffADX","uniDMI")
  dat.ind <- na.omit(dat.ind)
  
  ### caluculate market condition signal
  wts <- x[7:length(x)]
  nnet.model <- nnet(Signal~., size = hidden.size, Wts = wts, data= dat,  maxit = 0)
  signal <- as.integer(predict(nnet.model, dat.ind, type = "class"))
  c(rep(0, nrow(mktdata) - length(signal) + 1), signal)
}

##############################################################
#  calculate technical indicator
##############################################################
# mktdata  1:Open, 2:High, 3:Low, 4:Close 
get_indicators <- function(mktdata){
  rsi <- lapply(c(3:8),function(x) RSI(mktdata[,4], n = x))
  
  adx <- lapply(c(8:35), function(x) diff(ADX(mktdata[,2:4], n = x)$ADX))
  adx <- do.call(cbind,adx)
  
  dmi <- lapply(c(8:35), function(x) ADX(mktdata[,2:4], n = x)$DIp - ADX(mktdata[,2:4], n = x)$DIn) 
  dmi <- do.call(cbind,dmi)
  
  vol <- roll_sdr(mktdata[, 4], n = 7) # volatility
  
  list(rsi, adx, dmi, vol) # return
}

##############################################################
#  convert text
##############################################################
action_text <- function(x){
  if(x == -1){
    "short"
  }else if(x ==  0){
    "hold"
  }else if(x ==  1){
    "long"
  } 
}

percent_text <- function(x) sprintf("%1.1f%%", x * 100)
