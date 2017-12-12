return.analysis <- function(returns){
  round(
    c(
      trades         = length(returns),
      totalReturn   = sum(returns),
      averageReturn = mean(returns),
      returnSd      = sd(returns),
      winRate       = sum(returns > 0) / length(returns),
      maxReturn     = max(returns),
      minReturn     = min(returns),
      winMeanReturn = mean(returns[which(returns > 0)]),
      losMeanReturn = mean(returns[which(returns < 0)]),
      halfKellyRatio      = KellyRatio(returns, Rf = 0, method = "half"), 
      sharpeRatio   = mean(returns) / sd(returns),
      maxDrawdown   = max(cummax(returns) - returns)
    ), 3)
}