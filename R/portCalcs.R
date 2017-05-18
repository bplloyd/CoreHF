return.port = function(port, weights = rep(1/ncol(port), ncol(port))){
  port %*% weights
 }

 return.annualized = function(R, freq=12){
   R=as.vector(R)
   # prod(1 + R)^(freq/nrow(R)) - 1
   exp(sum((freq/length(R))*log(1+R))) - 1
 }

# return.annualized2 = function(R, freq=12){
#   #prod(1 + R)^(freq/nrow(R)) - 1
#   exp(sum(log(1+R)))^(freq/nrow(R)) - 1
#
# }

std.annualized = function(R, freq=12){
  sqrt(freq) * sd(R)
}

sharpe.annualized = function(R, rf=0, freq=12){
  return.annualized(R-rf, freq)/std.annualized(R, freq)
}

beta.sim = function(R, bm){
  return(lm(R ~ bm)$coefficients[2])
}

max.drawdown = function(R){
  R.cumulative = cumprod(1+R)
  max.cumulative = cummax(c(1, R.cumulative))[-1]
  drawdowns = R.cumulative/max.cumulative - 1
  max(abs(drawdowns))
}
