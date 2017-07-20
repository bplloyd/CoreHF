maxSharpe = function(R, minweights=rep(0, ncol(R)), maxweights = rep(1, ncol(R)), randports=NULL, targetReturn = NULL, targetVolatility = NULL, percentToAvg = 0.01){
  assets = names(R)
  if(is.null(randports)){
    randports = randomPortfolios(assets, maxweights, minweights)
  }
  R.analyzed = analyzePorts(R, randports, calcs = c("return.annualized", "std.annualized", "sharpe.annualized"))
  if(!is.null(targetReturn)){
    R.analyzed = R.analyzed[R.analyzed[, "return.annualized"] >= targetReturn, ]
  }
  if(!is.null(targetVolatility)){
    R.analyzed = R.analyzed[R.analyzed[, "std.annualized"] <= targetVolatility, ]
  }
  if(nrow(R.analyzed) == 0)
    best = R.analyzed
  else{
    sorted = sortAnalysis(assets, R.analyzed, funcs = c("sharpe.annualized"))[[1]]
    if(percentToAvg * nrow(sorted) < 1)
      best = sorted[1,,drop=FALSE]
    else{
      best = colMeans(sorted[1:ceiling(percentToAvg * nrow(sorted)),])
      best = as.matrix(t(best))
    }
  }
  result = list(Portfolio = best[, colnames(best) %in% assets],
                Stats = best[, !(colnames(best)%in%assets)])
}