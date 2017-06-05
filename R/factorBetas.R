factorBetas = function(R, factors, scale=TRUE){
  if(scale){
    betas = sapply(factors, function(f)PerformanceAnalytics::CAPM.beta(mgr.data, scale(f)))
  } else {
    betas = sapply(factors, function(f)PerformanceAnalytics::CAPM.beta(mgr.data, f))
  }
  
  betas = as.data.frame(betas[order(betas, decreasing = TRUE)])
  betas = cbind(betas, sapply(factors[, row.names(betas)], function(f)start(na.omit(f))))
  colnames(betas) = c("Beta", "Start")
  betas
}