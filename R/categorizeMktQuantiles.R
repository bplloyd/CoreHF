categorizeMktQuantiles = function(jointDist, mkt, probs=NULL)
{
  #library(data.table)
  if(is.null(probs))
    probs = seq(0,1,0.1)

  # colN = colnames(dt)

  #q = apply(jointDist[, mkt, drop=F], 2, FUN = function(c)return(quantile(c,probs)))
  q = quantile(jointDist[, mkt, drop=F], probs)
  #colnames(q) = mkts

  #q_cats = replicate(n=length(mkt), expr = rep(NA_integer_, nrow(jointDist)))

  q_cats = rep(NA_integer_, nrow(jointDist))

  # colnames(q_cats ) = paste0(mkts, "_QUANTILES")


  for(i in 1:(length(q)-1)){
    if(i==1){
      # q_cats[which(jointDist[, n] < q[i+1, n]), c(paste0(n, "_QUANTILES"))] = i
      q_cats[which(jointDist[, mkt, drop=F] < q[i+1])] = i
    } else if(i == (length(q)-1)){
      # q_cats[which(jointDist[, n] >= q[i, n]), c(paste0(n, "_QUANTILES"))] = i
      q_cats[which(jointDist[, mkt, drop=F] >= q[i])] = i
    } else{
      # q_cats[which((jointDist[, n] >= q[i, n]) & (jointDist[, n] < q[i+1, n])), c(paste0(n, "_QUANTILES"))] = i
      q_cats[which((jointDist[, mkt, drop=F] >= q[i]) & (jointDist[, mkt, drop=F] < q[i+1]))] = i
    }
  }
  return(q_cats)
}

