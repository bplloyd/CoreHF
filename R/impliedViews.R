impliedViews = function(R, weights, sigma = cov(R, use = "p")){
  mcr = (sigma%*%weights)/sqrt(weights%*%sigma%*%weights)[1,1]
  views = matrix(data = NA, nrow = nrow(mcr), ncol = nrow(mcr))
  row.names(views) = colnames(views) = row.names(mcr)
  for(i in 1:nrow(views)){
    for(j in 1:ncol(views)){
      views[i, j] = mcr[colnames(views)[j],1]/mcr[row.names(views)[i],1]
    }
  }
  views
}