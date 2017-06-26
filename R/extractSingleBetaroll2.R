extractSingleBetaroll2 = function(Fm.roll, name){
  #require(xts)
  nonerrs = which(sapply(Fm.roll, function(m)class(m) == "tsfm"))
  if(length(nonerrs) > 0){
    factor.names = unique(unlist(lapply(Fm.roll[nonerrs], function(m)names(m[1]$asset.fit[[name]]$coefficients))))
    i0 = min(nonerrs)
    result = matrix(NA, nrow = length(Fm.roll), ncol = length(factor.names))
    colnames(result) = factor.names
    for (i in nonerrs){
      if(name %in% row.names(coef(Fm.roll[[i]]))){
        if(class(Fm.roll[[i]]) == "tsfm"){
          coef.i = Fm.roll[[i]]$asset.fit[[name]]$coefficients
          result[i, names(coef.i)] = coef.i
        }
      }
    }
    row.names(result) = names(Fm.roll)
    return(result)
  } else {
    stop("All models are errors!")
  }
 
  
}
