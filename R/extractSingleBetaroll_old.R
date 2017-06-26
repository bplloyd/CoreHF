extractSingleBetaroll_old = function(Fm.roll, name){
  #require(xts)
  nonerrs = which(sapply(Fm.roll, function(m)class(m) == "tsfm"))
  if(length(nonerrs) > 0){
    i0 = min(nonerrs)
    result = matrix(NA, nrow = length(Fm.roll), ncol = ncol(coef(Fm.roll[[i0]])))
    colnames(result) = colnames(coef(Fm.roll[[i0]]))
    for (i in nonerrs){
      if(name %in% row.names(coef(Fm.roll[[i]]))){
        if(class(Fm.roll[[i]]) == "tsfm"){
          result[i,] = t(as.vector(coef(Fm.roll[[i]])[c(name),])[1,])
        }
      }
    }
    row.names(result) = names(Fm.roll)
    return(result)
  } else {
    stop("All models are errors!")
  }
 
  
}
