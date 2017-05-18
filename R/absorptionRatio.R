absorptionRatio = function(R, n=1, useCor = FALSE, use = "p")
{
  if(useCor){
    R.eig = eigen(cor(R, use = use))
  } else { 
    R.eig = eigen(cov(R, use = use))
  }
  return((cumsum(R.eig$values)/sum(R.eig$values))[n])
}

rollAbsorptionRatio = function(R, n=1, useCor = FALSE, use= "p", initialWindow = 12, fixedWindow = TRUE){
  R=na.omit(R)
  if(fixedWindow){
    return(xts::xts(sapply(initialWindow:nrow(R), 
                    function(j)return(absorptionRatio(R[(j-initialWindow+1):j,], n=n, useCor = useCor))),
             order.by = index(R)[initialWindow:nrow(R)]))
    
  } else {
    return(xts::xts(sapply(initialWindow:nrow(R), 
                    function(j)return(absorptionRatio(R[1:j,], n=n, useCor = useCor))),
             order.by = index(R)[initialWindow:nrow(R)]))
  }
}

compareRollingAR = function(base.port, prospects, n=1, useCor=FALSE, use = "p", initialWindow = 12, fixedWindow=TRUE){
  ar_roll = lapply(prospects, 
                   FUN = function(p) rollAbsorptionRatio(R=na.omit(cbind(base.port, p)), 
                                                         n = n, 
                                                         useCor = useCor, 
                                                         initialWindow = initialWindow, 
                                                         fixedWindow = fixedWindow
                                                         )
                   )
  ar_roll_combined = ar_roll[[1]]
  for(i in 2:length(ar_roll)){
    ar_roll_combined = cbind(ar_roll_combined, ar_roll[[i]])
  }
  names(ar_roll_combined) = names(ar_roll)
  return(ar_roll_combined)
}