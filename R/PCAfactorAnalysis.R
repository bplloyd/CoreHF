PCAfactorAnalysis = function(R, weights = NULL, useCor = F)
{
  R=na.omit(R)
  if(useCor)
    R.eig = eigen(cor(R))
  else
    R.eig = eigen(cov(as.matrix(R)))
  
  V=R.eig$vectors
  row.names(V)=names(R)
  colnames(V) = paste0("PC", 1:ncol(V))
  for(j in 1:ncol(V))
  {
    # if(V[which(abs(V[,j])==max(abs(V[,j]))),j] < 0)
    if(sum(V[,j])<0)
      V[,j] = -1*V[,j]
  }
  D = R.eig$values
  PrComps = as.matrix(R)%*%V
  colnames(PrComps) = paste0("PC", 1:ncol(PrComps))
  
  res = list(StdDeviations = sqrt(D), 
              Variances  = D, 
              Loadings = V, 
              Components = PrComps,
              PercentVariance = rbind(D/sum(D), cumsum(D/sum(D))),
              PercentStdDeviation = rbind(sqrt(D)/sum(sqrt(D)), cumsum(sqrt(D)/sum(sqrt(D)))),
              ComponentCorrelation = cor(x = PrComps, y = R)
              )
  
  if(!is.null(weights))
  {
     res = append(res, list(FactorLoadings = abs(t(V)%*%as.vector(weights))))
     res = append(res, list(ContributionToRisk = (res$Variances*res$FactorLoadings^2)/sum(res$Variances*res$FactorLoadings^2)))
     ent = (exp(-sum(res$ContributionToRisk*log(res$ContributionToRisk)))-1)/(length(V)-1)
     res = append(res, list(Entropy = ent))
  }
  
  return(res)
}