cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  # sig = c()
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      # if(tmp$p.value <= 1-conf.level){
      #   sig = c(sig, colnames(mat))
      # }
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p = p.mat, lower = lowCI.mat, upper = uppCI.mat))
}

cor.p = function(Ra, Rb, conf.level = 0.95){
  R = cbind(Ra, Rb)[index(na.omit(Ra)),]
  result = t(sapply(R[, 2:ncol(R)], function(rb){
    tmp <- cor.test(R[,1], rb, conf.level = conf.level)
    c(p=tmp$p.value, cor=tmp$estimate, lower=tmp$conf.int[1], upper=tmp$conf.int[2])
  }))
  # for(i in 2:ncol(R)){
  #   tmp <- cor.test(R[,1], R[,i], conf.level = conf.level)
  #   if(tmp$p.value <= 1-conf.level){
  #     sig = c(sig, colnames(mat))
  #   }
  # }
  colnames(result)[2] = "cor"
  result[order(result[, 1]), ]
  
}