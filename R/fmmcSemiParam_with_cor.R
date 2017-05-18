fmmcSemiParam_with_cor = function (B = 1000,
                                   factor.ret,
                                   beta,
                                   alpha,
                                   resid.par,
                                   resids,
                                   resid.dist = c("nonparametric", "normal",
                                                                       "Cornish-Fisher", "skew-t"), boot.method = c("random", "block"),
          seed = 123, tol = 0.001)
{
  if (missing(factor.ret)) {
    stop("Missing argument: factor.ret")
  }
  else {
    #factor.ret <- na.omit(as.matrix(factor.ret))
    factor.ret <- na.omit(factor.ret)
    #factor.names <- colnames(factor.ret)
    factor.names <- colnames(beta)
    K = ncol(factor.ret)
    T = nrow(factor.ret)
  }
  if (missing(beta)) {
    stop("Missing argument: beta")
  }
  else {
    fund.names <- rownames(beta)
    N = nrow(beta)
    # if (all(colnames(beta) != factor.names) || ncol(beta) !=
    #     length(factor.names)) {
    #   stop("Invalid argument: beta and factor.ret should correspond to the same \n           set of factors")
    # }
  }
  resid.dist = resid.dist[1]
  switch(resid.dist, normal = {
    if (ncol(resid.par) != 1) {
      stop("Invalid argument: resid.par")
    }
  }, `Cornish-Fisher` = {
    if (ncol(resid.par) != 3) {
      stop("Invalid argument: resid.par")
    }
  }, `skew-t` = {
    if (ncol(resid.par) != 4) {
      stop("Invalid argument: resid.par")
    }
  }, `nonparametric` = {},  stop("Invalid argument: resid.dist must be 'normal', 'Cornish-Fisher' or 'skew-t'"))
  boot.method = boot.method[1]
  if (!(boot.method %in% c("random", "block"))) {
    stop("Invalid argument: boot.method must be either 'random' or 'block'")
  }
  if (missing(alpha)) {
    alpha <- matrix(0, nrow(beta))
    rownames(alpha) = fund.names
  }
  if ((nrow(beta) != nrow(alpha)) || (nrow(beta) != nrow(resid.par))) {
    stop("Invalid argument: alpha, beta and resid.par should have the same \n         number of funds")
  }
  set.seed(seed)
  if (boot.method == "random") {
    boot.idx <- sample(x = T, size = B, replace = TRUE)
  }
  else {
    boot.idx <- as.vector(tseries::tsbootstrap(x = 1:T, nb = ceiling(B/T),
                                      type = "stationary"))
    adj.B <- ceiling(B/T) * T - B
    if (adj.B > 0) {
      boot.idx <- boot.idx[1:B]
    }
  }
  boot.factor.ret <- as.matrix(factor.ret)[boot.idx, , drop=FALSE]
  switch(resid.dist, normal = {
    sig = cov(resids, use = "p")
    #sim.resid[, i] <- rnorm(n = B, mean = 0, sd = resid.par[i, ])
    sim.resid = mvtnorm::rmvnorm(n=B, sigma = sig)
    colnames(sim.resid) = names(resids)
  }, `Cornish-Fisher` = {
    sim.resid <- matrix(0, B, N)
    colnames(sim.resid) = fund.names
    for(i in fund.names){
      sim.resid[, i] <- rCornishFisher(n = B, dp = resid.par[i,
                                                             ])
    }

  }, `skew-t` = {
    sig = cov(resids, use = "p")
    #sim.resid[, i] <- rst(n = B, dp = resid.par[i, ])
    sim.resid = mvtnorm::rmvt(n=B, sigma = sig)
    colnames(sim.resid) = names(resids)
  }, `nonparametric` = {
    data_df = as.data.frame(cbind(factor.ret, resids))
    data_mdf = mi::missing_data.frame(data_df)
    data_mi = mi::mi(data_mdf)
    rhat.results = mi::Rhats(data_mi)
    # i=1
    while(!all(abs(rhat.results - 1) < tol)){
      data_mi = mi::mi(data_mi)
      rhat.results = mi::Rhats(data_mi)
      # i = i+1
    }
    summary(data_mi)[mgr.names]
    
    data_complete = mi::complete(data_mi)
    sim.resid.list = lapply(data_complete, function(dc) as.matrix(dc[boot.idx, fund.names]))
    # rhat.results
    # i

  })
  if(resid.dist != 'nonparametric'){
    sim.fund.ret = sapply(1:nrow(alpha), FUN = function(i)rep(alpha[i, 1], B)) + (boot.factor.ret[, factor.names, drop=FALSE] %*% t(beta)) + sim.resid
  } else {
    sim.list = lapply(sim.resid.list,
                      function(r)return(sapply(1:nrow(alpha), 
                                               FUN = function(i)rep(alpha[i, 1], B)) + (boot.factor.ret[, factor.names, drop = FALSE] %*% t(beta)) + r))

    sim.fund.ret = sim.list[[1]]
    sim.resid = sim.resid.list[[1]]
    for(i in 2:length(sim.list)){
      sim.fund.ret = rbind(sim.fund.ret, sim.list[[i]])
      boot.factor.ret = rbind(boot.factor.ret, boot.factor.ret[1:B, , drop = F])
      sim.resid = rbind(sim.resid, sim.resid.list[[i]])
    }
  }

  #noAlpha = (boot.factor.ret %*% t(beta)) + sim.resid


  # sim.fund.ret <- matrix(0, B, N)
  # #sim.resid <- matrix(0, B, N)
  # colnames(sim.fund.ret) = colnames(sim.resid) = fund.names
  # for (i in fund.names) {
  #   switch(resid.dist, normal = {
  #     sig = cov(resids, use = "p")
  #     #sim.resid[, i] <- rnorm(n = B, mean = 0, sd = resid.par[i, ])
  #     sim.resid = mvtnorm::rmvnorm(n=B, sigma = sig)
  #   }, `Cornish-Fisher` = {
  #     sim.resid[, i] <- rCornishFisher(n = B, dp = resid.par[i,
  #                                                            ])
  #   }, `skew-t` = {
  #     sig = cov(resids, use = "p")
  #     #sim.resid[, i] <- rst(n = B, dp = resid.par[i, ])
  #     sim.resid = mvtnorm::rmvt(n=B, sigma = sig)
  #   })
  #   sim.fund.ret[, i] = alpha[i, 1] + boot.factor.ret %*%
  #     t(beta[i, , drop = FALSE]) + sim.resid[, i]
  # }
result <- list(sim.fund.ret = sim.fund.ret, boot.factor.ret = boot.factor.ret,
               sim.resid = sim.resid)
  return(result)
}
